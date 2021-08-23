use proc_macro::TokenStream;

use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{FnArg, GenericArgument, Item, Pat, PathArguments, ReturnType, Type};

#[derive(Debug, PartialEq)]
enum TypeKind {
    Unknown,
    Array,
    Slice,
    Tuple,
    Ref,
    MutRef,
    Name(String),
    Generic(String),
}

#[derive(Debug)]
struct TypeInfo {
    kind: TypeKind,
    elem_type: Option<Box<TypeInfo>>,
}

impl TypeInfo {
    fn new(kind: TypeKind) -> Self {
        Self {
            kind,
            elem_type: None,
        }
    }

    fn with_elem(kind: TypeKind, elem_type: TypeInfo) -> Self {
        Self {
            kind,
            elem_type: Some(elem_type.into()),
        }
    }

    fn unknown() -> Self {
        Self::new(TypeKind::Unknown)
    }

    fn is_result(&self) -> bool {
        self.kind == TypeKind::Generic("Result".to_string())
    }
}

impl ToString for TypeInfo {
    fn to_string(&self) -> String {
        let elem = self
            .elem_type
            .as_ref()
            .map(|elem_type| elem_type.to_string())
            .unwrap_or_default();

        match &self.kind {
            TypeKind::Unknown => "unknown".to_string(),
            TypeKind::Array => format!("[{}]", elem),
            TypeKind::Slice => format!("&[{}]", elem),
            TypeKind::Tuple => format!("({})", elem),
            TypeKind::Ref => format!("&{}", elem),
            TypeKind::MutRef => format!("&mut {}", elem),
            TypeKind::Name(name) => format!("{}", name),
            TypeKind::Generic(name) => format!("{}<{}>", name, elem),
        }
    }
}

fn parse_type_info(typedef: &Type) -> TypeInfo {
    match typedef {
        Type::Array(array_type) => {
            TypeInfo::with_elem(TypeKind::Array, parse_type_info(&array_type.elem))
        }
        Type::Paren(inner_type) => parse_type_info(&inner_type.elem),
        Type::Path(type_path) => {
            if type_path.path.segments.len() != 1 {
                return TypeInfo::unknown();
            }

            let segment = &type_path.path.segments[0];

            if segment.arguments.is_empty() {
                return TypeInfo::new(TypeKind::Name(segment.ident.to_string()));
            }

            if let PathArguments::AngleBracketed(path_args) = &segment.arguments {
                if let GenericArgument::Type(type_arg) = &path_args.args[0] {
                    return TypeInfo::with_elem(
                        TypeKind::Generic(segment.ident.to_string()),
                        parse_type_info(&type_arg),
                    );
                }
            }

            TypeInfo::unknown()
        }
        Type::Reference(type_reference) => TypeInfo::with_elem(
            if type_reference.mutability.is_some() {
                TypeKind::MutRef
            } else {
                TypeKind::Ref
            },
            parse_type_info(&type_reference.elem),
        ),
        Type::Slice(type_slice) => {
            TypeInfo::with_elem(TypeKind::Slice, parse_type_info(&type_slice.elem))
        }
        Type::Tuple(tuple) => {
            if tuple.elems.len() == 0 {
                return TypeInfo::new(TypeKind::Tuple);
            }

            TypeInfo::unknown()
        }
        Type::Group(type_group) => {
            panic!("GROUP={:?}", type_group);
        }
        _ => TypeInfo::unknown(),
    }
}

#[proc_macro_attribute]
pub fn export(_: TokenStream, input: TokenStream) -> TokenStream {
    let item = syn::parse(input).expect("syntax error");

    match item {
        Item::Fn(func) => {
            let name = &func.sig.ident;
            let args = &func.sig.inputs;
            let output = &func.sig.output;
            let statements = &func.block.stmts;

            let mut arg_len: usize = 0;
            let mut parse_args = vec![];
            let mut arg_names = vec![];

            for input in func.sig.inputs.iter() {
                match input {
                    FnArg::Receiver(_) => {
                        parse_args.push(quote! { compile_error!("Rust methods as atom methods are not supported yet"); });
                    }
                    FnArg::Typed(pat_type) => {
                        let ty: &Type = &*pat_type.ty;
                        let type_info = parse_type_info(ty);
                        let type_name = type_info.to_string();
                        let arg_name = match pat_type.pat.as_ref() {
                            Pat::Ident(ident) => { ident.ident.clone() }
                            _ => {
                                parse_args.push(quote! { compile_error!("invalid pattern for function argument"); });
                                continue;
                            }
                        };

                        arg_names.push(arg_name.clone());

                        // We don't support methods (yet?) so using 'this' as the name of an argument results in binding the receiver
                        let parse_arg = if arg_name.to_string() == "this" {
                            if type_info.kind == TypeKind::MutRef {
                                quote! { let this = vm.get_fn_self_mut()?.try_into()?; }
                            } else {
                                quote! { let this = vm.get_fn_self()?.try_into()?; }
                            }
                        } else {
                            arg_len += 1;

                            // No type conversion needed for 'Value' as it's already an atom value
                            if type_info.kind == TypeKind::Name("Value".to_string()) {
                                quote! {
                                   let #arg_name = __values.remove(0);
                                }
                            } else {
                                quote! {
                                   let #arg_name = __values.remove(0).try_into().map_err(|_| atom_runtime::RuntimeError::new(format!(
                                        "invalid type '{}' for argument '{}' of: {}",
                                        #type_name,
                                        stringify!(#arg_name),
                                        stringify!(#name),
                                    )))?;
                                }
                            }
                        };

                        parse_args.push(parse_arg);
                    }
                }
            }

            let is_void = if let ReturnType::Type(_, type_val) = output {
                let return_type = parse_type_info(type_val);

                return_type.is_result() && return_type.elem_type
                    .map(|elem_type| elem_type.kind)
                    .map(|kind| kind == TypeKind::Tuple)
                    .unwrap_or(false)
            } else {
                false
            };

            let inner = Ident::new(format!("__{}", name).as_str(), Span::call_site());
            let return_expr = if is_void {
                quote! {
                    #inner(#(#arg_names,)*)?;

                    Ok(None)
                }
            } else {
                quote! { Ok(Some(#inner(#(#arg_names,)*)?.into())) }
            };

            quote! {
                fn #inner(#args) #output {
                    #(#statements)*
                }

                fn #name(vm: &mut crate::vm::VM, mut __values: Vec<atom_runtime::Value>) -> atom_runtime::Result<Option<atom_runtime::Value>> {
                    use std::convert::TryInto;

                    if __values.len() != #arg_len {
                        return Err(atom_runtime::RuntimeError::new(format!(
                            "invalid argument count for target: {}(...) (expected {}, not {})",
                            stringify!(#name),
                            #arg_len,
                            __values.len(),
                        )));
                    }

                    #(#parse_args)*
                    #return_expr
                }
            }
        }
        _ => quote! { compile_error!("invalid token type for '#[export]' macro") },
    }.into()
}
