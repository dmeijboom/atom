use proc_macro::TokenStream;
use quote::quote;
use syn::{FnArg, Ident, ItemFn, LitStr, Stmt};

fn parse_arg_name(arg: &FnArg) -> Ident {
    if let FnArg::Typed(pat) = arg {
        if let syn::Pat::Ident(ident) = pat.pat.as_ref() {
            return ident.ident.clone();
        }
    }

    unimplemented!()
}

fn parse_arg(arg: &FnArg) -> (Ident, syn::Type) {
    if let FnArg::Typed(pat) = arg {
        if let syn::Pat::Ident(ident) = pat.pat.as_ref() {
            return (ident.ident.clone(), *pat.ty.clone());
        }
    }

    unimplemented!()
}

#[proc_macro_attribute]
pub fn atom_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    let export_as: LitStr = syn::parse_macro_input!(attr);
    let f: ItemFn = syn::parse_macro_input!(item);
    let arg_names = f.sig.inputs.iter().map(parse_arg_name).collect::<Vec<_>>();
    let args = f
        .sig
        .inputs
        .iter()
        .rev()
        .map(parse_arg)
        .map(|(name, ty)| {
            syn::parse_quote! {
                let mut #name: #ty = args.pop().unwrap_or(crate::runtime::value::Value::default()).into();
            }
        })
        .collect::<Vec<Stmt>>();

    let name = f.sig.ident.clone();
    let export_fn = Ident::new(&format!("_atom_{}", f.sig.ident), f.sig.ident.span());
    let export_name = Ident::new(&format!("_atom_{}_name", f.sig.ident), f.sig.ident.span());

    quote! {
        #[allow(non_upper_case_globals)]
        const #export_name: &str = #export_as;

        #f

        fn #export_fn<'gc>(gc: &mut crate::gc::Gc<'gc>, mut args: Vec<crate::runtime::value::Value<'gc>>) -> Result<crate::runtime::value::Value<'gc>, crate::runtime::error::RuntimeError> {
            #(#args)*
            let return_value = Self::#name(#(#arg_names),*)?;
            return_value.into_atom(gc)
        }
    }
    .into()
}
