use proc_macro::TokenStream;
use quote::quote;
use syn::{Ident, ItemFn, Stmt};

fn export(attr: TokenStream, item: TokenStream, offset: usize) -> TokenStream {
    let mut f: ItemFn = syn::parse_macro_input!(item);
    let exported_name: Ident = syn::parse_macro_input!(attr);
    let mut input_args = vec![];

    for arg in f.sig.inputs.iter_mut() {
        match arg {
            syn::FnArg::Receiver(_) => {}
            syn::FnArg::Typed(pat) => match pat.pat.as_mut() {
                syn::Pat::Ident(ident) if ident.ident != "gc" => {
                    input_args.push((ident.ident.clone(), pat.ty.clone()))
                }
                _ => {}
            },
        }
    }

    let args = input_args
        .into_iter()
        .enumerate()
        .map(|(i, (name, ty))| {
            syn::parse_quote! {
                let #name: #ty = args[#i].into();
            }
        })
        .collect::<Vec<Stmt>>();

    let fn_name = f.sig.ident;
    let body = f.block.stmts;
    let arg_count = args.len() - offset;
    let tmp_name = syn::Ident::new(&format!("__atom_export_{}", fn_name), fn_name.span());

    quote! {
        fn #tmp_name(gc: &mut Gc, args: Vec<Value>) -> Result<Value, Error> {
            #(#args)*
            let mut _inner = || { #(#body)* };
            _inner().map(Into::into)
        }

        fn #fn_name() -> crate::runtime::function::Func {
            crate::runtime::function::Func::with_handler(
                stringify!(#exported_name).to_string(),
                #arg_count,
                #tmp_name,
            )
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn atom_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    export(attr, item, 0)
}

#[proc_macro_attribute]
pub fn atom_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    export(attr, item, 1)
}
