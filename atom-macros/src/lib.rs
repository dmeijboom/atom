use proc_macro::TokenStream;
use quote::quote;
use syn::{FnArg, Ident, ItemFn, Stmt};

fn parse_arg(arg: FnArg) -> (Ident, syn::Type) {
    if let FnArg::Typed(pat) = arg {
        if let syn::Pat::Ident(ident) = *pat.pat {
            return (ident.ident, *pat.ty);
        }
    }

    unimplemented!()
}

#[proc_macro_attribute]
pub fn export(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let f: ItemFn = syn::parse_macro_input!(item);
    let mut iter = f.sig.inputs.into_iter();
    let (ctx_name, ctx_ty) = parse_arg(iter.next().expect("expected context argument"));
    let args = iter
        .map(parse_arg)
        .rev()
        .enumerate()
        .map(|(i, (name, ty))| {
            syn::parse_quote! {
                let #name: #ty = args[#i].convert();
            }
        })
        .collect::<Vec<Stmt>>();

    let body = f.block.stmts;
    let return_type = match f.sig.output {
        syn::ReturnType::Type(_, ret) => ret,
        syn::ReturnType::Default => unreachable!(),
    };

    let export_name = Ident::new(&format!("atom_{}", f.sig.ident), f.sig.ident.span());

    quote! {
        use crate::runtime::Convert as _;

        pub fn #export_name(#ctx_name: #ctx_ty, args: Vec<Value>) -> Result<crate::runtime::value::Value, crate::runtime::error::RuntimeError> {
            let mut _inner = move || -> #return_type {
                #(#args)*
                #(#body)*
            };
            _inner().map(Into::into)
        }
    }
    .into()
}
