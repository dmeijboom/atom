use proc_macro::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, token::Dot, FnArg, Ident, ItemFn, Stmt};

fn parse_arg(arg: FnArg) -> (Ident, syn::Type) {
    if let FnArg::Typed(pat) = arg {
        if let syn::Pat::Ident(ident) = *pat.pat {
            return (ident.ident, *pat.ty);
        }
    }

    unimplemented!()
}

fn export(item_fn: ItemFn, atom_name: Ident, ty: Option<Ident>) -> TokenStream {
    let mut iter = item_fn.sig.inputs.into_iter();
    let (ctx_name, ctx_ty) = parse_arg(iter.next().expect("expected context argument"));
    let mut args = iter
        .map(parse_arg)
        .enumerate()
        .map(|(i, (name, ty))| {
            syn::parse_quote! {
                let #name: #ty = args[#i].convert(#ctx_name.gc);
            }
        })
        .collect::<Vec<Stmt>>();

    args.reverse();

    let fn_name = &item_fn.sig.ident;
    let body = item_fn.block.stmts;
    let arg_count = args.len() - ty.as_ref().map(|_| 1).unwrap_or(0);
    let export_name = Ident::new(
        &format!("__atom_export_{}", item_fn.sig.ident),
        item_fn.sig.ident.span(),
    );
    let receiver = ty.map(|ty| quote!{
        .with_receiver(crate::runtime::function::Receiver::Type(crate::runtime::value::Type::#ty))
    });

    quote! {
        use crate::runtime::std::Convert as _;

        fn #export_name(#ctx_name: #ctx_ty, args: Vec<Value>) -> Result<crate::runtime::value::Value, crate::runtime::error::RuntimeError> {
            let mut _inner = move || {
                #(#args)*
                #(#body)*
            };
            _inner().map(Into::into)
        }

        fn #fn_name() -> crate::runtime::function::Func {
            crate::runtime::function::Func::with_handler(
                stringify!(#atom_name).to_string(),
                #arg_count,
                #export_name,
            )#receiver
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn atom_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    let f: ItemFn = syn::parse_macro_input!(item);
    let atom_name: Ident = syn::parse_macro_input!(attr);

    export(f, atom_name, None)
}

#[proc_macro_attribute]
pub fn atom_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let f: ItemFn = syn::parse_macro_input!(item);
    let mut args: <Punctuated<Ident, Dot> as IntoIterator>::IntoIter =
        syn::parse_macro_input!(attr with Punctuated<Ident, Dot>::parse_terminated).into_iter();

    let ty = args.next().expect("expecting atom type");
    let atom_name = args.next().expect("expected function name");

    export(f, atom_name, Some(ty))
}
