use proc_macro::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, token::Dot, FnArg, Ident, ItemFn, Stmt};

fn export(f: ItemFn, atom_name: Ident, ty: Option<Ident>) -> TokenStream {
    let args = f
        .sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pat) => match pat.pat.as_ref() {
                syn::Pat::Ident(ident) if ident.ident != "gc" => {
                    Some((ident.ident.clone(), pat.ty.clone()))
                }
                _ => None,
            },
        })
        .enumerate()
        .map(|(i, (name, ty))| {
            syn::parse_quote! {
                let #name: #ty = args[#i].into();
            }
        })
        .collect::<Vec<Stmt>>();

    let body = f.block.stmts;
    let arg_count = if ty.is_some() {
        args.len() - 1
    } else {
        args.len()
    };
    let fn_name = &f.sig.ident;
    let export_name = Ident::new(
        &format!("__atom_export_{}", f.sig.ident),
        f.sig.ident.span(),
    );
    let ty = ty.map(|ty| quote!{
        .with_receiver(crate::runtime::function::Receiver::Type(crate::runtime::value::Type::#ty))
    });

    quote! {
        fn #export_name(gc: &mut Gc, args: Vec<Value>) -> Result<Value, Error> {
            #(#args)*
            let mut _inner = || { #(#body)* };
            _inner().map(Into::into)
        }

        fn #fn_name() -> crate::runtime::function::Func {
            crate::runtime::function::Func::with_handler(
                stringify!(#atom_name).to_string(),
                #arg_count,
                #export_name,
            )#ty
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
