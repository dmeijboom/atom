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
    let inputs = || f.sig.inputs.iter().skip(1);
    let arg_names = inputs().map(parse_arg_name).collect::<Vec<_>>();
    let args = inputs()
        .map(parse_arg)
        .enumerate()
        .map(|(i, (name, ty))| {
            syn::parse_quote! {
                let mut #name: #ty = args[#i].into();
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

        fn #export_fn(gc: &mut crate::gc::Gc, args: Vec<crate::runtime::value::Value>) -> Result<crate::runtime::value::Value, crate::runtime::error::RuntimeError> {
            #(#args)*
            let return_value = Self::#name(gc, #(#arg_names),*)?;
            return_value.try_into_val(gc)
        }
    }
    .into()
}
