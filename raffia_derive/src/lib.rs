use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields};

#[proc_macro_derive(Spanned)]
pub fn spanned_derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let name = &ast.ident;
    let generics = &ast.generics;

    let generated = match &ast.data {
        Data::Struct(..) => quote! {
            impl #generics crate::pos::Spanned for #name #generics {
                #[inline]
                fn span(&self) -> &Span {
                    &self.span
                }
            }
        },
        Data::Enum(data_enum) => {
            let variants = data_enum.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let fields = &variant.fields;
                match fields {
                    Fields::Unnamed(..) => quote! {
                        Self::#ident(x) => x.span(),
                    },
                    Fields::Unit => quote! {
                        Self::#ident => unimplemented!(),
                    },
                    _ => unimplemented!("enum variant with named fields is not supported"),
                }
            });
            quote! {
                impl #generics crate::pos::Spanned for #name #generics {
                    fn span(&self) -> &Span {
                        match self {
                            #(#variants)*
                        }
                    }
                }
            }
        }
        _ => unimplemented!(),
    };

    generated.into()
}
