use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, DeriveInput, Fields, Ident};

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

#[proc_macro_derive(SpanIgnoredEq)]
pub fn span_ignored_eq_derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let name = &ast.ident;
    let generics = &ast.generics;

    let generated = match &ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            let comparions = fields
                .named
                .iter()
                .filter_map(|field| field.ident.as_ref())
                .filter(|ident| *ident != "span")
                .map(|ident| quote! { && self.#ident.span_ignored_eq(&other.#ident) });
            quote! {
                impl #generics crate::SpanIgnoredEq for #name #generics {
                    #[must_use]
                    fn span_ignored_eq(&self, other: &Self) -> bool {
                        true #(#comparions)*
                    }
                }
            }
        }
        Data::Enum(data_enum) => {
            let variants = data_enum.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let fields = &variant.fields;
                match fields {
                    Fields::Unnamed(..) => quote! {
                        (Self::#ident(a), Self::#ident(b)) => a.span_ignored_eq(&b),
                    },
                    Fields::Unit => quote! {
                        (Self::#ident, Self::#ident) => true,
                    },
                    _ => unimplemented!("enum variant with named fields is not supported"),
                }
            });
            quote! {
                impl #generics crate::SpanIgnoredEq for #name #generics {
                    #[must_use]
                    fn span_ignored_eq(&self, other: &Self) -> bool {
                        match (self, other) {
                            #(#variants)*
                            _ => false,
                        }
                    }
                }
            }
        }
        _ => unimplemented!(),
    };

    generated.into()
}

#[proc_macro_derive(EnumAsIs)]
pub fn enum_as_is_derive(input: TokenStream) -> TokenStream {
    use heck::ToSnakeCase;

    let ast: DeriveInput = syn::parse(input).unwrap();
    let name = &ast.ident;
    let generics = &ast.generics;

    let generated = match &ast.data {
        Data::Enum(data_enum) => {
            let variants = data_enum.variants.iter().map(|variant| {
                let original_name = &variant.ident;
                let variant_name = original_name.to_string().to_snake_case();

                let ident_is = Ident::new(&format!("is_{variant_name}"), variant.ident.span());
                let ident_as = Ident::new(&format!("as_{variant_name}"), variant.ident.span());

                let doc_is = format!("Checks inner field whether it's of variant [`{original_name}`](Self::{original_name}) or not.");
                let doc_as = format!("Returns [`Some`] with a reference to inner field if it's of variant [`{original_name}`](Self::{original_name}), otherwise returns [`None`].");

                match &variant.fields {
                    Fields::Unnamed(unamed_fields) => {
                        let ty = &unamed_fields.unnamed.first().unwrap().ty;
                        quote! {
                            #[doc = #doc_is]
                            #[inline]
                            pub fn #ident_is(&self) -> bool {
                                match self {
                                    Self::#original_name(..) => true,
                                    _ => false
                                }
                            }
                            #[doc = #doc_as]
                            #[inline]
                            pub fn #ident_as(&self) -> Option<&#ty> {
                                match self {
                                    Self::#original_name(value) => Some(value),
                                    _ => None,
                                }
                            }
                        }
                    }
                    Fields::Unit => quote! {
                        #[doc = #doc_is]
                        #[inline]
                        pub fn #ident_is(&self) -> bool {
                            match self {
                                Self::#original_name => true,
                                _ => false,
                            }
                        }
                    },
                    _ => unimplemented!("enum variant with named fields is not supported"),
                }
            });

            quote! {
                impl #generics #name #generics {
                    #(#variants)*
                }
            }
        }
        _ => unimplemented!("only enum is supported"),
    };

    generated.into()
}
