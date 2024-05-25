use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields, GenericArgument, Path,
    PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let (fields, def_fields, methods, fields_build) = gen_builder_f(&input.data);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let builder = quote! {
    pub struct #builder_name{
        #fields
    }
    };
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let expanded = quote! {
    #builder
    impl #impl_generics #name #ty_generics #where_clause{
        fn builder()->#builder_name{
        #builder_name{
        #def_fields
        }
        }
    }
    impl #impl_generics #builder_name #ty_generics #where_clause{
    fn build(&mut self)-> ::std::result::Result<#name,Box<dyn ::std::error::Error>>{
    Ok(#name {
    #fields_build
    })
    }
        #methods
    }
    };
    let tokens = TokenStream::from(expanded);
    eprintln!("TOKENS: {}", tokens);
    tokens
}

fn gen_builder_f(
    data: &Data,
) -> (
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
) {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let field_declarations = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if is_option(ty).is_some() {
                        quote_spanned! {f.span()=> #name: #ty }
                    } else {
                        quote_spanned! {f.span()=> #name: ::std::option::Option<#ty> }
                    }
                });
                let field_defaults = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! {f.span()=>#name: None }
                });
                let field_methods = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if let Some(oty) = is_option(ty) {
                        quote_spanned! {f.span()=>
                        pub fn #name(&mut self, #name:#oty)->&mut Self{
                            self.#name=Some(#name);
                            self
                        }
                        }
                    } else {
                        quote_spanned! {f.span()=>
                        pub fn #name(&mut self, #name:#ty)->&mut Self{
                            self.#name=Some(#name);
                            self
                        }
                        }
                    }
                });
                let field_builds = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    if is_option(&f.ty).is_some() {
                        quote_spanned! {f.span()=>
                        #name:self.#name.take()
                        }
                    } else {
                        let msg = format!("{} is required", name.clone().unwrap());
                        quote_spanned! {f.span()=>
                        #name:self.#name.take().ok_or(#msg)?
                        }
                    }
                });
                (
                    quote! {#(#field_declarations ,)*},
                    quote! {#(#field_defaults ,)*},
                    quote! {#(#field_methods )*},
                    quote! {#(#field_builds ,)*},
                )
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn is_option(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(TypePath {
            qself: None,
            path: Path {
                leading_colon: _,
                segments,
            },
        }) => match segments.iter().find(|s| s.ident == "Option") {
            Some(seg) => match &seg.arguments {
                PathArguments::AngleBracketed(args) => {
                    assert!(args.args.len() == 1);
                    match &args.args[0] {
                        GenericArgument::Type(ty) => Some(ty.clone()),
                        _ => unimplemented!(),
                    }
                }
                PathArguments::None | PathArguments::Parenthesized(_) => unimplemented!(),
            },
            None => None,
        },
        _ => None,
    }
}
