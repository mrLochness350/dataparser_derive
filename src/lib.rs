extern crate proc_macro;
extern crate syn;
extern crate quote;
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, Fields, GenericArgument, PathArguments, Type};
#[proc_macro_derive(StructDeserialize)]
pub fn derive_struct_deserialize(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    impl_struct_deserialize(&ast)
}

fn impl_struct_deserialize(ast: &syn::DeriveInput) -> TokenStream {
    let struct_name = &ast.ident;

    let fields = match &ast.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields) => fields.named.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                let expr = get_parse_expr(ty);
                quote! {
                    #name: #expr
                }
            }).collect::<Vec<_>>(),
            _ => panic!("Only named fields supported"),
        },
        _ => panic!("Only structs supported"),
    };

    let generator = quote! {
        impl ::dataparser_core::Decodable for #struct_name {
            fn from_parser(parser: &mut ::dataparser_core::parser::core::DataParser) -> ::dataparser_core::utils::ParseResult<Self> {
                Ok(Self {
                    #(#fields),*
                })
            }
        }
    };

    generator.into()
}

fn get_parse_expr(ty: &Type) -> proc_macro2::TokenStream {
    match ty {
        Type::Path(type_path) => {
            let last = type_path.path.segments.last().unwrap();
            let ident = &last.ident;
            let ident_str = ident.to_string();

            match ident_str.as_str() {
                "u8" => quote! { parser.get_byte()? },
                "u16" => quote! { parser.get_u16()? },
                "u32" => quote! { parser.get_u32()? },
                "u64" => quote! { parser.get_u64()? },
                "usize" => quote! { parser.get_usize()? },
                "i8" => quote! { parser.get_raw::<i8>(true)? },
                "i16" => quote! { parser.get_i16()? },
                "i32" => quote! { parser.get_i32()? },
                "i64" => quote! { parser.get_i64()? },
                "isize" => quote! { parser.get_raw::<isize>(true)? },
                "f32" => quote! { parser.get_f32()? },
                "f64" => quote! { parser.get_f64()? },
                "bool" => quote! { parser.get_bool()? },
                "String" => quote! { parser.get_string(false)? },
                "Option" => {
                    if let PathArguments::AngleBracketed(args) = &last.arguments {
                        if let Some(GenericArgument::Type(inner)) = args.args.first() {
                            quote! { parser.get_option::<#inner>()? }
                        } else {
                            quote! { compile_error!("Invalid Option<T> type") }
                        }
                    } else {
                        quote! { compile_error!("Invalid Option syntax") }
                    }
                }
                "Vec" => {
                    if let PathArguments::AngleBracketed(args) = &last.arguments {
                        if let Some(GenericArgument::Type(inner)) = args.args.first() {
                            quote! { parser.get_vector::<#inner>()? }
                        } else {
                            quote! { compile_error!("Invalid Vec<T> type") }
                        }
                    } else {
                        quote! { compile_error!("Invalid Vec syntax") }
                    }
                }
                _ => quote! { <#ty as ::dataparser_core::Decodable>::from_parser(parser)? }
            }
        }
        _ => quote! { <#ty as ::dataparser_core::Decodable>::from_parser(parser)? },
    }
}


#[proc_macro_derive(StructSerialize)]
pub fn derive_struct_serialize(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    impl_struct_serialize(&ast)
}

fn impl_struct_serialize(ast: &syn::DeriveInput) -> TokenStream {
    let struct_name = &ast.ident;

    let field_writes = match &ast.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields) => fields.named.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                get_write_expr(ty, name)
            }).collect::<Vec<_>>(),
            _ => panic!("Only named fields supported"),
        },
        _ => panic!("Only structs supported"),
    };

    let generator = quote! {
        impl ::dataparser_core::encoder::helpers::Encodable for #struct_name {
            fn encode_data(
                &self,
                writer: &mut ::dataparser_core::encoder::core::DataEncoder
            ) -> ::dataparser_core::utils::ParseResult<()> {
                #(#field_writes)*
                Ok(())
            }
        }
    };

    generator.into()
}

fn get_write_expr(ty: &Type, field: &Option<syn::Ident>) -> proc_macro2::TokenStream {
    let field_expr = quote!(self.#field);

    match ty {
        Type::Path(type_path) => {
            let last = type_path.path.segments.last().unwrap();
            let ident_str = last.ident.to_string();

            match ident_str.as_str() {
                "String" => quote! { writer.add_string(&*#field_expr)?; },
                "bool" => quote! { writer.add_bool(#field_expr)?; },
                "Option" => {
                    if let PathArguments::AngleBracketed(args) = &last.arguments {
                        if let Some(GenericArgument::Type(_)) = args.args.first() {
                            quote! {
                                match &#field_expr {
                                    Some(val) => {
                                        writer.add_bool(true)?;
                                        val.encode_data(writer)?;
                                    }
                                    None => writer.add_bool(false)?,
                                }
                            }
                        } else {
                            quote! { compile_error!("Invalid Option syntax"); }
                        }
                    } else {
                        quote! { compile_error!("Invalid Option structure"); }
                    }
                }
                "Vec" => {
                    if let PathArguments::AngleBracketed(args) = &last.arguments {
                        if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                            quote! {
                                writer.add_slice::<#inner_ty>(&#field_expr)?;
                            }
                        } else {
                            quote! { compile_error!("Invalid Vec<T> type"); }
                        }
                    } else {
                        quote! { compile_error!("Invalid Vec syntax"); }
                    }
                }
                primitive if [
                    "u8", "u16", "u32", "u64", "usize",
                    "i8", "i16", "i32", "i64", "isize",
                    "f32", "f64"
                ].contains(&primitive) => {
                    let method = syn::Ident::new(&format!("add_{}", ident_str), last.ident.span());
                    quote! { writer.#method(#field_expr)?; }
                }
                _ => quote! { #field_expr.encode_data(writer)?; }
            }
        }
        _ => quote! { #field_expr.encode_data(writer)?; },
    }
}
