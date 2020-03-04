extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenTree};
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Field,
    Fields, FieldsNamed, GenericArgument, Path, PathArguments, Type, TypePath,
};

fn get_type_name(ty: &Type) -> Option<String> {
    if let Type::Path(TypePath { ref path, .. }) = ty {
        if let std::option::Option::Some(path_segment) = path.segments.first() {
            return std::option::Option::Some(path_segment.ident.to_string());
        }
    }
    std::option::Option::None
}

fn get_option_inner(field: &Field) -> Option<&Type> {
    if let std::option::Option::Some(ty) = get_type_name(&field.ty) {
        if ty == "Option" {
            return get_inner_ty(&field.ty);
        }
    }
    std::option::Option::None
}

fn get_inner_ty(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath {
        path: Path { ref segments, .. },
        ..
    }) = ty
    {
        if let std::option::Option::Some(syn::PathSegment {
            arguments:
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }),
            ..
        }) = segments.last()
        {
            if let std::option::Option::Some(GenericArgument::Type(ref ty)) = args.first() {
                return std::option::Option::Some(ty);
            }
        }
    }
    std::option::Option::None
}

fn builder_attr(attrs: &[syn::Attribute]) -> Result<Option<String>, syn::Error> {
    if attrs.len() > 0 {
        if let std::option::Option::Some(TokenTree::Group(g)) =
            attrs.first().unwrap().tokens.clone().into_iter().next()
        {
            let mut stream = g.stream().into_iter();
            match stream.next().unwrap().to_string() {
                s if s != "each" => {
                    let attr = attrs.first().unwrap();
                    return Err(syn::Error::new(
                        attr.path.span().join(attr.tokens.span()).unwrap(),
                        "expected `builder(each = \"...\")`",
                    ));
                }
                _ => {}
            }
            assert_eq!(stream.next().unwrap().to_string(), "=");
            let arg = match stream.next().unwrap() {
                TokenTree::Literal(lit) => lit,
                tt => {
                    return Err(syn::Error::new(
                        tt.span(),
                        format!("Expected str, found {:?}", tt),
                    ));
                }
            };
            let arg_name = match syn::Lit::new(arg) {
                syn::Lit::Str(lit_str) => lit_str.value(),
                tt => {
                    return Err(syn::Error::new(
                        Span::call_site(),
                        format!("Expected str, found {:?}", tt),
                    ));
                }
            };
            return Ok(std::option::Option::Some(arg_name));
        }
    }
    Ok(std::option::Option::None)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    // span specifies the block of bytes from the source file to refer to in case of an error.
    let builder_ident = Ident::new(&builder_name, Span::call_site());

    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!("Derive only supports structs");
    };

    let optionized = fields.iter().map(|f| {
        let ty = &f.ty;
        let name = &f.ident;
        if let std::option::Option::Some(_) = get_option_inner(f) {
            quote! {#f}
        } else if let Ok(std::option::Option::Some(_)) = builder_attr(&f.attrs) {
            quote! {#name: #ty}
        } else {
            quote! {
                #name: std::option::Option<#ty>
            }
        }
    });

    let nones = fields.iter().map(|f| {
        let name = &f.ident;
        match builder_attr(&f.attrs) {
            Ok(std::option::Option::Some(_)) => {
                quote! {
                    #name: Vec::new()
                }
            }
            Ok(std::option::Option::None) => {
                quote! {
                    #name: std::option::Option::None
                }
            }

            Err(_) => quote! {#name: std::option::Option::None}, //msg.to_compile_error(),
        }
    });

    let functions = fields.iter().map(|f| {
        let ty = match get_option_inner(f) {
            std::option::Option::Some(ty) => ty,
            std::option::Option::None => &f.ty,
        };
        let name = &f.ident;
        let attrs = &f.attrs;
        match builder_attr(attrs) {
            Ok(std::option::Option::Some(arg_name)) => {
                let arg = Ident::new(&arg_name, Span::call_site());
                let ty = get_inner_ty(ty);
                return quote! {
                    fn #arg(&mut self, #arg: #ty) -> &mut Self {
                        self.#name.push(#arg);
                        self
                    }
                };
            }
            Ok(std::option::Option::None) => {
                quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }
                }
            }
            Err(msg) => msg.to_compile_error(),
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if let std::option::Option::Some(_) = get_option_inner(f) {
            quote! {
                #name: self.#name.clone()
            }
        } else if let Ok(std::option::Option::Some(_)) = builder_attr(&f.attrs) {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set!"))?
            }
        }
    });

    let res = quote! {
        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#nones, )*
                }

            }
        }

        pub struct #builder_ident {
            #(#optionized, )*
        }

        impl #builder_ident {

            #(#functions)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields, )*
                })
            }
        }
    };
    res.into()
}
