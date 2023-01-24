extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;

fn generate_parse_fn(enum_ident: &Ident, variant: syn::Variant) -> (Ident, TokenStream2) {
    let variant_ident = variant.ident;
    let fn_ident = Ident::new(
        &format!("try_parse_{}", variant_ident.to_string().to_lowercase()),
        Span::call_site(),
    );
    let mut var_idents = Vec::<Ident>::new();

    let mut get_fields = Vec::new();
    let add_consume_comma = |token_streams: &mut Vec<TokenStream2>| {
        token_streams.push(quote! {
            match iter.next() {
                Some((_, Token::Comma())) => (),
                _ => return (Some(tokens), None),
            }
        })
    };

    let mut start = true;
    let mut i_count = 0;
    let mut s_count = 0;
    let mut n_count = 0;

    for field in variant.fields {
        if !start {
            add_consume_comma(&mut get_fields);
        }
        if let syn::Type::Path(syn::TypePath {
            qself: _,
            path:
                syn::Path {
                    leading_colon: _,
                    segments,
                },
        }) = field.ty
        {
            if segments.len() == 1 && segments[0].ident == "i64" {
                let name = Ident::new(&format!("n{}", n_count), Span::call_site());
                get_fields.push(quote! {
                    let #name = match iter.next() {
                        Some((_, Token::Number(n))) => *n,
                        _ => return (Some(tokens), None),
                    };
                });
                var_idents.push(name);
                n_count += 1;
            }
            if segments.len() == 1 && segments[0].ident == "Ident" {
                let name = Ident::new(&format!("i{}", i_count), Span::call_site());
                get_fields.push(quote! {
                    let #name = match iter.next() {
                        Some((_, Token::Identity(s))) => Ident(*s),
                        _ => return (Some(tokens), None),
                    };
                });
                var_idents.push(name);
                i_count += 1;
            }
        } else if let syn::Type::Reference(syn::TypeReference {
            and_token: _,
            lifetime: _,
            mutability: _,
            elem,
        }) = field.ty
        {
            if let syn::Type::Path(syn::TypePath {
                qself: _,
                path:
                    syn::Path {
                        leading_colon: _,
                        segments,
                    },
            }) = *elem
            {
                if segments.len() == 1 && segments[0].ident == "str" {
                    let name = Ident::new(&format!("s{}", s_count), Span::call_site());
                    get_fields.push(quote! {
                        let #name = match iter.next() {
                            Some((_, Token::String(s))) => *s,
                            _ => return (Some(tokens), None),
                        };
                    });
                    var_idents.push(name);
                    s_count += 1;
                }
            }
        }
        start = false;
    }

    let variant_name = variant_ident.to_string();

    let f = quote! {
        fn #fn_ident<'s, 't>(tokens: &'t [Token<'s>]) -> (Option<&'t [Token<'s>]>, Option<#enum_ident<'s>>) {
            let mut iter = tokens.iter().enumerate();
            if let Some((_, Token::Identity(s))) = iter.next() {
                if !#variant_name.starts_with(s) { return (Some(tokens), None); }
            } else {
                return (Some(tokens), None);
            };
            #(#get_fields)*
            (
                match iter.next() {
                    Some((index, _)) => tokens.get(index..),
                    _ => None,
                },
                Some(#enum_ident::#variant_ident(#(#var_idents),*)),
            )
        }
    };
    (fn_ident, f)
}

#[proc_macro_derive(Parse)]
pub fn derive_parse(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();
    let parser_ident = syn::Ident::new(
        &format!("try_parse_{}", ast.ident.to_string().to_lowercase()),
        Span::call_site(),
    );
    let e = if let syn::Data::Enum(e) = ast.data {
        e
    } else {
        panic!()
    };
    let mut functions = Vec::new();
    let mut function_idents = Vec::new();
    for variant in e.variants {
        let ret = generate_parse_fn(&ast.ident, variant);
        functions.push(ret.1);
        function_idents.push(ret.0);
    }
    let enum_ident = &ast.ident;
    let ret = quote! {
        #(#functions)*
        fn #parser_ident<'s, 't>( mut tokens: &'t [Token<'s>]) -> (Option<&'t [Token<'s>]>, Vec<#enum_ident<'s>>){
            let matches = [#(#function_idents),*];
            let mut objs = Vec::<_>::new();
            loop {
                let mut matched = false;
                for f in matches {
                    match f(tokens) {
                        (None, Some(objs_res)) => {
                            objs.push(objs_res);
                            return (None, objs);
                        }
                        (Some(left), Some(objs_res)) => {
                            objs.push(objs_res);
                            matched = true;
                            tokens = left;
                        }
                        _ => {}
                    }
                }
                if !matched { break; }
            }
            (Some(tokens), objs)
        }
    };
    use std::io::Write;
    let mut f = std::fs::File::create("tmp.rs").unwrap();
    f.write_all(ret.to_string().as_bytes()).unwrap();
    std::process::Command::new("rustfmt")
        .arg("tmp.rs")
        .output()
        .expect("fmt failed!\n");
    ret.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
