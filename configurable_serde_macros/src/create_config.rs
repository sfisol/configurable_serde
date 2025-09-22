use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Ident, Token, parse::Parse, parse_macro_input};

pub fn create_config(input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(input as CreateConfigArgs);
    let macro_name = args.macro_name;
    let serde_args = args.serde_args;

    let output = quote! {
        #[macro_export]
        macro_rules! #macro_name {
            ( $( $item:item )* ) => {
                $(
                    // Can't use $crate here as it will expand to configurable_serde_macros
                    #[configurable_serde::configure_serde(
                        #serde_args
                    )]
                    $item
                )*
            };
        }
    };

    output.into()
}

struct CreateConfigArgs {
    macro_name: Ident,
    serde_args: TokenStream2,
}

// Macro entry function
impl Parse for CreateConfigArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Parse the macro name. `syn`'s default error is good enough here.
        let macro_name: Ident = input.parse()?;

        // Check for a comma and provide a custom error if it's missing.
        if !input.peek(Token![,]) {
            let msg = "expected a comma after the macro name";
            // Create an error that points to the macro name the user provided.
            return Err(syn::Error::new(macro_name.span(), msg));
        }
        // If we found a comma, consume it.
        input.parse::<Token![,]>()?;

        // Parse the rest of the tokens for the serde arguments.
        let serde_args: TokenStream2 = input.parse()?;
        if serde_args.is_empty() {
            let msg = "expected one or more configure_serde parameters after the comma";
            // Create an error that points to the end of the user's input.
            return Err(syn::Error::new(input.span(), msg));
        }

        Ok(CreateConfigArgs {
            macro_name,
            serde_args,
        })
    }
}
