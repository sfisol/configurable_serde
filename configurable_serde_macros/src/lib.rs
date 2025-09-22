use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;

mod configure_serde;
mod create_config;

#[proc_macro_attribute]
#[proc_macro_error]
pub fn configure_serde(args: TokenStream, input: TokenStream) -> TokenStream {
    configure_serde::configure_serde(args, input)
}

#[proc_macro]
pub fn create_config(input: TokenStream) -> TokenStream {
    create_config::create_config(input)
}
