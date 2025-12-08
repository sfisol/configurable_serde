use darling::FromMeta;
use darling::ast::NestedMeta;
use proc_macro::TokenStream;
use proc_macro_error::emit_error;
use quote::quote;
use syn::{Attribute, Item, parse_macro_input};

/// Arguments passed to the `configure_serde` attribute macro.
/// Uses `darling` for easy parsing from attribute syntax.
#[derive(Debug, FromMeta)]
struct MacroArgs {
    /// A general rename rule for both structs and enums.
    /// Can be overridden by `struct_rename_all` or `enum_rename_all`.
    #[darling(default)]
    rename_all: Option<String>,

    /// A specific rename rule for struct fields.
    #[darling(default)]
    struct_rename_all: Option<String>,

    /// A specific rename rule for enum variants.
    #[darling(default)]
    enum_rename_all: Option<String>,

    /// If present, adds `#[serde(default, skip_serializing_if = "Option::is_none")]`
    /// to any field of type `Option<T>`.
    #[darling(default)]
    skip_if_none: bool,

    /// If present, adds `#[serde(deny_unknown_fields)]` to the container.
    #[darling(default)]
    deny_unknown_fields: bool,

    /// If present, adds `#[serde(with = "value")]` to any field of type `DateTime<Utc>`.
    #[darling(default)]
    date_format: Option<String>,

    /// If present, adds `#[serde(with = "value")]` to any field of type `Option<DateTime<Utc>>`.
    #[darling(default)]
    optional_date_format: Option<String>,
}

/// Macro entry function
pub fn configure_serde(args: TokenStream, input: TokenStream) -> TokenStream {
    // 1. Parse the arguments passed to the macro attribute (e.g., `rename_all = "camelCase"`)
    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(args) => args,
        Err(e) => {
            // If parsing fails, return the errors as compiler errors.
            emit_error!(e.span(), e.to_compile_error());
            return TokenStream::new();
        }
    };

    let args: MacroArgs = match MacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            // If parsing fails, return the errors as compiler errors.
            return TokenStream::from(e.write_errors());
        }
    };

    // 2. Parse the item the attribute is attached to (e.g., a `struct` or `enum`).
    let mut item = parse_macro_input!(input as Item);

    // 3. Apply the configuration based on whether it's a struct or enum.
    let result = match &mut item {
        Item::Struct(item_struct) => apply_to_struct(item_struct, &args),
        Item::Enum(item_enum) => apply_to_enum(item_enum, &args),
        _ => {
            // If the attribute is used on something other than a struct or enum, return an error.
            Err(syn::Error::new_spanned(
                &item,
                "configure_serde can only be used on structs and enums",
            ))
        }
    };

    if let Err(e) = result {
        return TokenStream::from(e.to_compile_error());
    }

    // 4. Return the modified code as a TokenStream.
    TokenStream::from(quote! { #item })
}

/// Applies serde attributes to a struct based on the parsed macro arguments.
fn apply_to_struct(s: &mut syn::ItemStruct, args: &MacroArgs) -> Result<(), syn::Error> {
    // --- Apply Container-Level Attributes ---
    let mut container_attrs = Vec::new();

    // Determine the rename rule, giving precedence to `struct_rename_all`.
    let rename_case = args.struct_rename_all.as_ref().or(args.rename_all.as_ref());
    if let Some(case) = rename_case {
        container_attrs.push(quote! { rename_all = #case });
    }

    if args.deny_unknown_fields {
        container_attrs.push(quote! { deny_unknown_fields });
    }

    // If we have any container attributes, create a new `#[serde(...)]` attribute
    // and add it to the struct's attributes.
    if !container_attrs.is_empty() {
        let new_attr: Attribute = syn::parse_quote! { #[serde(#(#container_attrs),*)] };
        s.attrs.push(new_attr);
    }

    // --- Apply Field-Level Attributes ---
    for field in s.fields.iter_mut() {
        let mut field_attrs = Vec::new();

        // Check if the `skip_if_none` flag is set.
        if args.skip_if_none {
            // Check if the field's type is `Option<T>`.
            if let syn::Type::Path(type_path) = &field.ty
                && type_path.qself.is_none() // TODO: Support qualified std::option::Option?
                && let Some(segment) = type_path.path.segments.last()
                && segment.ident == "Option"
            {
                field_attrs.push(quote! { default, skip_serializing_if = "Option::is_none" });
            }
        }

        // Apply `date_format` if the field is `DateTime<Utc>`.
        if let Some(format_module) = &args.date_format
            && let syn::Type::Path(type_path) = &field.ty
            && type_path.qself.is_none()
            && let Some(segment) = type_path.path.segments.last()
            && segment.ident == "DateTime"
        {
            // Check generic argument (any TimeZone)
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments
                && !args.args.is_empty()
            {
                field_attrs.push(quote! { with = #format_module });
            }
        }

        // Apply `optional_date_format` if the field is `Option<DateTime<Utc>>`.
        if let Some(format_module) = &args.optional_date_format
            && let syn::Type::Path(type_path) = &field.ty
            && type_path.qself.is_none()
            && let Some(segment) = type_path.path.segments.last()
            && segment.ident == "Option"
            && let syn::PathArguments::AngleBracketed(args) = &segment.arguments
            && let Some(syn::GenericArgument::Type(syn::Type::Path(inner_path))) = args.args.first()
            && let Some(inner_segment) = inner_path.path.segments.last()
            && inner_segment.ident == "DateTime"
            && let syn::PathArguments::AngleBracketed(inner_args) = &inner_segment.arguments
            && !inner_args.args.is_empty()
        {
            field_attrs.push(quote! { with = #format_module });
        }

        // If we have any field attributes, create and add them.
        if !field_attrs.is_empty() {
            let new_attr: Attribute = syn::parse_quote! { #[serde(#(#field_attrs),*)] };
            field.attrs.push(new_attr);
        }
    }
    Ok(())
}

/// Applies serde attributes to an enum based on the parsed macro arguments.
fn apply_to_enum(e: &mut syn::ItemEnum, args: &MacroArgs) -> Result<(), syn::Error> {
    // --- Apply Container-Level Attributes ---
    let mut container_attrs = Vec::new();

    // Determine the rename rule, giving precedence to `enum_rename_all`.
    let rename_case = args.enum_rename_all.as_ref().or(args.rename_all.as_ref());
    if let Some(case) = rename_case {
        container_attrs.push(quote! { rename_all = #case });
    }

    if args.deny_unknown_fields {
        container_attrs.push(quote! { deny_unknown_fields });
    }

    // If we have any container attributes, create and add them.
    if !container_attrs.is_empty() {
        let new_attr: Attribute = syn::parse_quote! { #[serde(#(#container_attrs),*)] };
        e.attrs.push(new_attr);
    }

    // TODO: Logic for variant-specific attributes could be added here if needed.
    Ok(())
}
