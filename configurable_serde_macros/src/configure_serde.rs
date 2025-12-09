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

    /// If present, adds `#[serde(with = "value")]` to any field of type `T`.
    #[darling(skip)]
    format_type: Vec<FormatTypeRule>,
}

#[derive(Debug)]
struct FormatTypeRule {
    ty: syn::Type,
    formatter: String,
}

impl FromMeta for FormatTypeRule {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        if items.len() != 2 {
            return Err(darling::Error::custom(
                "format_type expects exactly two arguments: (Type, \"formatter_path\")",
            ));
        }

        let ty = match &items[0] {
            NestedMeta::Meta(syn::Meta::Path(path)) => syn::Type::Path(syn::TypePath {
                qself: None,
                path: path.clone(),
            }),
            NestedMeta::Lit(syn::Lit::Str(s)) => s.parse().map_err(darling::Error::custom)?,
            _ => {
                // Try to parse as Type from the token stream of the item
                // This is a bit tricky with darling's NestedMeta.
                // Let's try to assume it's a Type if it's not a simple path.
                // Actually, darling parses attributes.
                // If the user writes `format_type(Option<Decimal>, "...")`, `Option<Decimal>` is likely parsed as a Meta::Path if it was simple, but with generics it might be different.
                // `darling` uses `syn::Meta`. `syn::Meta` can be `Path`, `List`, `NameValue`.
                // `Option<Decimal>` is not a valid `Meta` on its own in standard attribute syntax unless it's a path.
                // But `Option<Decimal>` *is* a path (with arguments).
                // So `syn::Meta::Path` should cover it.
                if let NestedMeta::Meta(syn::Meta::Path(path)) = &items[0] {
                    syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: path.clone(),
                    })
                } else {
                    return Err(darling::Error::custom(
                        "Expected a type as the first argument",
                    ));
                }
            }
        };

        // Wait, `syn::Meta` doesn't support arbitrary types in the position of "Path".
        // `syn::Meta::Path` is `Path`. `Option<Decimal>` is a `Path`.
        // So `NestedMeta::Meta(syn::Meta::Path(path))` is correct for `Option<Decimal>`.

        let formatter = match &items[1] {
            NestedMeta::Lit(syn::Lit::Str(s)) => s.value(),
            _ => {
                return Err(darling::Error::custom(
                    "Expected a string literal as the second argument",
                ));
            }
        };

        Ok(FormatTypeRule { ty, formatter })
    }
}

use syn::parse::Parse;

/// Macro entry function
pub fn configure_serde(args: TokenStream, input: TokenStream) -> TokenStream {
    // 1. Parse the arguments manually to handle `format_type` with generics.
    let mut format_types = Vec::new();
    let mut other_metas = Vec::new();

    let args: proc_macro2::TokenStream = args.into();
    let mut args_iter = args.into_iter().peekable();
    while args_iter.peek().is_some() {
        // Collect tokens for one argument
        let mut arg_tokens = proc_macro2::TokenStream::new();
        let mut nesting = 0;

        for token in args_iter.by_ref() {
            match &token {
                proc_macro2::TokenTree::Punct(p) if p.as_char() == ',' && nesting == 0 => {
                    break; // End of current argument
                }
                proc_macro2::TokenTree::Group(g) => {
                    // Groups are single tokens in the stream, so they don't affect nesting of the outer stream.
                    // The commas inside the group are hidden within the group token.
                    arg_tokens.extend(std::iter::once(proc_macro2::TokenTree::Group(g.clone())));
                }
                proc_macro2::TokenTree::Punct(p) => {
                    if p.as_char() == '<'
                        || p.as_char() == '('
                        || p.as_char() == '['
                        || p.as_char() == '{'
                    {
                        nesting += 1;
                    } else if p.as_char() == '>'
                        || p.as_char() == ')'
                        || p.as_char() == ']'
                        || p.as_char() == '}'
                    {
                        nesting -= 1;
                    }
                    arg_tokens.extend(std::iter::once(proc_macro2::TokenTree::Punct(p.clone())));
                }
                _ => {
                    arg_tokens.extend(std::iter::once(token.clone()));
                }
            }
        }

        if arg_tokens.is_empty() {
            continue;
        }

        // Now check if this argument is `format_type(...)`
        // We can try to parse it as our custom rule.
        // If it fails, we assume it's a standard Meta.

        // To check if it starts with `format_type`, we can peek the first token.
        let arg_tokens_clone = arg_tokens.clone();
        let mut arg_iter = arg_tokens_clone.into_iter();
        if let Some(proc_macro2::TokenTree::Ident(ident)) = arg_iter.next()
            && ident == "format_type"
        {
            // It might be format_type. Check if next is Group(Parenthesis).
            if let Some(proc_macro2::TokenTree::Group(group)) = arg_iter.next()
                && group.delimiter() == proc_macro2::Delimiter::Parenthesis
            {
                // Yes, it looks like format_type(...).
                // Parse the inner content.
                let inner_stream = group.stream();
                // Parse (Type, "String") from inner_stream
                let parse_rule = |input: syn::parse::ParseStream| {
                    let ty: syn::Type = input.parse()?;
                    input.parse::<syn::Token![,]>()?;
                    let lit: syn::LitStr = input.parse()?;
                    Ok(FormatTypeRule {
                        ty,
                        formatter: lit.value(),
                    })
                };

                match syn::parse::Parser::parse2(parse_rule, inner_stream) {
                    Ok(rule) => {
                        format_types.push(rule);
                        continue;
                    }
                    Err(e) => {
                        // If it failed to parse as our rule, maybe it's something else?
                        // But if it started with `format_type(...)`, it should be our rule.
                        emit_error!(ident.span(), "Invalid format_type syntax: {}", e);
                        return TokenStream::new();
                    }
                }
            }
        }

        // If not format_type, parse as NestedMeta
        // Try Meta first
        if let Ok(meta) = syn::parse::Parser::parse2(syn::Meta::parse, arg_tokens.clone()) {
            other_metas.push(NestedMeta::Meta(meta));
        } else if let Ok(lit) = syn::parse::Parser::parse2(syn::Lit::parse, arg_tokens.clone()) {
            other_metas.push(NestedMeta::Lit(lit));
        } else {
            // We can't easily get a span from TokenStream without iterating.
            // Just emit error on the macro call site or use the first token's span if available.
            // But we don't have easy access to it here without re-parsing.
            // Let's just emit a generic error.
            // Or better, try to parse as Meta again to get the error.
            if let Err(e) = syn::parse::Parser::parse2(syn::Meta::parse, arg_tokens) {
                emit_error!(e.span(), "Failed to parse attribute argument: {}", e);
                return TokenStream::new();
            }
        }
    }

    let mut args: MacroArgs = match MacroArgs::from_list(&other_metas) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    args.format_type = format_types;

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

        // Apply `format_type` rules
        for rule in &args.format_type {
            // Compare types by their string representation
            // This is a heuristic but works for most cases where the user writes the type exactly as it appears in the struct
            let field_ty = &field.ty;
            let rule_ty = &rule.ty;
            let field_ty_str = quote! { #field_ty }.to_string().replace(" ", "");
            let rule_ty_str = quote! { #rule_ty }.to_string().replace(" ", "");

            // We also need to handle the case where the field type might be fully qualified or not,
            // but for now exact match (ignoring spaces) is a good start.
            // A more robust way would be to check if the path segments match.

            if field_ty_str == rule_ty_str {
                let format_module = &rule.formatter;
                field_attrs.push(quote! { with = #format_module });
            }
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
