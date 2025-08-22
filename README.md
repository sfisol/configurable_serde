# Configurable Serde

A crate providing a procedural macro to apply reusable serde configurations.
The core of this crate is the `#[configurable_serde]` attribute macro, which
generates `#[serde(...)]` attributes for you.

> **NOTE:**
> While it is tested and working, this is a very early stage of the project
so it lacks many of possible configurable parameters and also may be incompatible
with more complicated structs and enums.

## Basic Usage

```rust
use configurable_serde::configurable_serde;
use serde::{Serialize, Deserialize};

#[configurable_serde(rename_all = "camelCase", skip_if_none)]
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct User {
    user_id: String,
    display_name: Option<String>,
}

// The struct will be serialized with camelCase fields, and `display_name`
// will be omitted if it is `None`.

let user = User { user_id: "u-123".to_string(), display_name: None };
let json = serde_json::to_string(&user).unwrap();

assert_eq!(json, r#"{"userId":"u-123"}"#);
```

## Reusing Configurations

To reuse a configuration across multiple structs, you can define your own
declarative macro (`macro_rules!`) that applies the `#[configurable_serde]`
attribute with your desired settings.

```rust
use configurable_serde::configurable_serde;
use serde::{Serialize, Deserialize};

/// Defines a reusable configuration named `apply_api_config`.
macro_rules! apply_api_config {
    ($item:item) => {
        #[configurable_serde(
            struct_rename_all = "camelCase",
            enum_rename_all = "SCREAMING_SNAKE_CASE",
            skip_if_none,
            deny_unknown_fields
        )]
        $item
    };
}

// Now, apply this configuration to a struct.
apply_api_config! {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    pub struct Product {
        product_id: String,
        stock_count: Option<u32>,
    }
}

// And to an enum.
apply_api_config! {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    pub enum Status {
        InStock,
        Backordered,
        Discontinued,
    }
}

// Test the struct serialization
let product = Product {
    product_id: "prod-456".to_string(),
    stock_count: Some(50)
};

let product_json = serde_json::to_string(&product).unwrap();

assert_eq!(product_json, r#"{"productId":"prod-456","stockCount":50}"#);

// Test the enum serialization
let status = Status::InStock;

let status_json = serde_json::to_string(&status).unwrap();

assert_eq!(status_json, r#""IN_STOCK""#);
```
