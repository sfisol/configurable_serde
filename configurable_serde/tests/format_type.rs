use configurable_serde::configure_serde;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Debug, PartialEq)]
struct MyType(i32);

mod my_formatter {
    use super::*;

    pub fn serialize<S>(val: &MyType, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("MyType({})", val.0))
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<MyType, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let val = s
            .trim_start_matches("MyType(")
            .trim_end_matches(")")
            .parse()
            .map_err(serde::de::Error::custom)?;
        Ok(MyType(val))
    }
}

mod double_formatter {
    use super::*;

    pub fn serialize<S>(val: &i32, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("{}", val * 2))
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<i32, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let val: i32 = s.parse().map_err(serde::de::Error::custom)?;
        Ok(val / 2)
    }
}

#[configure_serde(
    format_type(MyType, "my_formatter"),
    format_type(i32, "double_formatter")
)]
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct TestStruct {
    val: MyType,
    val2: i32,
}

#[test]
fn test_format_type() {
    let t = TestStruct {
        val: MyType(42),
        val2: 42,
    };
    let json = serde_json::to_string(&t).unwrap();
    assert_eq!(json, r#"{"val":"MyType(42)","val2":"84"}"#);

    let t2: TestStruct = serde_json::from_str(&json).unwrap();
    assert_eq!(t, t2);
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Wrapper<T>(T);

#[configure_serde(
    format_type(Wrapper<i32>, "my_formatter_wrapper"),
    format_type(Wrapper<i64>, "my_formatter_wrapper")
)]
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct TestStructGeneric {
    val: Wrapper<i32>,
    val2: Wrapper<i64>,
}

mod my_formatter_wrapper {
    use super::*;
    use std::fmt::Display;
    use std::str::FromStr;

    pub fn serialize<S, T>(val: &Wrapper<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Display,
    {
        serializer.serialize_str(&format!("Wrapper({})", val.0))
    }

    pub fn deserialize<'de, D, T>(deserializer: D) -> Result<Wrapper<T>, D::Error>
    where
        D: Deserializer<'de>,
        T: FromStr,
        T::Err: Display,
    {
        let s = String::deserialize(deserializer)?;
        let val = s
            .trim_start_matches("Wrapper(")
            .trim_end_matches(")")
            .parse()
            .map_err(serde::de::Error::custom)?;
        Ok(Wrapper(val))
    }
}

#[test]
fn test_format_type_generic() {
    let t = TestStructGeneric {
        val: Wrapper(123),
        val2: Wrapper(456),
    };
    let json = serde_json::to_string(&t).unwrap();
    assert_eq!(json, r#"{"val":"Wrapper(123)","val2":"Wrapper(456)"}"#);

    let t2: TestStructGeneric = serde_json::from_str(&json).unwrap();
    assert_eq!(t, t2);
}
