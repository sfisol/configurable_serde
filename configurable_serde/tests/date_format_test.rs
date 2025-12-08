use chrono::{DateTime, FixedOffset, TimeZone, Utc};
use configurable_serde::configure_serde;
use serde::{Deserialize, Serialize};

mod my_custom_date_formatter {
    use chrono::{DateTime, NaiveDateTime, TimeZone, Utc};
    use serde::{self, Deserialize, Deserializer, Serializer};

    const FORMAT: &str = "%Y-%m-%dT%H:%M:%SZ";

    pub fn serialize<S, Tz>(date: &DateTime<Tz>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        Tz: TimeZone,
        Tz::Offset: std::fmt::Display,
    {
        let s = format!("{}", date.format(FORMAT));
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<DateTime<Utc>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let dt = NaiveDateTime::parse_from_str(&s, FORMAT).map_err(serde::de::Error::custom)?;
        Ok(DateTime::<Utc>::from_naive_utc_and_offset(dt, Utc))
    }
}

mod my_warsaw_formatter {
    use chrono::{DateTime, FixedOffset, NaiveDateTime, TimeZone};
    use serde::{self, Deserialize, Deserializer, Serializer};

    const FORMAT: &str = "%Y-%m-%dT%H:%M:%S+0200";

    pub fn serialize<S>(date: &DateTime<FixedOffset>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let s = format!("{}", date.format(FORMAT));
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<DateTime<FixedOffset>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let dt = NaiveDateTime::parse_from_str(&s, FORMAT).map_err(serde::de::Error::custom)?;
        // Hardcode offset for test purposes
        let offset = FixedOffset::east_opt(2 * 3600).unwrap();
        Ok(offset.from_local_datetime(&dt).unwrap())
    }
}

mod my_custom_optional_date_formatter {
    use chrono::{DateTime, NaiveDateTime, Utc};
    use serde::{self, Deserialize, Deserializer, Serializer};

    const FORMAT: &str = "%Y-%m-%dT%H:%M:%SZ";

    pub fn serialize<S>(date: &Option<DateTime<Utc>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match date {
            Some(date) => {
                let s = format!("{}", date.format(FORMAT));
                serializer.serialize_str(&s)
            }
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<DateTime<Utc>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = Option::<String>::deserialize(deserializer)?;
        match s {
            Some(s) => {
                let dt =
                    NaiveDateTime::parse_from_str(&s, FORMAT).map_err(serde::de::Error::custom)?;
                Ok(Some(DateTime::<Utc>::from_naive_utc_and_offset(dt, Utc)))
            }
            None => Ok(None),
        }
    }
}

#[configure_serde(
    date_format = "my_custom_date_formatter",
    optional_date_format = "my_custom_optional_date_formatter"
)]
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Event {
    name: String,
    timestamp: DateTime<Utc>,
    optional_timestamp: Option<DateTime<Utc>>,
}

#[test]
fn test_date_format() {
    let event = Event {
        name: "test".to_string(),
        timestamp: Utc.with_ymd_and_hms(2023, 10, 27, 12, 0, 0).unwrap(),
        optional_timestamp: Some(Utc.with_ymd_and_hms(2023, 10, 28, 12, 0, 0).unwrap()),
    };

    let json = serde_json::to_string(&event).unwrap();
    assert_eq!(
        json,
        r#"{"name":"test","timestamp":"2023-10-27T12:00:00Z","optional_timestamp":"2023-10-28T12:00:00Z"}"#
    );

    let deserialized: Event = serde_json::from_str(&json).unwrap();
    assert_eq!(event, deserialized);
}

#[test]
fn test_optional_date_format_none() {
    let event = Event {
        name: "test".to_string(),
        timestamp: Utc.with_ymd_and_hms(2023, 10, 27, 12, 0, 0).unwrap(),
        optional_timestamp: None,
    };

    let json = serde_json::to_string(&event).unwrap();
    assert_eq!(
        json,
        r#"{"name":"test","timestamp":"2023-10-27T12:00:00Z","optional_timestamp":null}"#
    );

    let deserialized: Event = serde_json::from_str(&json).unwrap();
    assert_eq!(event, deserialized);
}

#[configure_serde(date_format = "my_warsaw_formatter")]
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct WarsawEvent {
    timestamp: DateTime<FixedOffset>,
}

#[test]
fn test_warsaw_timezone() {
    let offset = FixedOffset::east_opt(2 * 3600).unwrap(); // Warsaw is UTC+2 in summer
    let event = WarsawEvent {
        timestamp: offset.with_ymd_and_hms(2023, 10, 27, 14, 0, 0).unwrap(),
    };

    let json = serde_json::to_string(&event).unwrap();
    assert_eq!(json, r#"{"timestamp":"2023-10-27T14:00:00+0200"}"#);
}
