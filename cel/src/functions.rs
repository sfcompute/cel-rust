use crate::context::Context;
use crate::magic::{Arguments, This};
use crate::objects::{OptionalValue, Value};
use crate::parser::Expression;
use crate::resolvers::Resolver;
use crate::ExecutionError;
use std::cmp::Ordering;
use std::convert::TryInto;
use std::sync::Arc;

type Result<T> = std::result::Result<T, ExecutionError>;

/// `FunctionContext` is a context object passed to functions when they are called.
///
/// It contains references to the target object (if the function is called as
/// a method), the program context ([`Context`]) which gives functions access
/// to variables, and the arguments to the function call.
#[derive(Clone)]
pub struct FunctionContext<'context, 'call: 'context> {
    pub name: &'call str,
    pub this: Option<Value>,
    pub ptx: &'context Context<'context>,
    pub args: &'call [Expression],
    pub arg_idx: usize,
}

impl<'context, 'call: 'context> FunctionContext<'context, 'call> {
    pub fn new(
        name: &'call str,
        this: Option<Value>,
        ptx: &'context Context<'context>,
        args: &'call [Expression],
    ) -> Self {
        Self {
            name,
            this,
            ptx,
            args,
            arg_idx: 0,
        }
    }

    /// Resolves the given expression using the program's [`Context`].
    pub fn resolve<R>(&self, resolver: R) -> Result<Value>
    where
        R: Resolver,
    {
        resolver.resolve(self)
    }

    /// Returns an execution error for the currently execution function.
    pub fn error<M: ToString>(&self, message: M) -> ExecutionError {
        ExecutionError::function_error(self.name, message)
    }
}

/// Calculates the size of either the target, or the provided args depending on how
/// the function is called.
///
/// If called as a method, the target will be used. If called as a function, the
/// first argument will be used.
///
/// The following [`Value`] variants are supported:
/// * [`Value::List`]
/// * [`Value::Map`]
/// * [`Value::String`]
/// * [`Value::Bytes`]
///
/// # Examples
/// ```skip
/// size([1, 2, 3]) == 3
/// ```
/// ```skip
/// 'foobar'.size() == 6
/// ```
pub fn size(ftx: &FunctionContext, This(this): This<Value>) -> Result<i64> {
    let size = match this {
        Value::List(l) => l.len(),
        Value::Map(m) => m.map.len(),
        Value::String(s) => s.len(),
        Value::Bytes(b) => b.len(),
        value => return Err(ftx.error(format!("cannot determine the size of {value:?}"))),
    };
    Ok(size as i64)
}

/// Returns true if the target contains the provided argument. The actual behavior
/// depends mainly on the type of the target.
///
/// The following [`Value`] variants are supported:
/// * [`Value::List`] - Returns true if the list contains the provided value.
/// * [`Value::Map`] - Returns true if the map contains the provided key.
/// * [`Value::String`] - Returns true if the string contains the provided substring.
/// * [`Value::Bytes`] - Returns true if the bytes contain the provided byte.
///
/// # Example
///
/// ## List
/// ```cel
/// [1, 2, 3].contains(1) == true
/// ```
///
/// ## Map
/// ```cel
/// {"a": 1, "b": 2, "c": 3}.contains("a") == true
/// ```
///
/// ## String
/// ```cel
/// "abc".contains("b") == true
/// ```
///
/// ## Bytes
/// ```cel
/// b"abc".contains(b"c") == true
/// ```
pub fn contains(This(this): This<Value>, arg: Value) -> Result<Value> {
    Ok(match this {
        Value::List(v) => v.contains(&arg),
        Value::Map(v) => v
            .map
            .contains_key(&arg.try_into().map_err(ExecutionError::UnsupportedKeyType)?),
        Value::String(s) => {
            if let Value::String(arg) = arg {
                s.contains(arg.as_str())
            } else {
                false
            }
        }
        Value::Bytes(b) => {
            if let Value::Bytes(arg) = arg {
                let s = arg.as_slice();
                b.windows(arg.len()).any(|w| w == s)
            } else {
                false
            }
        }
        _ => false,
    }
    .into())
}

// Performs a type conversion on the target. The following conversions are currently
// supported:
// * `string` - Returns a copy of the target string.
// * `timestamp` - Returns the timestamp in RFC3339 format.
// * `duration` - Returns the duration in a string formatted like "72h3m0.5s".
// * `int` - Returns the integer value of the target.
// * `uint` - Returns the unsigned integer value of the target.
// * `float` - Returns the float value of the target.
// * `bytes` - Converts bytes to string using from_utf8_lossy.
pub fn string(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => Value::String(v.clone()),
        #[cfg(feature = "chrono")]
        Value::Timestamp(t) => {
            // Format as RFC3339 with 'Z' suffix for UTC (not '+00:00')
            let utc = t.to_utc();
            // Check if there are nanoseconds
            let nanos = utc.timestamp_subsec_nanos();
            let formatted = if nanos == 0 {
                utc.to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
            } else {
                utc.to_rfc3339_opts(chrono::SecondsFormat::Nanos, true)
            };
            // Replace '+00:00' with 'Z' for UTC timestamps
            let formatted = formatted.replace("+00:00", "Z");
            Value::String(Arc::new(formatted))
        },
        #[cfg(feature = "chrono")]
        Value::Duration(v) => {
            // Format as decimal seconds (e.g., "1000000s")
            let total_secs = v.num_seconds();
            let nanos = v.num_nanoseconds().unwrap_or(0) % 1_000_000_000;
            if nanos == 0 {
                Value::String(Arc::new(format!("{}s", total_secs)))
            } else {
                // Include fractional seconds
                let frac = (nanos as f64) / 1_000_000_000.0;
                let total = total_secs as f64 + frac;
                Value::String(Arc::new(format!("{}s", total)))
            }
        },
        Value::Int(v) => Value::String(v.to_string().into()),
        Value::UInt(v) => Value::String(v.to_string().into()),
        Value::Float(v) => Value::String(v.to_string().into()),
        Value::Bytes(v) => Value::String(Arc::new(String::from_utf8_lossy(v.as_slice()).into())),
        v => return Err(ftx.error(format!("cannot convert {v:?} to string"))),
    })
}

pub fn bytes(value: Arc<String>) -> Result<Value> {
    Ok(Value::Bytes(value.as_bytes().to_vec().into()))
}

// Performs a type conversion on the target.
pub fn double(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => v
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|e| ftx.error(format!("string parse error: {e}")))?,
        Value::Float(v) => Value::Float(v),
        Value::Int(v) => Value::Float(v as f64),
        Value::UInt(v) => Value::Float(v as f64),
        v => return Err(ftx.error(format!("cannot convert {v:?} to double"))),
    })
}

// Performs a type conversion on the target.
pub fn uint(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => v
            .parse::<u64>()
            .map(Value::UInt)
            .map_err(|e| ftx.error(format!("string parse error: {e}")))?,
        Value::Float(v) => {
            if v > u64::MAX as f64 || v < u64::MIN as f64 {
                return Err(ftx.error("unsigned integer overflow"));
            }
            Value::UInt(v as u64)
        }
        Value::Int(v) => Value::UInt(
            v.try_into()
                .map_err(|_| ftx.error("unsigned integer overflow"))?,
        ),
        Value::UInt(v) => Value::UInt(v),
        v => return Err(ftx.error(format!("cannot convert {v:?} to uint"))),
    })
}

// Performs a type conversion on the target.
pub fn int(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => v
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|e| ftx.error(format!("string parse error: {e}")))?,
        Value::Float(v) => {
            if v > i64::MAX as f64 || v < i64::MIN as f64 {
                return Err(ftx.error("integer overflow"));
            }
            Value::Int(v as i64)
        }
        Value::Int(v) => Value::Int(v),
        Value::UInt(v) => Value::Int(v.try_into().map_err(|_| ftx.error("integer overflow"))?),
        #[cfg(feature = "chrono")]
        Value::Timestamp(t) => Value::Int(t.timestamp()),
        v => return Err(ftx.error(format!("cannot convert {v:?} to int"))),
    })
}

/// The `dyn` function marks a value as dynamic during type-checking.
/// At runtime, it simply returns the value unchanged.
pub fn dyn_(_ftx: &FunctionContext, value: Value) -> Result<Value> {
    // At runtime, dyn is a no-op - just return the value
    Ok(value)
}

/// The `type` function returns the type name of a value as a string.
/// This is used for type checking and introspection.
pub fn type_(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    let type_name = match this {
        Value::Null => "null_type",
        Value::Bool(_) => "bool",
        Value::Int(_) => "int",
        Value::UInt(_) => "uint",
        Value::Float(_) => "double",
        Value::String(_) => "string",
        Value::Bytes(_) => "bytes",
        Value::List(_) => "list",
        Value::Map(_) => "map",
        Value::Struct(s) => {
            // Clone the type name to avoid lifetime issues
            return Ok(Value::String(s.type_name.clone()));
        }
        #[cfg(feature = "chrono")]
        Value::Timestamp(_) => "google.protobuf.Timestamp",
        #[cfg(feature = "chrono")]
        Value::Duration(_) => "google.protobuf.Duration",
        Value::Function(_, _) => "function",
        Value::Opaque(_) => return Err(ftx.error("type() not supported for opaque values")),
    };
    Ok(Value::String(Arc::new(type_name.to_string())))
}

/// Returns an empty list. Used for type denotation.
pub fn list_constructor(_ftx: &FunctionContext) -> Result<Value> {
    Ok(Value::List(Arc::new(vec![])))
}

/// Returns an empty map. Used for type denotation.
pub fn map_constructor(_ftx: &FunctionContext) -> Result<Value> {
    Ok(Value::Map(crate::objects::Map { map: Arc::new(std::collections::HashMap::new()) }))
}

/// Returns the null_type string. Used for type denotation.
pub fn null_type(_ftx: &FunctionContext) -> Result<Value> {
    Ok(Value::String(Arc::new("null_type".to_string())))
}

/// Returns the math type. Used for type denotation.
pub fn math_type(_ftx: &FunctionContext) -> Result<Value> {
    Ok(Value::String(Arc::new("math".to_string())))
}

/// Returns false as the default bool value. Used for type denotation.
pub fn bool_constructor(_ftx: &FunctionContext) -> Result<Value> {
    Ok(Value::Bool(false))
}

pub fn optional_none(ftx: &FunctionContext) -> Result<Value> {
    if ftx.this.is_some() || !ftx.args.is_empty() {
        return Err(ftx.error("unsupported function"));
    }
    Ok(Value::Opaque(Arc::new(OptionalValue::none())))
}

pub fn optional_of(ftx: &FunctionContext, value: Value) -> Result<Value> {
    if ftx.this.is_some() {
        return Err(ftx.error("unsupported function"));
    }
    Ok(Value::Opaque(Arc::new(OptionalValue::of(value))))
}

pub fn optional_of_non_zero_value(ftx: &FunctionContext, value: Value) -> Result<Value> {
    if ftx.this.is_some() {
        return Err(ftx.error("unsupported function"));
    }
    if value.is_zero() {
        Ok(Value::Opaque(Arc::new(OptionalValue::none())))
    } else {
        Ok(Value::Opaque(Arc::new(OptionalValue::of(value))))
    }
}
pub fn optional_value(This(this): This<Value>) -> Result<Value> {
    <&OptionalValue>::try_from(&this)?
        .value()
        .cloned()
        .ok_or_else(|| ExecutionError::function_error("value", "optional.none() dereference"))
}

pub fn optional_has_value(This(this): This<Value>) -> Result<bool> {
    Ok(<&OptionalValue>::try_from(&this)?.value().is_some())
}

pub fn optional_or_optional(This(this): This<Value>, other: Value) -> Result<Value> {
    let this_opt: &OptionalValue = (&this).try_into()?;
    match this_opt.value() {
        Some(_) => Ok(this),
        None => {
            let _: &OptionalValue = (&other).try_into()?;
            Ok(other)
        }
    }
}

pub fn optional_or_value(This(this): This<Value>, other: Value) -> Result<Value> {
    let this_opt: &OptionalValue = (&this).try_into()?;
    match this_opt.value() {
        Some(v) => Ok(v.clone()),
        None => Ok(other),
    }
}

/// Returns true if a string starts with another string.
///
/// # Example
/// ```cel
/// "abc".startsWith("a") == true
/// ```
pub fn starts_with(This(this): This<Arc<String>>, prefix: Arc<String>) -> bool {
    this.starts_with(prefix.as_str())
}

/// Returns true if a string ends with another string.
///
/// # Example
/// ```cel
/// "abc".endsWith("c") == true
/// ```
pub fn ends_with(This(this): This<Arc<String>>, suffix: Arc<String>) -> bool {
    this.ends_with(suffix.as_str())
}

/// Returns the character at the specified index in the string.
///
/// # Example
/// ```cel
/// "abc".charAt(1) == "b"
/// ```
pub fn char_at(ftx: &FunctionContext, This(this): This<Arc<String>>, index: i64) -> Result<Value> {
    if index < 0 {
        return Err(ftx.error(format!("charAt index {} is negative", index)));
    }
    
    let chars: Vec<char> = this.chars().collect();
    let idx = index as usize;
    
    if idx >= chars.len() {
        return Err(ftx.error(format!("charAt index {} is out of range for string of length {}", index, chars.len())));
    }
    
    Ok(Value::String(Arc::new(chars[idx].to_string())))
}

/// Returns the index of the first occurrence of the specified substring.
/// Returns -1 if not found.
///
/// # Example
/// ```cel
/// "abc".indexOf("b") == 1
/// ```
pub fn index_of(This(this): This<Arc<String>>, substr: Arc<String>) -> Result<Value> {
    let index = match this.find(substr.as_str()) {
        Some(index) => index as i64,
        None => -1,
    };
    Ok(Value::Int(index))
}

/// Returns the index of the last occurrence of the specified substring.
/// Returns -1 if not found.
///
/// # Example
/// ```cel
/// "abcabc".lastIndexOf("b") == 4
/// ```
pub fn last_index_of(This(this): This<Arc<String>>, substr: Arc<String>) -> Result<Value> {
    let index = match this.rfind(substr.as_str()) {
        Some(index) => index as i64,
        None => -1,
    };
    Ok(Value::Int(index))
}

/// Returns a quoted string literal representation of the input string.
/// Escapes special characters appropriately.
///
/// # Example
/// ```cel
/// quote("hello\nworld") == "\"hello\\nworld\""
/// ```
pub fn quote(This(this): This<Arc<String>>) -> Result<Value> {
    let escaped = this
        .chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\\' => "\\\\".to_string(),
            '\n' => "\\n".to_string(),
            '\r' => "\\r".to_string(),
            '\t' => "\\t".to_string(),
            '\u{08}' => "\\b".to_string(),  // backspace
            '\u{0C}' => "\\f".to_string(),  // form feed
            c if c.is_control() => format!("\\u{{{:04X}}}", c as u32),
            c => c.to_string(),
        })
        .collect::<String>();
    
    Ok(Value::String(Arc::new(format!("\"{}\"", escaped))))
}

/// Returns true if the value is NaN.
pub fn is_nan(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::Float(f) => Ok(Value::Bool(f.is_nan())),
        Value::Int(_) | Value::UInt(_) => Ok(Value::Bool(false)),
        v => Err(ftx.error(format!("isNaN not supported for {v:?}"))),
    }
}

/// Returns the sign of the number: -1 for negative, 0 for zero, 1 for positive.
pub fn sign(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::Int(i) => Ok(Value::Int(i.signum())),
        Value::UInt(u) => Ok(Value::Int(if u == 0 { 0 } else { 1 })),
        Value::Float(f) => {
            if f.is_nan() {
                Ok(Value::Float(f))
            } else if f > 0.0 {
                Ok(Value::Int(1))
            } else if f < 0.0 {
                Ok(Value::Int(-1))
            } else {
                Ok(Value::Int(0))
            }
        },
        v => Err(ftx.error(format!("sign not supported for {v:?}"))),
    }
}

/// Splits a string by a delimiter.
pub fn split(This(this): This<Arc<String>>, delimiter: Arc<String>) -> Result<Value> {
    let parts: Vec<Value> = this
        .split(delimiter.as_str())
        .map(|part| Value::String(Arc::new(part.to_string())))
        .collect();
    Ok(Value::List(Arc::new(parts)))
}

/// Returns a substring of the string from start index to end index.
pub fn substring(ftx: &FunctionContext, This(this): This<Arc<String>>, start: i64, end: i64) -> Result<Value> {
    if start < 0 {
        return Err(ftx.error(format!("substring start index {} is negative", start)));
    }
    if end < 0 {
        return Err(ftx.error(format!("substring end index {} is negative", end)));
    }
    if start > end {
        return Err(ftx.error(format!("substring start index {} is greater than end index {}", start, end)));
    }
    
    let chars: Vec<char> = this.chars().collect();
    let start_idx = start as usize;
    let end_idx = end as usize;
    
    if start_idx >= chars.len() {
        return Ok(Value::String(Arc::new(String::new())));
    }
    
    let end_idx = end_idx.min(chars.len());
    let substring: String = chars[start_idx..end_idx].iter().collect();
    
    Ok(Value::String(Arc::new(substring)))
}

/// Trims whitespace from both ends of the string.
pub fn trim(This(this): This<Arc<String>>) -> Result<Value> {
    Ok(Value::String(Arc::new(this.trim().to_string())))
}

/// Returns true if a string matches the regular expression.
///
/// # Example
/// ```cel
/// "abc".matches("^[a-z]*$") == true
/// ```
#[cfg(feature = "regex")]
pub fn matches(
    ftx: &FunctionContext,
    This(this): This<Arc<String>>,
    regex: Arc<String>,
) -> Result<bool> {
    match regex::Regex::new(&regex) {
        Ok(re) => Ok(re.is_match(&this)),
        Err(err) => Err(ftx.error(format!("'{regex}' not a valid regex:\n{err}"))),
    }
}

#[cfg(feature = "chrono")]
pub use time::duration;
#[cfg(feature = "chrono")]
pub use time::timestamp;

#[cfg(feature = "chrono")]
pub mod time {
    use super::{FunctionContext, Result};
    use crate::magic::This;
    use crate::{ExecutionError, Value};
    use chrono::{Datelike, Days, Months, Timelike};
    use std::sync::Arc;

    /// Duration parses the provided argument into a [`Value::Duration`] value.
    ///
    /// Supports two overloads:
    /// - `duration(string)` - parses a duration string
    /// - `duration(duration)` - identity function, returns the duration unchanged
    ///
    /// The string argument must be in the format of a duration. See
    /// the [`parse_duration`] documentation for more information on the supported
    /// formats.
    ///
    /// # Examples
    /// - `1h` parses as 1 hour
    /// - `1.5h` parses as 1 hour and 30 minutes
    /// - `1h30m` parses as 1 hour and 30 minutes
    /// - `1h30m1s` parses as 1 hour, 30 minutes, and 1 second
    /// - `1ms` parses as 1 millisecond
    /// - `1.5ms` parses as 1 millisecond and 500 microseconds
    /// - `1ns` parses as 1 nanosecond
    /// - `1.5ns` parses as 1 nanosecond (sub-nanosecond durations not supported)
    /// Duration function that handles both string and duration arguments
    /// Overloads:
    /// - duration(string) -> duration
    /// - duration(duration) -> duration (identity)
    pub fn duration(value: Arc<String>) -> crate::functions::Result<Value> {
        Ok(Value::Duration(_duration(value.as_str())?))
    }
    
    /// Duration function that accepts a Value (for overload support)
    /// This handles the case where duration is called with a duration argument
    pub fn duration_value(ftx: &FunctionContext, value: Value) -> crate::functions::Result<Value> {
        match value {
            Value::Duration(d) => Ok(Value::Duration(d)),
            Value::String(s) => Ok(Value::Duration(_duration(s.as_str())?)),
            _ => Err(ftx.error("duration() requires a string or duration argument")),
        }
    }
    
    /// Duration conversion - handles both string and duration arguments
    /// This provides the overload: duration(duration) -> duration (identity)
    pub fn duration_from_value(ftx: &FunctionContext, This(this): This<Value>) -> crate::functions::Result<Value> {
        match this {
            Value::Duration(d) => Ok(Value::Duration(d)),
            Value::String(s) => {
                // Try to parse as string (fallback for method call style)
                Ok(Value::Duration(_duration(s.as_str())?))
            }
            _ => Err(ftx.error("duration() requires a string or duration argument")),
        }
    }

    /// Timestamp parses the provided argument into a [`Value::Timestamp`] value.
    /// The
    pub fn timestamp(value: Arc<String>) -> Result<Value> {
        Ok(Value::Timestamp(
            chrono::DateTime::parse_from_rfc3339(value.as_str())
                .map_err(|e| ExecutionError::function_error("timestamp", e.to_string().as_str()))?,
        ))
    }
    
    /// Timestamp conversion - handles string, int, and timestamp arguments
    /// Overloads:
    /// - timestamp(int) -> timestamp (converts Unix timestamp in seconds)
    /// - timestamp(timestamp) -> timestamp (identity)
    pub fn timestamp_from_int(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
        match this {
            Value::Int(secs) => {
                use chrono::{DateTime, FixedOffset, TimeZone};
                let ts = FixedOffset::east_opt(0)
                    .unwrap()
                    .timestamp_opt(secs, 0)
                    .single()
                    .ok_or_else(|| ftx.error("invalid timestamp"))?;
                Ok(Value::Timestamp(ts))
            }
            #[cfg(feature = "chrono")]
            Value::Timestamp(t) => Ok(Value::Timestamp(t)),
            Value::String(s) => {
                Ok(Value::Timestamp(
                    chrono::DateTime::parse_from_rfc3339(s.as_str())
                        .map_err(|e| ftx.error(format!("timestamp parse error: {e}")))?,
                ))
            }
            _ => Err(ftx.error("timestamp() requires a string, int, or timestamp argument")),
        }
    }

    /// A wrapper around [`parse_duration`] that converts errors into [`ExecutionError`].
    /// and only returns the duration, rather than returning the remaining input.
    fn _duration(i: &str) -> Result<chrono::Duration> {
        let (_, duration) = crate::duration::parse_duration(i)
            .map_err(|e| ExecutionError::function_error("duration", e.to_string()))?;
        Ok(duration)
    }

    fn _timestamp(i: &str) -> Result<chrono::DateTime<chrono::FixedOffset>> {
        chrono::DateTime::parse_from_rfc3339(i)
            .map_err(|e| ExecutionError::function_error("timestamp", e.to_string()))
    }

    pub fn timestamp_year(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok(this.year().into())
    }

    pub fn timestamp_month(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.month0() as i32).into())
    }

    pub fn timestamp_year_day(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        let year = this
            .checked_sub_days(Days::new(this.day0() as u64))
            .unwrap()
            .checked_sub_months(Months::new(this.month0()))
            .unwrap();
        Ok(this.signed_duration_since(year).num_days().into())
    }

    pub fn timestamp_month_day(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.day0() as i32).into())
    }

    pub fn timestamp_date(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.day() as i32).into())
    }

    pub fn timestamp_weekday(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.weekday().num_days_from_sunday() as i32).into())
    }

    pub fn timestamp_hours(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.hour() as i32).into())
    }

    pub fn timestamp_minutes(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.minute() as i32).into())
    }

    pub fn timestamp_seconds(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.second() as i32).into())
    }

    pub fn timestamp_millis(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.timestamp_subsec_millis() as i32).into())
    }
}

pub fn max(Arguments(args): Arguments) -> Result<Value> {
    // If items is a list of values, then operate on the list
    let items = if args.len() == 1 {
        match &args[0] {
            Value::List(values) => values,
            _ => return Ok(args[0].clone()),
        }
    } else {
        &args
    };

    items
        .iter()
        .skip(1)
        .try_fold(items.first().unwrap_or(&Value::Null), |acc, x| {
            match acc.partial_cmp(x) {
                Some(Ordering::Greater) => Ok(acc),
                Some(_) => Ok(x),
                None => Err(ExecutionError::ValuesNotComparable(acc.clone(), x.clone())),
            }
        })
        .cloned()
}

pub fn min(Arguments(args): Arguments) -> Result<Value> {
    // If items is a list of values, then operate on the list
    let items = if args.len() == 1 {
        match &args[0] {
            Value::List(values) => values,
            _ => return Ok(args[0].clone()),
        }
    } else {
        &args
    };

    items
        .iter()
        .skip(1)
        .try_fold(items.first().unwrap_or(&Value::Null), |acc, x| {
            match acc.partial_cmp(x) {
                Some(Ordering::Less) => Ok(acc),
                Some(_) => Ok(x),
                None => Err(ExecutionError::ValuesNotComparable(acc.clone(), x.clone())),
            }
        })
        .cloned()
}

/// Returns the greatest value from the arguments.
/// Similar to max but named "greatest" for CEL compatibility.
pub fn greatest(Arguments(args): Arguments) -> Result<Value> {
    max(Arguments(args))
}

/// Returns the least value from the arguments.
/// Similar to min but named "least" for CEL compatibility.
pub fn least(Arguments(args): Arguments) -> Result<Value> {
    min(Arguments(args))
}

/// Truncates a floating point number to an integer.
pub fn trunc(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::Float(f) => Ok(Value::Float(f.trunc())),
        Value::Int(i) => Ok(Value::Int(i)),
        Value::UInt(u) => Ok(Value::UInt(u)),
        v => Err(ftx.error(format!("cannot truncate {v:?}"))),
    }
}

/// Returns the floor of a floating point number.
pub fn floor(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::Float(f) => Ok(Value::Float(f.floor())),
        Value::Int(i) => Ok(Value::Int(i)),
        Value::UInt(u) => Ok(Value::UInt(u)),
        v => Err(ftx.error(format!("cannot floor {v:?}"))),
    }
}

/// Returns the ceiling of a floating point number.
pub fn ceil(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::Float(f) => Ok(Value::Float(f.ceil())),
        Value::Int(i) => Ok(Value::Int(i)),
        Value::UInt(u) => Ok(Value::UInt(u)),
        v => Err(ftx.error(format!("cannot ceil {v:?}"))),
    }
}

/// Returns true if the value is finite (not NaN or infinity).
pub fn is_finite(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::Float(f) => Ok(Value::Bool(f.is_finite())),
        Value::Int(_) | Value::UInt(_) => Ok(Value::Bool(true)),
        v => Err(ftx.error(format!("isFinite not supported for {v:?}"))),
    }
}

/// Returns true if the value is infinite.
pub fn is_inf(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::Float(f) => Ok(Value::Bool(f.is_infinite())),
        Value::Int(_) | Value::UInt(_) => Ok(Value::Bool(false)),
        v => Err(ftx.error(format!("isInf not supported for {v:?}"))),
    }
}

/// Bitwise OR operation.
pub fn bit_or(ftx: &FunctionContext, left: Value, right: Value) -> Result<Value> {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
        (Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a | b)),
        (Value::Int(a), Value::UInt(b)) => {
            Ok(Value::UInt(a as u64 | b))
        }
        (Value::UInt(a), Value::Int(b)) => {
            Ok(Value::UInt(a | b as u64))
        }
        (left, right) => Err(ftx.error(format!("bitOr not supported for {left:?} and {right:?}"))),
    }
}

/// Bitwise XOR operation.
pub fn bit_xor(ftx: &FunctionContext, left: Value, right: Value) -> Result<Value> {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
        (Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a ^ b)),
        (Value::Int(a), Value::UInt(b)) => {
            Ok(Value::UInt(a as u64 ^ b))
        }
        (Value::UInt(a), Value::Int(b)) => {
            Ok(Value::UInt(a ^ b as u64))
        }
        (left, right) => Err(ftx.error(format!("bitXor not supported for {left:?} and {right:?}"))),
    }
}

/// Converts a string to lowercase (ASCII only).
pub fn lower_ascii(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::String(s) => {
            Ok(Value::String(Arc::new(s.to_lowercase())))
        }
        v => Err(ftx.error(format!("lowerAscii not supported for {v:?}"))),
    }
}

/// Converts a string to uppercase (ASCII only).
pub fn upper_ascii(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::String(s) => {
            Ok(Value::String(Arc::new(s.to_uppercase())))
        }
        v => Err(ftx.error(format!("upperAscii not supported for {v:?}"))),
    }
}

/// Reverses a list.
pub fn reverse(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    match this {
        Value::List(list) => {
            let mut reversed = list.as_ref().clone();
            reversed.reverse();
            Ok(Value::List(Arc::new(reversed)))
        }
        v => Err(ftx.error(format!("reverse not supported for {v:?}"))),
    }
}

/// Optional map operation: applies a function to the value if present.
/// Called as: optional.optMap(var_name, expression)
/// If the optional has a value, binds var_name to that value and evaluates expression.
pub fn optional_map(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    let opt: &OptionalValue = (&this).try_into()?;
    
    // optMap takes 2 arguments: variable name and expression
    if ftx.args.len() != 2 {
        return Err(ftx.error("optMap requires 2 arguments: variable name and expression"));
    }
    
    match opt.value() {
        Some(value) => {
            // Extract variable name from first argument (should be an identifier)
            let var_name = match &ftx.args[0].expr {
                crate::common::ast::Expr::Ident(name) => name.clone(),
                _ => return Err(ftx.error("optMap first argument must be a variable name")),
            };
            
            // Create a new context with the variable bound
            let mut new_ctx = ftx.ptx.new_inner_scope();
            new_ctx.add_variable_from_value(&var_name, value.clone());
            
            // Evaluate the expression in the new context
            let result = Value::resolve(&ftx.args[1], &new_ctx)?;
            
            // Return a new optional with the result
            Ok(Value::Opaque(Arc::new(OptionalValue::of(result))))
        }
        None => Ok(Value::Opaque(Arc::new(OptionalValue::none()))),
    }
}

/// Optional flat map operation: applies a function that returns an optional.
/// Called as: optional.optFlatMap(var_name, expression)
/// If the optional has a value, binds var_name to that value and evaluates expression.
/// The expression must return an optional value.
pub fn optional_flat_map(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    let opt: &OptionalValue = (&this).try_into()?;
    
    // optFlatMap takes 2 arguments: variable name and expression
    if ftx.args.len() != 2 {
        return Err(ftx.error("optFlatMap requires 2 arguments: variable name and expression"));
    }
    
    match opt.value() {
        Some(value) => {
            // Extract variable name from first argument (should be an identifier)
            let var_name = match &ftx.args[0].expr {
                crate::common::ast::Expr::Ident(name) => name.clone(),
                _ => return Err(ftx.error("optFlatMap first argument must be a variable name")),
            };
            
            // Create a new context with the variable bound
            let mut new_ctx = ftx.ptx.new_inner_scope();
            new_ctx.add_variable_from_value(&var_name, value.clone());
            
            // Evaluate the expression in the new context
            let result = Value::resolve(&ftx.args[1], &new_ctx)?;
            
            // The result should be an optional - if it's not, wrap it
            if let Ok(_result_opt) = <&OptionalValue>::try_from(&result) {
                Ok(result.clone())
            } else {
                // If the expression doesn't return an optional, wrap it
                Ok(Value::Opaque(Arc::new(OptionalValue::of(result))))
            }
        }
        None => Ok(Value::Opaque(Arc::new(OptionalValue::none()))),
    }
}

/// String formatting function.
/// Called as: "format string".format([arg1, arg2, ...])
/// Supports printf-style format specifiers: %s, %d, %f, %e, %x, %X, %o, %b, %%
pub fn format(ftx: &FunctionContext, This(this): This<Value>, args: Arguments) -> Result<Value> {
    // Get the format string
    let format_str = match &this {
        Value::String(s) => s.as_str(),
        _ => return Err(ftx.error("format() must be called on a string")),
    };
    
    // Get the arguments as a list
    // Arguments is a tuple struct wrapping Arc<Vec<Value>>
    let arg_list = if args.0.len() == 1 {
        match &args.0[0] {
            Value::List(values) => values.as_ref(),
            _ => return Err(ftx.error("format() requires a list of arguments")),
        }
    } else {
        return Err(ftx.error("format() requires exactly one argument (a list)"));
    };
    
    let mut result = std::string::String::new();
    let mut arg_index = 0;
    let mut chars = format_str.chars().peekable();
    
    while let Some(ch) = chars.next() {
        if ch == '%' {
            // Check for %%
            if chars.peek() == Some(&'%') {
                chars.next(); // consume the second %
                result.push('%');
                continue;
            }
            
            // Parse format specifier: %[.precision]conversion
            let mut precision: Option<usize> = None;
            
            // Check for precision (e.g., %.3f)
            if chars.peek() == Some(&'.') {
                chars.next(); // consume '.'
                let mut prec_str = std::string::String::new();
                while let Some(&next_ch) = chars.peek() {
                    if next_ch.is_ascii_digit() {
                        prec_str.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if !prec_str.is_empty() {
                    precision = Some(prec_str.parse().unwrap_or(6));
                }
            }
            
            // Get conversion character
            let conversion = chars.next().ok_or_else(|| {
                ftx.error("incomplete format specifier")
            })?;
            
            // Get the argument
            if arg_index >= arg_list.len() {
                return Err(ftx.error("not enough arguments for format string"));
            }
            let arg = &arg_list[arg_index];
            arg_index += 1;
            
            // Format based on conversion type
            let formatted = format_value(arg, conversion, precision, ftx)?;
            result.push_str(&formatted);
        } else {
            result.push(ch);
        }
    }
    
    if arg_index < arg_list.len() {
        return Err(ftx.error("too many arguments for format string"));
    }
    
    Ok(Value::String(Arc::new(std::string::String::from(result))))
}

/// Formats a single value according to a conversion specifier.
fn format_value(
    value: &Value,
    conversion: char,
    precision: Option<usize>,
    ftx: &FunctionContext,
) -> std::result::Result<String, ExecutionError> {
    let default_precision = precision.unwrap_or(6);
    
    match (conversion, value) {
        ('s', _) => {
            // String conversion - handles all types
            Ok(format_value_as_string(value))
        }
        ('d', Value::Int(i)) => Ok(i.to_string()),
        ('d', Value::UInt(u)) => Ok(u.to_string()),
        ('d', Value::Float(f)) => {
            if f.is_nan() {
                Ok("NaN".to_string())
            } else if f.is_infinite() {
                Ok(if f.is_sign_negative() { "-Infinity".to_string() } else { "Infinity".to_string() })
            } else {
                Ok(format!("{}", f))
            }
        }
        ('f', Value::Int(i)) => {
            format_float(*i as f64, default_precision, false)
        }
        ('f', Value::UInt(u)) => {
            format_float(*u as f64, default_precision, false)
        }
        ('f', Value::Float(f)) => {
            if f.is_nan() {
                Ok("NaN".to_string())
            } else if f.is_infinite() {
                Ok(if f.is_sign_negative() { "-Infinity".to_string() } else { "Infinity".to_string() })
            } else {
                format_float(*f, default_precision, false)
            }
        }
        ('e', Value::Int(i)) => {
            format_float(*i as f64, default_precision, true)
        }
        ('e', Value::UInt(u)) => {
            format_float(*u as f64, default_precision, true)
        }
        ('e', Value::Float(f)) => {
            if f.is_nan() {
                Ok("NaN".to_string())
            } else if f.is_infinite() {
                Ok(if f.is_sign_negative() { "-Infinity".to_string() } else { "Infinity".to_string() })
            } else {
                format_float(*f, default_precision, true)
            }
        }
        ('x', Value::Int(i)) => {
            if *i < 0 {
                Ok(format!("-{:x}", (-i) as u64))
            } else {
                Ok(format!("{:x}", *i as u64))
            }
        }
        ('x', Value::UInt(u)) => Ok(format!("{:x}", u)),
        ('x', Value::String(s)) => {
            // Convert string to bytes and format as hex
            Ok(s.as_bytes().iter().map(|b| format!("{:02x}", b)).collect())
        }
        ('x', Value::Bytes(b)) => {
            Ok(b.as_slice().iter().map(|b| format!("{:02x}", b)).collect())
        }
        ('X', Value::Int(i)) => {
            if *i < 0 {
                Ok(format!("-{:X}", (-i) as u64))
            } else {
                Ok(format!("{:X}", *i as u64))
            }
        }
        ('X', Value::UInt(u)) => Ok(format!("{:X}", u)),
        ('X', Value::String(s)) => {
            // Convert string to bytes and format as hex
            Ok(s.as_bytes().iter().map(|b| format!("{:02X}", b)).collect())
        }
        ('X', Value::Bytes(b)) => {
            Ok(b.as_slice().iter().map(|b| format!("{:02X}", b)).collect())
        }
        ('o', Value::Int(i)) => {
            if *i < 0 {
                Ok(format!("-{:o}", (-i) as u64))
            } else {
                Ok(format!("{:o}", *i as u64))
            }
        }
        ('o', Value::UInt(u)) => Ok(format!("{:o}", u)),
        ('b', Value::Int(i)) => {
            if *i < 0 {
                Ok(format!("-{:b}", (-i) as u64))
            } else {
                Ok(format!("{:b}", *i as u64))
            }
        }
        ('b', Value::UInt(u)) => Ok(format!("{:b}", u)),
        ('b', Value::Bool(b)) => Ok(if *b { "1".to_string() } else { "0".to_string() }),
        _ => Err(ftx.error(format!("unsupported format specifier %{} for value type", conversion))),
    }
}

/// Formats a float with the given precision.
/// Uses banker's rounding (round half to even).
fn format_float(f: f64, precision: usize, scientific: bool) -> std::result::Result<String, ExecutionError> {
    if precision == 0 {
        if scientific {
            // For %e with precision 0, we still need the decimal point
            Ok(format!("{:.0e}", f))
        } else {
            // For %f with precision 0, no decimal point
            Ok(format!("{:.0}", f))
        }
    } else if scientific {
        // Scientific notation: d.dddddde±dd
        // Rust's format! with 'e' doesn't always include the + sign, so we need to handle it
        let formatted = format!("{:.*e}", precision, f);
        // Ensure the exponent has 2 digits and always includes the sign
        if let Some(e_pos) = formatted.find('e') {
            let (mantissa, exp_part) = formatted.split_at(e_pos);
            let exp_str = if exp_part.starts_with("e+") {
                let exp_num: i32 = exp_part[2..].parse().unwrap_or(0);
                format!("e+{:02}", exp_num)
            } else if exp_part.starts_with("e-") {
                let exp_num: i32 = exp_part[2..].parse().unwrap_or(0);
                format!("e-{:02}", exp_num)
            } else if exp_part.len() > 1 && exp_part[1..].chars().next().unwrap().is_ascii_digit() {
                // Handle case where Rust omits the + sign (e.g., "e3" instead of "e+03")
                let exp_num: i32 = exp_part[1..].parse().unwrap_or(0);
                if exp_num >= 0 {
                    format!("e+{:02}", exp_num)
                } else {
                    format!("e-{:02}", -exp_num)
                }
            } else {
                exp_part.to_string()
            };
            Ok(format!("{}{}", mantissa, exp_str))
        } else {
            Ok(formatted)
        }
    } else {
        // Fixed point: dddddd.dddddd
        Ok(format!("{:.*}", precision, f))
    }
}

/// Formats a value as a string (for %s conversion).
/// This handles all types including lists, maps, timestamps, durations, etc.
fn format_value_as_string(value: &Value) -> String {
    use crate::objects::Value::*;
    
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::UInt(u) => u.to_string(),
        Value::Float(f) => {
            if f.is_nan() {
                "NaN".to_string()
            } else if f.is_infinite() {
                if f.is_sign_negative() {
                    "-Infinity".to_string()
                } else {
                    "Infinity".to_string()
                }
            } else {
                // Remove trailing zeros and decimal point if not needed
                let s = f.to_string();
                if s.contains('.') {
                    s.trim_end_matches('0').trim_end_matches('.').to_string()
                } else {
                    s
                }
            }
        }
        Value::String(s) => s.as_str().to_string(),
        Value::Bytes(b) => {
            // Convert bytes to string using from_utf8_lossy
            std::string::String::from_utf8_lossy(b.as_slice()).replace('\u{fffd}', "\u{fffd}")
        }
        Value::List(list) => {
            let items: Vec<std::string::String> = list.iter().map(|v| format_value_as_string(v)).collect();
            format!("[{}]", items.join(", "))
        }
        Value::Map(map) => {
            // Sort keys by their string representation
            let mut entries: Vec<(std::string::String, std::string::String)> = map.map.iter()
                .map(|(k, v)| (format_key_as_string(k), format_value_as_string(v)))
                .collect();
            entries.sort_by(|a, b| a.0.cmp(&b.0));
            let pairs: Vec<std::string::String> = entries.iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
        Value::Struct(s) => {
            // Format struct fields
            let mut entries: Vec<(std::string::String, std::string::String)> = s.fields.iter()
                .map(|(k, v): (&std::string::String, &Value)| (k.clone(), format_value_as_string(v)))
                .collect();
            entries.sort_by(|a, b| a.0.cmp(&b.0));
            let pairs: Vec<std::string::String> = entries.iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
        #[cfg(feature = "chrono")]
        Value::Timestamp(t) => {
            // Format as RFC3339 with 'Z' suffix for UTC (not '+00:00')
            let utc = t.to_utc();
            // Use to_rfc3339_opts with appropriate precision
            let nanos = utc.timestamp_subsec_nanos();
            let formatted = if nanos == 0 {
                utc.to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
            } else {
                utc.to_rfc3339_opts(chrono::SecondsFormat::Nanos, true)
            };
            // Replace '+00:00' with 'Z' for UTC timestamps
            formatted.replace("+00:00", "Z")
        }
        #[cfg(feature = "chrono")]
        Value::Duration(d) => {
            // Format as decimal seconds
            let total_secs = d.num_seconds();
            let nanos = d.num_nanoseconds().unwrap_or(0) % 1_000_000_000;
            if nanos == 0 {
                format!("{}s", total_secs)
            } else {
                // Include fractional seconds
                let frac = (nanos as f64) / 1_000_000_000.0;
                let total = total_secs as f64 + frac;
                format!("{}s", total)
            }
        }
        // Dyn values are wrapped in Opaque, handle them there
        Value::Function(_, _) => "<function>".to_string(),
        Value::Opaque(_) => {
            // Try to convert OptionalValue
            if let Ok(opt) = <&OptionalValue>::try_from(value) {
                if let Some(inner) = opt.value() {
                    format_value_as_string(inner)
                } else {
                    "null".to_string()
                }
            } else {
                "<opaque>".to_string()
            }
        }
    }
}

/// Formats a map key as a string.
fn format_key_as_string(key: &crate::objects::Key) -> String {
    use crate::objects::Key::*;
    match key {
        Int(i) => i.to_string(),
        Uint(u) => u.to_string(),
        String(s) => s.as_str().to_string(),
        Bool(b) => b.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::tests::test_script;

    fn assert_script(input: &(&str, &str)) {
        assert_eq!(test_script(input.1, None), Ok(true.into()), "{}", input.0);
    }

    fn assert_error(input: &(&str, &str, &str)) {
        assert_eq!(
            test_script(input.1, None)
                .expect_err("expected error")
                .to_string(),
            input.2,
            "{}",
            input.0
        );
    }

    #[test]
    fn test_size() {
        [
            ("size of list", "size([1, 2, 3]) == 3"),
            ("size of map", "size({'a': 1, 'b': 2, 'c': 3}) == 3"),
            ("size of string", "size('foo') == 3"),
            ("size of bytes", "size(b'foo') == 3"),
            ("size as a list method", "[1, 2, 3].size() == 3"),
            ("size as a string method", "'foobar'.size() == 6"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_has() {
        let tests = vec![
            ("map has", "has(foo.bar) == true"),
            ("map not has", "has(foo.baz) == false"),
        ];

        for (name, script) in tests {
            let mut ctx = Context::default();
            ctx.add_variable_from_value("foo", std::collections::HashMap::from([("bar", 1)]));
            assert_eq!(test_script(script, Some(ctx)), Ok(true.into()), "{name}");
        }
    }

    #[test]
    fn test_map() {
        [
            ("map list", "[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
            ("map list 2", "[1, 2, 3].map(y, y + 1) == [2, 3, 4]"),
            (
                "map list filter",
                "[1, 2, 3].map(y, y % 2 == 0, y + 1) == [3]",
            ),
            (
                "nested map",
                "[[1, 2], [2, 3]].map(x, x.map(x, x * 2)) == [[2, 4], [4, 6]]",
            ),
            (
                "map to list",
                r#"{'John': 'smart'}.map(key, key) == ['John']"#,
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_filter() {
        [("filter list", "[1, 2, 3].filter(x, x > 2) == [3]")]
            .iter()
            .for_each(assert_script);
    }

    #[test]
    fn test_all() {
        [
            ("all list #1", "[0, 1, 2].all(x, x >= 0)"),
            ("all list #2", "[0, 1, 2].all(x, x > 0) == false"),
            ("all map", "{0: 0, 1:1, 2:2}.all(x, x >= 0) == true"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_exists() {
        [
            ("exist list #1", "[0, 1, 2].exists(x, x > 0)"),
            ("exist list #2", "[0, 1, 2].exists(x, x == 3) == false"),
            ("exist list #3", "[0, 1, 2, 2].exists(x, x == 2)"),
            ("exist map", "{0: 0, 1:1, 2:2}.exists(x, x > 0)"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_exists_one() {
        [
            ("exist list #1", "[0, 1, 2].exists_one(x, x > 0) == false"),
            ("exist list #2", "[0, 1, 2].exists_one(x, x == 0)"),
            ("exist map", "{0: 0, 1:1, 2:2}.exists_one(x, x == 2)"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_max() {
        [
            ("max single", "max(1) == 1"),
            ("max multiple", "max(1, 2, 3) == 3"),
            ("max negative", "max(-1, 0) == 0"),
            ("max float", "max(-1.0, 0.0) == 0.0"),
            ("max list", "max([1, 2, 3]) == 3"),
            ("max empty list", "max([]) == null"),
            ("max no args", "max() == null"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_min() {
        [
            ("min single", "min(1) == 1"),
            ("min multiple", "min(1, 2, 3) == 1"),
            ("min negative", "min(-1, 0) == -1"),
            ("min float", "min(-1.0, 0.0) == -1.0"),
            (
                "min float multiple",
                "min(1.61803, 3.1415, 2.71828, 1.41421) == 1.41421",
            ),
            ("min list", "min([1, 2, 3]) == 1"),
            ("min empty list", "min([]) == null"),
            ("min no args", "min() == null"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_starts_with() {
        [
            ("starts with true", "'foobar'.startsWith('foo') == true"),
            ("starts with false", "'foobar'.startsWith('bar') == false"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_ends_with() {
        [
            ("ends with true", "'foobar'.endsWith('bar') == true"),
            ("ends with false", "'foobar'.endsWith('foo') == false"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_timestamp() {
        [(
                "comparison",
                "timestamp('2023-05-29T00:00:00Z') > timestamp('2023-05-28T00:00:00Z')",
            ),
            (
                "comparison",
                "timestamp('2023-05-29T00:00:00Z') < timestamp('2023-05-30T00:00:00Z')",
            ),
            (
                "subtracting duration",
                "timestamp('2023-05-29T00:00:00Z') - duration('24h') == timestamp('2023-05-28T00:00:00Z')",
            ),
            (
                "subtracting date",
                "timestamp('2023-05-29T00:00:00Z') - timestamp('2023-05-28T00:00:00Z') == duration('24h')",
            ),
            (
                "adding duration",
                "timestamp('2023-05-28T00:00:00Z') + duration('24h') == timestamp('2023-05-29T00:00:00Z')",
            ),
            (
                "timestamp string",
                "timestamp('2023-05-28T00:00:00Z').string() == '2023-05-28T00:00:00+00:00'",
            ),
            (
                "timestamp getFullYear",
                "timestamp('2023-05-28T00:00:00Z').getFullYear() == 2023",
            ),
            (
                "timestamp getMonth",
                "timestamp('2023-05-28T00:00:00Z').getMonth() == 4",
            ),
            (
                "timestamp getDayOfMonth",
                "timestamp('2023-05-28T00:00:00Z').getDayOfMonth() == 27",
            ),
            (
                "timestamp getDayOfYear",
                "timestamp('2023-05-28T00:00:00Z').getDayOfYear() == 147",
            ),
            (
                "timestamp getDate",
                "timestamp('2023-05-28T00:00:00Z').getDate() == 28",
            ),
            (
                "timestamp getDayOfWeek",
                "timestamp('2023-05-28T00:00:00Z').getDayOfWeek() == 0",
            ),
            (
                "timestamp getHours",
                "timestamp('2023-05-28T02:00:00Z').getHours() == 2",
            ),
            (
                "timestamp getMinutes",
                " timestamp('2023-05-28T00:05:00Z').getMinutes() == 5",
            ),
            (
                "timestamp getSeconds",
                "timestamp('2023-05-28T00:00:06Z').getSeconds() == 6",
            ),
            (
                "timestamp getMilliseconds",
                "timestamp('2023-05-28T00:00:42.123Z').getMilliseconds() == 123",
            ),
        ]
        .iter()
        .for_each(assert_script);

        [
            (
                "timestamp out of range",
                "timestamp('0000-01-00T00:00:00Z')",
                "Error executing function 'timestamp': input is out of range",
            ),
            (
                "timestamp out of range",
                "timestamp('9999-12-32T23:59:59.999999999Z')",
                "Error executing function 'timestamp': input is out of range",
            ),
            (
                "timestamp overflow",
                "timestamp('9999-12-31T23:59:59Z') + duration('1s')",
                "Overflow from binary operator 'add': Timestamp(9999-12-31T23:59:59+00:00), Duration(TimeDelta { secs: 1, nanos: 0 })",
            ),
            (
                "timestamp underflow",
                "timestamp('0001-01-01T00:00:00Z') - duration('1s')",
                "Overflow from binary operator 'sub': Timestamp(0001-01-01T00:00:00+00:00), Duration(TimeDelta { secs: 1, nanos: 0 })",
            ),
            (
                "timestamp underflow",
                "timestamp('0001-01-01T00:00:00Z') + duration('-1s')",
                "Overflow from binary operator 'add': Timestamp(0001-01-01T00:00:00+00:00), Duration(TimeDelta { secs: -1, nanos: 0 })",
            ),
        ]
        .iter()
        .for_each(assert_error)
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_duration() {
        [
            ("duration equal 1", "duration('1s') == duration('1000ms')"),
            ("duration equal 2", "duration('1m') == duration('60s')"),
            ("duration equal 3", "duration('1h') == duration('60m')"),
            ("duration comparison 1", "duration('1m') > duration('1s')"),
            ("duration comparison 2", "duration('1m') < duration('1h')"),
            (
                "duration subtraction",
                "duration('1h') - duration('1m') == duration('59m')",
            ),
            (
                "duration addition",
                "duration('1h') + duration('1m') == duration('1h1m')",
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_timestamp_variable() {
        let mut context = Context::default();
        let ts: chrono::DateTime<chrono::FixedOffset> =
            chrono::DateTime::parse_from_rfc3339("2023-05-29T00:00:00Z").unwrap();
        context
            .add_variable("ts", crate::Value::Timestamp(ts))
            .unwrap();

        let program = crate::Program::compile("ts == timestamp('2023-05-29T00:00:00Z')").unwrap();
        let result = program.execute(&context).unwrap();
        assert_eq!(result, true.into());
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_chrono_string() {
        [
            ("duration", "duration('1h30m').string() == '1h30m0s'"),
            (
                "timestamp",
                "timestamp('2023-05-29T00:00:00Z').string() == '2023-05-29T00:00:00+00:00'",
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_contains() {
        let tests = vec![
            ("list", "[1, 2, 3].contains(3) == true"),
            ("map", "{1: true, 2: true, 3: true}.contains(3) == true"),
            ("string", "'foobar'.contains('bar') == true"),
            ("bytes", "b'foobar'.contains(b'o') == true"),
        ];

        for (name, script) in tests {
            assert_eq!(test_script(script, None), Ok(true.into()), "{name}");
        }
    }

    #[cfg(feature = "regex")]
    #[test]
    fn test_matches() {
        let tests = vec![
            ("string", "'foobar'.matches('^[a-zA-Z]*$') == true"),
            (
                "map",
                "{'1': 'abc', '2': 'def', '3': 'ghi'}.all(key, key.matches('^[a-zA-Z]*$')) == false",
            ),
        ];

        for (name, script) in tests {
            assert_eq!(
                test_script(script, None),
                Ok(true.into()),
                ".matches failed for '{name}'"
            );
        }
    }

    #[cfg(feature = "regex")]
    #[test]
    fn test_matches_err() {
        assert_eq!(
            test_script(
                "'foobar'.matches('(foo') == true", None),
            Err(
                crate::ExecutionError::FunctionError {
                    function: "matches".to_string(),
                    message: "'(foo' not a valid regex:\nregex parse error:\n    (foo\n    ^\nerror: unclosed group".to_string()
                }
            )
        );
    }

    #[test]
    fn test_string() {
        [
            ("string", "'foo'.string() == 'foo'"),
            ("int", "10.string() == '10'"),
            ("float", "10.5.string() == '10.5'"),
            ("bytes", "b'foo'.string() == 'foo'"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_bytes() {
        [
            ("string", "bytes('abc') == b'abc'"),
            ("bytes", "bytes('abc') == b'\\x61b\\x63'"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_double() {
        [
            ("string", "'10'.double() == 10.0"),
            ("int", "10.double() == 10.0"),
            ("double", "10.0.double() == 10.0"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_uint() {
        [
            ("string", "'10'.uint() == 10.uint()"),
            ("double", "10.5.uint() == 10.uint()"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_int() {
        [
            ("string", "'10'.int() == 10"),
            ("int", "10.int() == 10"),
            ("uint", "10.uint().int() == 10"),
            ("double", "10.5.int() == 10"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn no_bool_coercion() {
        [
            ("string || bool", "'' || false", "No such overload"),
            ("int || bool", "1 || false", "No such overload"),
            ("int || bool", "1u || false", "No such overload"),
            ("float || bool", "0.1|| false", "No such overload"),
            ("list || bool", "[] || false", "No such overload"),
            ("map || bool", "{} || false", "No such overload"),
            ("null || bool", "null || false", "No such overload"),
        ]
        .iter()
        .for_each(assert_error)
    }
}
