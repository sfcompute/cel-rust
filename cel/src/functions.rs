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
        Value::Struct(s) => s.fields.len(),
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
        Value::Timestamp(t) => Value::String(t.to_rfc3339().into()),
        #[cfg(feature = "chrono")]
        Value::Duration(v) => Value::String(crate::duration::format_duration(&v).into()),
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
        Value::String(v) => {
            let parsed = v
                .parse::<f64>()
                .map_err(|e| ftx.error(format!("string parse error: {e}")))?;

            // Handle special string values
            if v.eq_ignore_ascii_case("nan") {
                Value::Float(f64::NAN)
            } else if v.eq_ignore_ascii_case("inf") || v.eq_ignore_ascii_case("infinity") || v.as_str() == "+inf" {
                Value::Float(f64::INFINITY)
            } else if v.eq_ignore_ascii_case("-inf") || v.eq_ignore_ascii_case("-infinity") {
                Value::Float(f64::NEG_INFINITY)
            } else {
                Value::Float(parsed)
            }
        }
        Value::Float(v) => Value::Float(v),
        Value::Int(v) => Value::Float(v as f64),
        Value::UInt(v) => Value::Float(v as f64),
        v => return Err(ftx.error(format!("cannot convert {v:?} to double"))),
    })
}

// Performs a type conversion on the target, respecting f32 precision and range.
pub fn float(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => {
            // Parse as f64 first to handle special values and range
            let parsed_f64 = v
                .parse::<f64>()
                .map_err(|e| ftx.error(format!("string parse error: {e}")))?;

            // Handle special string values
            let value_f64 = if v.eq_ignore_ascii_case("nan") {
                f64::NAN
            } else if v.eq_ignore_ascii_case("inf") || v.eq_ignore_ascii_case("infinity") || v.as_str() == "+inf" {
                f64::INFINITY
            } else if v.eq_ignore_ascii_case("-inf") || v.eq_ignore_ascii_case("-infinity") {
                f64::NEG_INFINITY
            } else {
                parsed_f64
            };

            // Convert to f32 and back to f64 to apply f32 precision and range rules
            let as_f32 = value_f64 as f32;
            Value::Float(as_f32 as f64)
        }
        Value::Float(v) => {
            // Apply f32 precision and range rules
            let as_f32 = v as f32;
            Value::Float(as_f32 as f64)
        }
        Value::Int(v) => {
            let as_f32 = v as f32;
            Value::Float(as_f32 as f64)
        }
        Value::UInt(v) => {
            let as_f32 = v as f32;
            Value::Float(as_f32 as f64)
        }
        v => return Err(ftx.error(format!("cannot convert {v:?} to float"))),
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
            // Check for NaN and infinity
            if !v.is_finite() {
                return Err(ftx.error("cannot convert non-finite value to uint"));
            }
            // Check if value is negative
            if v < 0.0 {
                return Err(ftx.error("unsigned integer overflow"));
            }
            // More strict range checking for float to uint conversion
            if v > u64::MAX as f64 {
                return Err(ftx.error("unsigned integer overflow"));
            }
            // Additional check: ensure the float value, when truncated, is within bounds
            let truncated = v.trunc();
            if truncated < 0.0 || truncated > u64::MAX as f64 {
                return Err(ftx.error("unsigned integer overflow"));
            }
            Value::UInt(truncated as u64)
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
            // Check for NaN and infinity
            if !v.is_finite() {
                return Err(ftx.error("cannot convert non-finite value to int"));
            }
            // More strict range checking for float to int conversion
            // We need to ensure the value fits within i64 range and doesn't lose precision
            if v > i64::MAX as f64 || v < i64::MIN as f64 {
                return Err(ftx.error("integer overflow"));
            }
            // Additional check: ensure the float value, when truncated, is within bounds
            // This handles edge cases near the limits
            let truncated = v.trunc();
            if truncated > i64::MAX as f64 || truncated < i64::MIN as f64 {
                return Err(ftx.error("integer overflow"));
            }
            Value::Int(truncated as i64)
        }
        Value::Int(v) => Value::Int(v),
        Value::UInt(v) => Value::Int(v.try_into().map_err(|_| ftx.error("integer overflow"))?),
        v => return Err(ftx.error(format!("cannot convert {v:?} to int"))),
    })
}

// Performs a type conversion to list.
pub fn list(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::List(v) => Value::List(v.clone()),
        v => return Err(ftx.error(format!("cannot convert {v:?} to list"))),
    })
}

// Performs a type conversion to map.
pub fn map(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::Map(v) => Value::Map(v.clone()),
        v => return Err(ftx.error(format!("cannot convert {v:?} to map"))),
    })
}

// Performs a type conversion to null_type.
pub fn null_type(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::Null => Value::Null,
        v => return Err(ftx.error(format!("cannot convert {v:?} to null_type"))),
    })
}

// Performs a type conversion to dynamic type (dyn).
// In CEL, dyn() is essentially an identity function that returns the value as-is,
// indicating it should be treated as a dynamic type.
pub fn dyn_conversion(_ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(this)
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

pub fn optional_opt_map(ftx: &FunctionContext) -> Result<Value> {
    // optMap takes 2 arguments: the optional value (this) and a lambda/expression
    if ftx.this.is_none() || ftx.args.len() != 2 {
        return Err(ftx.error("optMap requires 2 arguments"));
    }

    let this = ftx.this.as_ref().unwrap();
    let this_opt: &OptionalValue = this.try_into()?;

    // If the optional has no value, return none
    if this_opt.value().is_none() {
        return Ok(Value::Opaque(Arc::new(OptionalValue::none())));
    }

    // Otherwise, we need to evaluate the lambda with the value
    // For now, return an error indicating this needs macro expansion support
    Err(ftx.error("optMap requires macro expansion support"))
}

pub fn optional_opt_flat_map(ftx: &FunctionContext) -> Result<Value> {
    // optFlatMap takes 2 arguments: the optional value (this) and a lambda/expression
    if ftx.this.is_none() || ftx.args.len() != 2 {
        return Err(ftx.error("optFlatMap requires 2 arguments"));
    }

    let this = ftx.this.as_ref().unwrap();
    let this_opt: &OptionalValue = this.try_into()?;

    // If the optional has no value, return none
    if this_opt.value().is_none() {
        return Ok(Value::Opaque(Arc::new(OptionalValue::none())));
    }

    // Otherwise, we need to evaluate the lambda with the value
    // For now, return an error indicating this needs macro expansion support
    Err(ftx.error("optFlatMap requires macro expansion support"))
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
    use super::Result;
    use crate::magic::This;
    use crate::{ExecutionError, Value};
    use chrono::{Datelike, Days, Months, Timelike};
    use std::sync::Arc;

    /// Duration parses the provided argument into a [`Value::Duration`] value.
    ///
    /// The argument must be string, and must be in the format of a duration. See
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
    pub fn duration(value: Arc<String>) -> crate::functions::Result<Value> {
        Ok(Value::Duration(_duration(value.as_str())?))
    }

    /// Timestamp parses the provided argument into a [`Value::Timestamp`] value.
    /// The
    pub fn timestamp(value: Arc<String>) -> Result<Value> {
        Ok(Value::Timestamp(
            chrono::DateTime::parse_from_rfc3339(value.as_str())
                .map_err(|e| ExecutionError::function_error("timestamp", e.to_string().as_str()))?,
        ))
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

    /// Parse a timezone string and convert a timestamp to that timezone.
    /// Supports fixed offset format like "+05:30" or "-08:00", or "UTC"/"Z".
    fn parse_timezone<Tz: chrono::TimeZone>(
        tz_str: &str,
        dt: chrono::DateTime<Tz>,
    ) -> Option<chrono::DateTime<chrono::FixedOffset>>
    where
        Tz::Offset: std::fmt::Display,
    {
        // Handle UTC special case
        if tz_str == "UTC" || tz_str == "Z" {
            return Some(dt.with_timezone(&chrono::Utc).fixed_offset());
        }

        // Try to parse as fixed offset (e.g., "+05:30", "-08:00")
        if let Some(offset) = parse_fixed_offset(tz_str) {
            return Some(dt.with_timezone(&offset));
        }

        None
    }

    /// Parse a fixed offset timezone string like "+05:30" or "-08:00"
    fn parse_fixed_offset(tz_str: &str) -> Option<chrono::FixedOffset> {
        if tz_str.len() < 3 {
            return None;
        }

        let sign = match tz_str.chars().next()? {
            '+' => 1,
            '-' => -1,
            _ => return None,
        };

        let rest = &tz_str[1..];
        let parts: Vec<&str> = rest.split(':').collect();

        let (hours, minutes) = match parts.len() {
            1 => {
                // Format: "+05" or "-08"
                let h = parts[0].parse::<i32>().ok()?;
                (h, 0)
            }
            2 => {
                // Format: "+05:30" or "-08:00"
                let h = parts[0].parse::<i32>().ok()?;
                let m = parts[1].parse::<i32>().ok()?;
                (h, m)
            }
            _ => return None,
        };

        let total_seconds = sign * (hours * 3600 + minutes * 60);
        chrono::FixedOffset::east_opt(total_seconds)
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
        ftx: &crate::FunctionContext,
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        let dt = if ftx.args.is_empty() {
            this.with_timezone(&chrono::Utc).fixed_offset()
        } else {
            let tz_str = ftx.resolve(ftx.args[0].clone())?;
            let tz_str = match tz_str {
                Value::String(s) => s,
                _ => return Err(ftx.error("timezone must be a string")),
            };
            parse_timezone(&tz_str, this)
                .ok_or_else(|| ftx.error(format!("invalid timezone: {}", tz_str)))?
        };
        Ok((dt.day0() as i32).into())
    }

    pub fn timestamp_date(
        ftx: &crate::FunctionContext,
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        let dt = if ftx.args.is_empty() {
            this.with_timezone(&chrono::Utc).fixed_offset()
        } else {
            let tz_str = ftx.resolve(ftx.args[0].clone())?;
            let tz_str = match tz_str {
                Value::String(s) => s,
                _ => return Err(ftx.error("timezone must be a string")),
            };
            parse_timezone(&tz_str, this)
                .ok_or_else(|| ftx.error(format!("invalid timezone: {}", tz_str)))?
        };
        Ok((dt.day() as i32).into())
    }

    pub fn timestamp_weekday(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.weekday().num_days_from_sunday() as i32).into())
    }

    pub fn timestamp_hours(
        ftx: &crate::FunctionContext,
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        let dt = if ftx.args.is_empty() {
            this.with_timezone(&chrono::Utc).fixed_offset()
        } else {
            let tz_str = ftx.resolve(ftx.args[0].clone())?;
            let tz_str = match tz_str {
                Value::String(s) => s,
                _ => return Err(ftx.error("timezone must be a string")),
            };
            parse_timezone(&tz_str, this)
                .ok_or_else(|| ftx.error(format!("invalid timezone: {}", tz_str)))?
        };
        Ok((dt.hour() as i32).into())
    }

    pub fn timestamp_minutes(
        ftx: &crate::FunctionContext,
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        let dt = if ftx.args.is_empty() {
            this.with_timezone(&chrono::Utc).fixed_offset()
        } else {
            let tz_str = ftx.resolve(ftx.args[0].clone())?;
            let tz_str = match tz_str {
                Value::String(s) => s,
                _ => return Err(ftx.error("timezone must be a string")),
            };
            parse_timezone(&tz_str, this)
                .ok_or_else(|| ftx.error(format!("invalid timezone: {}", tz_str)))?
        };
        Ok((dt.minute() as i32).into())
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

/// Returns true if the target value is NaN (Not a Number).
///
/// This function checks if a floating-point value is NaN. For non-float values,
/// it returns false.
///
/// # Examples
/// ```cel
/// isNaN(0.0 / 0.0) == true
/// isNaN(1.0 / 0.0) == false
/// isNaN(1.0) == false
/// ```
pub fn is_nan(This(this): This<Value>) -> Result<bool> {
    Ok(match this {
        Value::Float(v) => v.is_nan(),
        _ => false,
    })
}

/// Returns true if the target value is infinite (positive or negative infinity).
///
/// This function checks if a floating-point value is infinite. For non-float values,
/// it returns false.
///
/// # Examples
/// ```cel
/// isInf(1.0 / 0.0) == true
/// isInf(-1.0 / 0.0) == true
/// isInf(1.0) == false
/// ```
pub fn is_inf(This(this): This<Value>) -> Result<bool> {
    Ok(match this {
        Value::Float(v) => v.is_infinite(),
        _ => false,
    })
}

/// Returns true if the target value is finite (not NaN and not infinite).
///
/// This function checks if a value is finite. For integer types, always returns true.
/// For floating-point values, returns true only if the value is neither NaN nor infinite.
///
/// # Examples
/// ```cel
/// isFinite(1.0) == true
/// isFinite(1.0 / 0.0) == false
/// isFinite(0.0 / 0.0) == false
/// isFinite(42) == true
/// ```
pub fn is_finite(This(this): This<Value>) -> Result<bool> {
    Ok(match this {
        Value::Float(v) => v.is_finite(),
        Value::Int(_) | Value::UInt(_) => true,
        _ => false,
    })
}

/// Returns the ceiling of the target value (rounds up to the nearest integer).
///
/// For float values, returns the smallest integer greater than or equal to the value.
/// For integer values, returns the value unchanged.
///
/// # Examples
/// ```cel
/// ceil(1.2) == 2.0
/// ceil(-1.2) == -1.0
/// ceil(5) == 5.0
/// ```
pub fn ceil(This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::Float(v) => Value::Float(v.ceil()),
        Value::Int(v) => Value::Float(v as f64),
        Value::UInt(v) => Value::Float(v as f64),
        _ => return Err(ExecutionError::function_error("ceil", "argument must be numeric")),
    })
}

/// Returns the floor of the target value (rounds down to the nearest integer).
///
/// For float values, returns the largest integer less than or equal to the value.
/// For integer values, returns the value unchanged.
///
/// # Examples
/// ```cel
/// floor(1.8) == 1.0
/// floor(-1.2) == -2.0
/// floor(5) == 5.0
/// ```
pub fn floor(This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::Float(v) => Value::Float(v.floor()),
        Value::Int(v) => Value::Float(v as f64),
        Value::UInt(v) => Value::Float(v as f64),
        _ => return Err(ExecutionError::function_error("floor", "argument must be numeric")),
    })
}

/// Truncates the target value toward zero (removes the fractional part).
///
/// For float values, returns the integer part by removing the fractional component.
/// For integer values, returns the value unchanged.
///
/// # Examples
/// ```cel
/// trunc(1.8) == 1.0
/// trunc(-1.8) == -1.0
/// trunc(5) == 5.0
/// ```
pub fn trunc(This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::Float(v) => Value::Float(v.trunc()),
        Value::Int(v) => Value::Float(v as f64),
        Value::UInt(v) => Value::Float(v as f64),
        _ => return Err(ExecutionError::function_error("trunc", "argument must be numeric")),
    })
}

/// Rounds the target value to the nearest integer.
///
/// For float values, returns the nearest integer using "round half away from zero" rounding.
/// For integer values, returns the value unchanged.
///
/// # Examples
/// ```cel
/// round(1.4) == 1.0
/// round(1.5) == 2.0
/// round(-1.5) == -2.0
/// round(5) == 5.0
/// ```
pub fn round(This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::Float(v) => Value::Float(v.round()),
        Value::Int(v) => Value::Float(v as f64),
        Value::UInt(v) => Value::Float(v as f64),
        _ => return Err(ExecutionError::function_error("round", "argument must be numeric")),
    })
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

/// Converts an integer value to an enum type with range validation.
///
/// This function validates that the integer value is within the valid range
/// defined by the enum type's min and max values. If the value is out of range,
/// it returns an error.
///
/// # Arguments
/// * `ftx` - Function context
/// * `enum_type` - The enum type definition containing min/max range
/// * `value` - The integer value to convert
///
/// # Returns
/// * `Ok(Value::Int(value))` if the value is within range
/// * `Err(ExecutionError)` if the value is out of range
pub fn convert_int_to_enum(
    ftx: &FunctionContext,
    enum_type: Arc<crate::objects::EnumType>,
    value: i64,
) -> Result<Value> {
    // Convert i64 to i32 for range checking
    let value_i32 = value.try_into().map_err(|_| {
        ftx.error(format!(
            "value {} out of range for enum type '{}'",
            value, enum_type.type_name
        ))
    })?;

    if !enum_type.is_valid_value(value_i32) {
        return Err(ftx.error(format!(
            "value {} out of range for enum type '{}' (valid range: {}..{})",
            value, enum_type.type_name, enum_type.min_value, enum_type.max_value
        )));
    }

    Ok(Value::Int(value))
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::tests::test_script;
    use crate::ExecutionError;

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
            (
                "timestamp getDate with timezone",
                "timestamp('2023-05-28T23:00:00Z').getDate('+01:00') == 29",
            ),
            (
                "timestamp getDayOfMonth with timezone",
                "timestamp('2023-05-28T23:00:00Z').getDayOfMonth('+01:00') == 28",
            ),
            (
                "timestamp getHours with timezone",
                "timestamp('2023-05-28T23:00:00Z').getHours('+01:00') == 0",
            ),
            (
                "timestamp getMinutes with timezone",
                "timestamp('2023-05-28T23:45:00Z').getMinutes('+01:00') == 45",
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
            ("nan", "double('NaN').string() == 'NaN'"),
            ("inf", "double('inf') == double('inf')"),
            ("-inf", "double('-inf') < 0.0"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_float() {
        [
            ("string", "'10'.float() == 10.0"),
            ("int", "10.float() == 10.0"),
            ("double", "10.0.float() == 10.0"),
            ("nan", "float('NaN').string() == 'NaN'"),
            ("inf", "float('inf') == float('inf')"),
            ("-inf", "float('-inf') < 0.0"),
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
    fn test_list() {
        [
            ("list", "[1, 2, 3].list() == [1, 2, 3]"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_map() {
        [
            ("map", "{'a': 1, 'b': 2}.map() == {'a': 1, 'b': 2}"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_null_type() {
        [
            ("null", "null.null_type() == null"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_dyn() {
        [
            ("int", "10.dyn() == 10"),
            ("string", "'hello'.dyn() == 'hello'"),
            ("list", "[1, 2, 3].dyn() == [1, 2, 3]"),
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

    #[test]
    fn test_enum_conversion_valid_range() {
        use crate::objects::EnumType;
        use std::sync::Arc;

        // Create an enum type with range 0..2 (e.g., proto enum with values 0, 1, 2)
        let enum_type = Arc::new(EnumType::new("test.TestEnum".to_string(), 0, 2));

        let mut context = Context::default();
        context.add_function("toTestEnum", {
            let enum_type = enum_type.clone();
            move |ftx: &crate::FunctionContext, value: i64| -> crate::functions::Result<crate::Value> {
                super::convert_int_to_enum(ftx, enum_type.clone(), value)
            }
        });

        // Valid conversions within range
        let program = crate::Program::compile("toTestEnum(0) == 0").unwrap();
        assert_eq!(program.execute(&context).unwrap(), true.into());

        let program = crate::Program::compile("toTestEnum(1) == 1").unwrap();
        assert_eq!(program.execute(&context).unwrap(), true.into());

        let program = crate::Program::compile("toTestEnum(2) == 2").unwrap();
        assert_eq!(program.execute(&context).unwrap(), true.into());
    }

    #[test]
    fn test_enum_conversion_too_big() {
        use crate::objects::EnumType;
        use std::sync::Arc;

        // Create an enum type with range 0..2
        let enum_type = Arc::new(EnumType::new("test.TestEnum".to_string(), 0, 2));

        let mut context = Context::default();
        context.add_function("toTestEnum", {
            let enum_type = enum_type.clone();
            move |ftx: &crate::FunctionContext, value: i64| -> crate::functions::Result<crate::Value> {
                super::convert_int_to_enum(ftx, enum_type.clone(), value)
            }
        });

        // Invalid conversion - value too large
        let program = crate::Program::compile("toTestEnum(100)").unwrap();
        let result = program.execute(&context);
        assert!(result.is_err(), "Should error on value too large");
        assert!(result.unwrap_err().to_string().contains("out of range"));
    }

    #[test]
    fn test_enum_conversion_too_negative() {
        use crate::objects::EnumType;
        use std::sync::Arc;

        // Create an enum type with range 0..2
        let enum_type = Arc::new(EnumType::new("test.TestEnum".to_string(), 0, 2));

        let mut context = Context::default();
        context.add_function("toTestEnum", {
            let enum_type = enum_type.clone();
            move |ftx: &crate::FunctionContext, value: i64| -> crate::functions::Result<crate::Value> {
                super::convert_int_to_enum(ftx, enum_type.clone(), value)
            }
        });

        // Invalid conversion - value too negative
        let program = crate::Program::compile("toTestEnum(-10)").unwrap();
        let result = program.execute(&context);
        assert!(result.is_err(), "Should error on value too negative");
        assert!(result.unwrap_err().to_string().contains("out of range"));
    }

    #[test]
    fn test_enum_conversion_negative_range() {
        use crate::objects::EnumType;
        use std::sync::Arc;

        // Create an enum type with negative range -2..2
        let enum_type = Arc::new(EnumType::new("test.SignedEnum".to_string(), -2, 2));

        let mut context = Context::default();
        context.add_function("toSignedEnum", {
            let enum_type = enum_type.clone();
            move |ftx: &crate::FunctionContext, value: i64| -> crate::functions::Result<crate::Value> {
                super::convert_int_to_enum(ftx, enum_type.clone(), value)
            }
        });

        // Valid negative values
        let program = crate::Program::compile("toSignedEnum(-2) == -2").unwrap();
        assert_eq!(program.execute(&context).unwrap(), true.into());

        let program = crate::Program::compile("toSignedEnum(-1) == -1").unwrap();
        assert_eq!(program.execute(&context).unwrap(), true.into());

        // Invalid - too negative
        let program = crate::Program::compile("toSignedEnum(-3)").unwrap();
        let result = program.execute(&context);
        assert!(result.is_err(), "Should error on value too negative");

        // Invalid - too positive
        let program = crate::Program::compile("toSignedEnum(3)").unwrap();
        let result = program.execute(&context);
        assert!(result.is_err(), "Should error on value too large");
    }

    #[test]
    fn test_has_in_ternary() {
        // Conformance test: presence_test_with_ternary variants

        // Variant 1: has() as condition (present case)
        let result1 = test_script("has({'a': 1}.a) ? 'present' : 'absent'", None);
        assert_eq!(result1, Ok("present".into()), "presence_test_with_ternary_1");

        // Variant 2: has() as condition (absent case)
        let result2 = test_script("has({'a': 1}.b) ? 'present' : 'absent'", None);
        assert_eq!(result2, Ok("absent".into()), "presence_test_with_ternary_2");

        // Variant 3: has() in true branch
        let result3 = test_script("true ? has({'a': 1}.a) : false", None);
        assert_eq!(result3, Ok(true.into()), "presence_test_with_ternary_3");

        // Variant 4: has() in false branch
        let result4 = test_script("false ? true : has({'a': 1}.a)", None);
        assert_eq!(result4, Ok(true.into()), "presence_test_with_ternary_4");
    }

    #[test]
    fn test_list_elem_type_exhaustive() {
        // Conformance test: list_elem_type_exhaustive
        // Test heterogeneous list with all() macro - should give proper error message
        let script = "[1, 'foo', 3].all(e, e % 2 == 1)";
        let result = test_script(script, None);

        // This should produce an error when trying e % 2 on string
        // The error should indicate the type mismatch
        match result {
            Err(ExecutionError::UnsupportedBinaryOperator(op, left, right)) => {
                assert_eq!(op, "rem", "Expected 'rem' operator");
                assert!(matches!(left, crate::objects::Value::String(_)),
                    "Expected String on left side");
                assert!(matches!(right, crate::objects::Value::Int(_)),
                    "Expected Int on right side");
            }
            other => {
                panic!("Expected UnsupportedBinaryOperator error, got: {:?}", other);
            }
        }
    }

    #[test]
    fn test_is_nan() {
        [
            ("isNaN with NaN", "isNaN(0.0 / 0.0) == true"),
            ("isNaN with infinity", "isNaN(1.0 / 0.0) == false"),
            ("isNaN with normal float", "isNaN(1.0) == false"),
            ("isNaN with int", "isNaN(42) == false"),
            ("isNaN method call", "(0.0 / 0.0).isNaN() == true"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_is_inf() {
        [
            ("isInf with positive infinity", "isInf(1.0 / 0.0) == true"),
            ("isInf with negative infinity", "isInf(-1.0 / 0.0) == true"),
            ("isInf with normal float", "isInf(1.0) == false"),
            ("isInf with NaN", "isInf(0.0 / 0.0) == false"),
            ("isInf with int", "isInf(42) == false"),
            ("isInf method call", "(1.0 / 0.0).isInf() == true"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_is_finite() {
        [
            ("isFinite with normal float", "isFinite(1.0) == true"),
            ("isFinite with int", "isFinite(42) == true"),
            ("isFinite with uint", "isFinite(42u) == true"),
            ("isFinite with infinity", "isFinite(1.0 / 0.0) == false"),
            ("isFinite with NaN", "isFinite(0.0 / 0.0) == false"),
            ("isFinite method call", "1.0.isFinite() == true"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_ceil() {
        [
            ("ceil positive decimal", "ceil(1.2) == 2.0"),
            ("ceil negative decimal", "ceil(-1.2) == -1.0"),
            ("ceil int", "ceil(5) == 5.0"),
            ("ceil uint", "ceil(5u) == 5.0"),
            ("ceil whole number", "ceil(3.0) == 3.0"),
            ("ceil method call", "1.2.ceil() == 2.0"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_floor() {
        [
            ("floor positive decimal", "floor(1.8) == 1.0"),
            ("floor negative decimal", "floor(-1.2) == -2.0"),
            ("floor int", "floor(5) == 5.0"),
            ("floor uint", "floor(5u) == 5.0"),
            ("floor whole number", "floor(3.0) == 3.0"),
            ("floor method call", "1.8.floor() == 1.0"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_trunc() {
        [
            ("trunc positive decimal", "trunc(1.8) == 1.0"),
            ("trunc negative decimal", "trunc(-1.8) == -1.0"),
            ("trunc int", "trunc(5) == 5.0"),
            ("trunc uint", "trunc(5u) == 5.0"),
            ("trunc whole number", "trunc(3.0) == 3.0"),
            ("trunc method call", "1.8.trunc() == 1.0"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_round() {
        [
            ("round 1.4", "round(1.4) == 1.0"),
            ("round 1.5", "round(1.5) == 2.0"),
            ("round -1.5", "round(-1.5) == -2.0"),
            ("round int", "round(5) == 5.0"),
            ("round uint", "round(5u) == 5.0"),
            ("round whole number", "round(3.0) == 3.0"),
            ("round method call", "1.5.round() == 2.0"),
        ]
        .iter()
        .for_each(assert_script);
    }
}
