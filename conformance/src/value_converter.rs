use cel::objects::Value as CelValue;
use prost_types::Any;
use std::collections::HashMap;
use std::sync::Arc;

use crate::proto::cel::expr::Value as ProtoValue;

/// Converts a CEL spec protobuf Value to a cel-rust Value
pub fn proto_value_to_cel_value(proto_value: &ProtoValue) -> Result<CelValue, ConversionError> {
    use cel::objects::{Key, Map, Value::*};
    use std::sync::Arc;

    match proto_value.kind.as_ref() {
        Some(crate::proto::cel::expr::value::Kind::NullValue(_)) => Ok(Null),
        Some(crate::proto::cel::expr::value::Kind::BoolValue(v)) => Ok(Bool(*v)),
        Some(crate::proto::cel::expr::value::Kind::Int64Value(v)) => Ok(Int(*v)),
        Some(crate::proto::cel::expr::value::Kind::Uint64Value(v)) => Ok(UInt(*v)),
        Some(crate::proto::cel::expr::value::Kind::DoubleValue(v)) => Ok(Float(*v)),
        Some(crate::proto::cel::expr::value::Kind::StringValue(v)) => {
            Ok(String(Arc::new(v.clone())))
        }
        Some(crate::proto::cel::expr::value::Kind::BytesValue(v)) => {
            Ok(Bytes(Arc::new(v.to_vec())))
        }
        Some(crate::proto::cel::expr::value::Kind::ListValue(list)) => {
            let mut values = Vec::new();
            for item in &list.values {
                values.push(proto_value_to_cel_value(item)?);
            }
            Ok(List(Arc::new(values)))
        }
        Some(crate::proto::cel::expr::value::Kind::MapValue(map)) => {
            let mut entries = HashMap::new();
            for entry in &map.entries {
                let key_proto = entry.key.as_ref().ok_or(ConversionError::MissingKey)?;
                let key_cel = proto_value_to_cel_value(key_proto)?;
                let value = proto_value_to_cel_value(
                    entry.value.as_ref().ok_or(ConversionError::MissingValue)?,
                )?;

                // Convert key to Key enum
                let key = match key_cel {
                    Int(i) => Key::Int(i),
                    UInt(u) => Key::Uint(u),
                    String(s) => Key::String(s),
                    Bool(b) => Key::Bool(b),
                    _ => return Err(ConversionError::UnsupportedKeyType),
                };
                entries.insert(key, value);
            }
            Ok(Map(Map {
                map: Arc::new(entries),
            }))
        }
        Some(crate::proto::cel::expr::value::Kind::EnumValue(_)) => {
            Err(ConversionError::Unsupported("enum values".to_string()))
        }
        Some(crate::proto::cel::expr::value::Kind::ObjectValue(any)) => {
            convert_any_to_cel_value(any)
        }
        Some(crate::proto::cel::expr::value::Kind::TypeValue(v)) => {
            // TypeValue is a string representing a type name
            Ok(String(Arc::new(v.clone())))
        }
        None => Err(ConversionError::EmptyValue),
    }
}

/// Converts a google.protobuf.Any message to a CEL value.
/// Handles wrapper types and converts other messages to Structs.
fn convert_any_to_cel_value(any: &Any) -> Result<CelValue, ConversionError> {
    use cel::objects::Value::*;

    // Try to decode as wrapper types first
    // Wrapper types should be unwrapped to their inner value
    let type_url = &any.type_url;

    // Wrapper types in protobuf are simple: they have a single field named "value"
    // We can manually decode them from the wire format
    // Wire format: field_number (1 byte varint) + wire_type + value

    // Helper to decode a varint
    fn decode_varint(bytes: &[u8]) -> Option<(u64, usize)> {
        let mut result = 0u64;
        let mut shift = 0;
        for (i, &byte) in bytes.iter().enumerate() {
            result |= ((byte & 0x7F) as u64) << shift;
            if (byte & 0x80) == 0 {
                return Some((result, i + 1));
            }
            shift += 7;
            if shift >= 64 {
                return None;
            }
        }
        None
    }

    // Helper to decode a fixed64 (double)
    fn decode_fixed64(bytes: &[u8]) -> Option<f64> {
        if bytes.len() < 8 {
            return None;
        }
        let mut buf = [0u8; 8];
        buf.copy_from_slice(&bytes[0..8]);
        Some(f64::from_le_bytes(buf))
    }

    // Helper to decode a fixed32 (float)
    fn decode_fixed32(bytes: &[u8]) -> Option<f32> {
        if bytes.len() < 4 {
            return None;
        }
        let mut buf = [0u8; 4];
        buf.copy_from_slice(&bytes[0..4]);
        Some(f32::from_le_bytes(buf))
    }

    // Helper to decode a length-delimited string
    fn decode_string(bytes: &[u8]) -> Option<(std::string::String, usize)> {
        if let Some((len, len_bytes)) = decode_varint(bytes) {
            let len = len as usize;
            if bytes.len() >= len_bytes + len {
                if let Ok(s) =
                    std::string::String::from_utf8(bytes[len_bytes..len_bytes + len].to_vec())
                {
                    return Some((s, len_bytes + len));
                }
            }
        }
        None
    }

    // Decode wrapper types - they all have field number 1 with the value
    if type_url.contains("google.protobuf.BoolValue") {
        // Field 1: bool value (wire type 0 = varint)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x08 {
                // field 1, wire type 0
                if let Some((bool_val, _)) = decode_varint(&any.value[1..]) {
                    return Ok(Bool(bool_val != 0));
                }
            }
        }
    } else if type_url.contains("google.protobuf.BytesValue") {
        // Field 1: bytes value (wire type 2 = length-delimited)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x0A {
                // field 1, wire type 2
                if let Some((len, len_bytes)) = decode_varint(&any.value[1..]) {
                    let len = len as usize;
                    if any.value.len() >= 1 + len_bytes + len {
                        let bytes = any.value[1 + len_bytes..1 + len_bytes + len].to_vec();
                        return Ok(Bytes(Arc::new(bytes)));
                    }
                }
            }
        }
    } else if type_url.contains("google.protobuf.DoubleValue") {
        // Field 1: double value (wire type 1 = fixed64)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x09 {
                // field 1, wire type 1
                if let Some(val) = decode_fixed64(&any.value[1..]) {
                    return Ok(Float(val));
                }
            }
        }
    } else if type_url.contains("google.protobuf.FloatValue") {
        // Field 1: float value (wire type 5 = fixed32)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x0D {
                // field 1, wire type 5
                if let Some(val) = decode_fixed32(&any.value[1..]) {
                    return Ok(Float(val as f64));
                }
            }
        }
    } else if type_url.contains("google.protobuf.Int32Value") {
        // Field 1: int32 value (wire type 0 = varint, signed but not zigzag)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x08 {
                // field 1, wire type 0
                if let Some((val, _)) = decode_varint(&any.value[1..]) {
                    // Convert to signed i32 (two's complement)
                    let val = val as i32;
                    return Ok(Int(val as i64));
                }
            }
        }
    } else if type_url.contains("google.protobuf.Int64Value") {
        // Field 1: int64 value (wire type 0 = varint, signed but not zigzag)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x08 {
                // field 1, wire type 0
                if let Some((val, _)) = decode_varint(&any.value[1..]) {
                    // Convert to signed i64 (two's complement)
                    let val = val as i64;
                    return Ok(Int(val));
                }
            }
        }
    } else if type_url.contains("google.protobuf.StringValue") {
        // Field 1: string value (wire type 2 = length-delimited)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x0A {
                // field 1, wire type 2
                if let Some((s, _)) = decode_string(&any.value[1..]) {
                    return Ok(String(Arc::new(s)));
                }
            }
        }
    } else if type_url.contains("google.protobuf.UInt32Value") {
        // Field 1: uint32 value (wire type 0 = varint)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x08 {
                // field 1, wire type 0
                if let Some((val, _)) = decode_varint(&any.value[1..]) {
                    return Ok(UInt(val));
                }
            }
        }
    } else if type_url.contains("google.protobuf.UInt64Value") {
        // Field 1: uint64 value (wire type 0 = varint)
        if let Some((field_and_type, _)) = decode_varint(&any.value) {
            if field_and_type == 0x08 {
                // field 1, wire type 0
                if let Some((val, _)) = decode_varint(&any.value[1..]) {
                    return Ok(UInt(val));
                }
            }
        }
    }

    // For other proto messages, try to decode them and convert to Struct
    // Extract the type name from the type_url (format: type.googleapis.com/packagename.MessageName)
    let type_name = if let Some(last_slash) = type_url.rfind('/') {
        &type_url[last_slash + 1..]
    } else {
        type_url
    };

    // Try to decode as TestAllTypes (proto2 or proto3)
    use prost::Message;
    if type_url.contains("cel.expr.conformance.proto3.TestAllTypes") {
        if let Ok(msg) =
            crate::proto::cel::expr::conformance::proto3::TestAllTypes::decode(&any.value[..])
        {
            return convert_test_all_types_proto3_to_struct(&msg);
        }
    } else if type_url.contains("cel.expr.conformance.proto2.TestAllTypes") {
        if let Ok(msg) =
            crate::proto::cel::expr::conformance::proto2::TestAllTypes::decode(&any.value[..])
        {
            return convert_test_all_types_proto2_to_struct(&msg);
        }
    }

    // For other proto messages, return an error for now
    // We can extend this to handle more message types as needed
    Err(ConversionError::Unsupported(format!(
        "proto message type: {} (not yet supported)",
        type_name
    )))
}

/// Decode a wrapper type from bytes using wire format
fn decode_wrapper_from_bytes(bytes: &[u8], wrapper_type: &str) -> Option<CelValue> {
    use cel::objects::Value::*;
    use std::sync::Arc;

    // Helper to decode a varint
    fn decode_varint(bytes: &[u8]) -> Option<(u64, usize)> {
        let mut result = 0u64;
        let mut shift = 0;
        for (i, &byte) in bytes.iter().enumerate() {
            result |= ((byte & 0x7F) as u64) << shift;
            if (byte & 0x80) == 0 {
                return Some((result, i + 1));
            }
            shift += 7;
            if shift >= 64 {
                return None;
            }
        }
        None
    }

    // Helper to decode a fixed64 (double)
    fn decode_fixed64(bytes: &[u8]) -> Option<f64> {
        if bytes.len() < 8 {
            return None;
        }
        let mut buf = [0u8; 8];
        buf.copy_from_slice(&bytes[0..8]);
        Some(f64::from_le_bytes(buf))
    }

    // Helper to decode a fixed32 (float)
    fn decode_fixed32(bytes: &[u8]) -> Option<f32> {
        if bytes.len() < 4 {
            return None;
        }
        let mut buf = [0u8; 4];
        buf.copy_from_slice(&bytes[0..4]);
        Some(f32::from_le_bytes(buf))
    }

    // Helper to decode a length-delimited string
    fn decode_string(bytes: &[u8]) -> Option<(std::string::String, usize)> {
        if let Some((len, len_bytes)) = decode_varint(bytes) {
            let len = len as usize;
            if bytes.len() >= len_bytes + len {
                if let Ok(s) =
                    std::string::String::from_utf8(bytes[len_bytes..len_bytes + len].to_vec())
                {
                    return Some((s, len_bytes + len));
                }
            }
        }
        None
    }

    // Decode based on wrapper type
    if wrapper_type == "BoolValue" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x08 {
                if let Some((bool_val, _)) = decode_varint(&bytes[1..]) {
                    return Some(Bool(bool_val != 0));
                }
            }
        }
    } else if wrapper_type == "BytesValue" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x0A {
                if let Some((len, len_bytes)) = decode_varint(&bytes[1..]) {
                    let len = len as usize;
                    if bytes.len() >= 1 + len_bytes + len {
                        let bytes = bytes[1 + len_bytes..1 + len_bytes + len].to_vec();
                        return Some(Bytes(Arc::new(bytes)));
                    }
                }
            }
        }
    } else if wrapper_type == "DoubleValue" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x09 {
                if let Some(val) = decode_fixed64(&bytes[1..]) {
                    return Some(Float(val));
                }
            }
        }
    } else if wrapper_type == "FloatValue" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x0D {
                if let Some(val) = decode_fixed32(&bytes[1..]) {
                    return Some(Float(val as f64));
                }
            }
        }
    } else if wrapper_type == "Int32Value" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x08 {
                if let Some((val, _)) = decode_varint(&bytes[1..]) {
                    return Some(Int(val as i32 as i64));
                }
            }
        }
    } else if wrapper_type == "Int64Value" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x08 {
                if let Some((val, _)) = decode_varint(&bytes[1..]) {
                    return Some(Int(val as i64));
                }
            }
        }
    } else if wrapper_type == "StringValue" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x0A {
                if let Some((s, _)) = decode_string(&bytes[1..]) {
                    return Some(String(Arc::new(s)));
                }
            }
        }
    } else if wrapper_type == "UInt32Value" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x08 {
                if let Some((val, _)) = decode_varint(&bytes[1..]) {
                    return Some(UInt(val));
                }
            }
        }
    } else if wrapper_type == "UInt64Value" {
        if let Some((field_and_type, _)) = decode_varint(bytes) {
            if field_and_type == 0x08 {
                if let Some((val, _)) = decode_varint(&bytes[1..]) {
                    return Some(UInt(val));
                }
            }
        }
    }

    None
}

/// Convert a proto3 TestAllTypes message to a CEL Struct
fn convert_test_all_types_proto3_to_struct(
    msg: &crate::proto::cel::expr::conformance::proto3::TestAllTypes,
) -> Result<CelValue, ConversionError> {
    use cel::objects::{Struct, Value::*};
    use std::sync::Arc;

    let mut fields = HashMap::new();

    // Wrapper types are already decoded by prost - just convert them to CEL values
    if let Some(val) = msg.single_bool_wrapper {
        fields.insert("single_bool_wrapper".to_string(), Bool(val));
    }
    if let Some(ref val) = msg.single_bytes_wrapper {
        fields.insert(
            "single_bytes_wrapper".to_string(),
            Bytes(Arc::new(val.clone())),
        );
    }
    if let Some(val) = msg.single_double_wrapper {
        fields.insert("single_double_wrapper".to_string(), Float(val));
    }
    if let Some(val) = msg.single_float_wrapper {
        fields.insert("single_float_wrapper".to_string(), Float(val as f64));
    }
    if let Some(val) = msg.single_int32_wrapper {
        fields.insert("single_int32_wrapper".to_string(), Int(val as i64));
    }
    if let Some(val) = msg.single_int64_wrapper {
        fields.insert("single_int64_wrapper".to_string(), Int(val));
    }
    if let Some(ref val) = msg.single_string_wrapper {
        fields.insert(
            "single_string_wrapper".to_string(),
            String(Arc::new(val.clone())),
        );
    }
    if let Some(val) = msg.single_uint32_wrapper {
        fields.insert("single_uint32_wrapper".to_string(), UInt(val as u64));
    }
    if let Some(val) = msg.single_uint64_wrapper {
        fields.insert("single_uint64_wrapper".to_string(), UInt(val));
    }

    // Add other fields
    fields.insert("single_bool".to_string(), Bool(msg.single_bool));
    fields.insert(
        "single_string".to_string(),
        String(Arc::new(msg.single_string.clone())),
    );
    fields.insert(
        "single_bytes".to_string(),
        Bytes(Arc::new(msg.single_bytes.as_ref().to_vec())),
    );
    fields.insert("single_int32".to_string(), Int(msg.single_int32 as i64));
    fields.insert("single_int64".to_string(), Int(msg.single_int64));
    fields.insert("single_uint32".to_string(), UInt(msg.single_uint32 as u64));
    fields.insert("single_uint64".to_string(), UInt(msg.single_uint64));
    fields.insert("single_float".to_string(), Float(msg.single_float as f64));
    fields.insert("single_double".to_string(), Float(msg.single_double));

    Ok(Struct(Struct {
        type_name: Arc::new("cel.expr.conformance.proto3.TestAllTypes".to_string()),
        fields: Arc::new(fields),
    }))
}

/// Convert a proto2 TestAllTypes message to a CEL Struct
fn convert_test_all_types_proto2_to_struct(
    msg: &crate::proto::cel::expr::conformance::proto2::TestAllTypes,
) -> Result<CelValue, ConversionError> {
    use cel::objects::{Struct, Value::*};
    use std::sync::Arc;

    let mut fields = HashMap::new();

    // Proto2 has optional fields, so we need to check if they're set
    // Wrapper types are already decoded by prost - just convert them to CEL values
    if let Some(val) = msg.single_bool_wrapper {
        fields.insert("single_bool_wrapper".to_string(), Bool(val));
    }
    if let Some(ref val) = msg.single_bytes_wrapper {
        fields.insert(
            "single_bytes_wrapper".to_string(),
            Bytes(Arc::new(val.clone())),
        );
    }
    if let Some(val) = msg.single_double_wrapper {
        fields.insert("single_double_wrapper".to_string(), Float(val));
    }
    if let Some(val) = msg.single_float_wrapper {
        fields.insert("single_float_wrapper".to_string(), Float(val as f64));
    }
    if let Some(val) = msg.single_int32_wrapper {
        fields.insert("single_int32_wrapper".to_string(), Int(val as i64));
    }
    if let Some(val) = msg.single_int64_wrapper {
        fields.insert("single_int64_wrapper".to_string(), Int(val));
    }
    if let Some(ref val) = msg.single_string_wrapper {
        fields.insert(
            "single_string_wrapper".to_string(),
            String(Arc::new(val.clone())),
        );
    }
    if let Some(val) = msg.single_uint32_wrapper {
        fields.insert("single_uint32_wrapper".to_string(), UInt(val as u64));
    }
    if let Some(val) = msg.single_uint64_wrapper {
        fields.insert("single_uint64_wrapper".to_string(), UInt(val));
    }

    // Add other fields (proto2 has defaults)
    fields.insert(
        "single_bool".to_string(),
        Bool(msg.single_bool.unwrap_or(true)),
    );
    if let Some(ref s) = msg.single_string {
        fields.insert("single_string".to_string(), String(Arc::new(s.clone())));
    }
    if let Some(ref b) = msg.single_bytes {
        fields.insert(
            "single_bytes".to_string(),
            Bytes(Arc::new(b.clone().into())),
        );
    }
    if let Some(i) = msg.single_int32 {
        fields.insert("single_int32".to_string(), Int(i as i64));
    }
    if let Some(i) = msg.single_int64 {
        fields.insert("single_int64".to_string(), Int(i));
    }
    if let Some(u) = msg.single_uint32 {
        fields.insert("single_uint32".to_string(), UInt(u as u64));
    }
    if let Some(u) = msg.single_uint64 {
        fields.insert("single_uint64".to_string(), UInt(u));
    }
    if let Some(f) = msg.single_float {
        fields.insert("single_float".to_string(), Float(f as f64));
    }
    if let Some(d) = msg.single_double {
        fields.insert("single_double".to_string(), Float(d));
    }

    Ok(Struct(Struct {
        type_name: Arc::new("cel.expr.conformance.proto2.TestAllTypes".to_string()),
        fields: Arc::new(fields),
    }))
}

#[derive(Debug, thiserror::Error)]
pub enum ConversionError {
    #[error("Missing key in map entry")]
    MissingKey,
    #[error("Missing value in map entry")]
    MissingValue,
    #[error("Unsupported key type for map")]
    UnsupportedKeyType,
    #[error("Unsupported value type: {0}")]
    Unsupported(String),
    #[error("Empty value")]
    EmptyValue,
}
