use crate::objects::Value;
use std::collections::HashMap;
use std::sync::Arc;

/// ExtensionDescriptor describes a protocol buffer extension field.
#[derive(Clone, Debug)]
pub struct ExtensionDescriptor {
    /// The fully-qualified name of the extension field (e.g., "pkg.my_extension")
    pub name: String,
    /// The message type this extension extends (e.g., "pkg.MyMessage")
    pub extendee: String,
    /// The number/tag of the extension field
    pub number: i32,
    /// Whether this is a package-scoped extension (true) or message-scoped (false)
    pub is_package_scoped: bool,
}

/// ExtensionRegistry stores registered protobuf extension fields.
/// Extensions can be:
/// - Package-scoped: defined at package level, accessed as `msg.ext_name`
/// - Message-scoped: defined within a message, accessed as `msg.MessageType.ext_name`
#[derive(Clone, Debug, Default)]
pub struct ExtensionRegistry {
    /// Maps fully-qualified extension names to their descriptors
    extensions: HashMap<String, ExtensionDescriptor>,
    /// Maps message type names to their extension field values
    /// Key format: "message_type_name:extension_name"
    extension_values: HashMap<String, HashMap<String, Value>>,
}

impl ExtensionRegistry {
    pub fn new() -> Self {
        Self {
            extensions: HashMap::new(),
            extension_values: HashMap::new(),
        }
    }

    /// Registers a new extension field descriptor
    pub fn register_extension(&mut self, descriptor: ExtensionDescriptor) {
        self.extensions.insert(descriptor.name.clone(), descriptor);
    }

    /// Sets an extension field value for a specific message instance
    pub fn set_extension_value(&mut self, message_type: &str, ext_name: &str, value: Value) {
        let key = format!("{}:{}", message_type, ext_name);
        self.extension_values
            .entry(key)
            .or_insert_with(HashMap::new)
            .insert(ext_name.to_string(), value);
    }

    /// Gets an extension field value for a specific message
    pub fn get_extension_value(&self, message_type: &str, ext_name: &str) -> Option<&Value> {
        // Try direct lookup first
        if let Some(values) = self.extension_values.get(&format!("{}:{}", message_type, ext_name)) {
            if let Some(value) = values.get(ext_name) {
                return Some(value);
            }
        }

        // Try matching by extension name across all message types
        for ((stored_type, stored_ext), values) in &self.extension_values {
            if stored_ext == ext_name {
                // Check if the extension is registered for this message type
                if let Some(descriptor) = self.extensions.get(ext_name) {
                    if &descriptor.extendee == message_type || stored_type == message_type {
                        return values.get(ext_name);
                    }
                }
            }
        }

        None
    }

    /// Checks if an extension is registered
    pub fn has_extension(&self, ext_name: &str) -> bool {
        self.extensions.contains_key(ext_name)
    }

    /// Gets an extension descriptor by name
    pub fn get_extension(&self, ext_name: &str) -> Option<&ExtensionDescriptor> {
        self.extensions.get(ext_name)
    }

    /// Resolves an extension field access
    /// Handles both package-scoped (pkg.ext) and message-scoped (MessageType.ext) syntax
    pub fn resolve_extension(&self, message_type: &str, field_name: &str) -> Option<Value> {
        // Check if field_name contains a dot, indicating scoped access
        if field_name.contains('.') {
            // This might be pkg.ext or MessageType.ext syntax
            if let Some(value) = self.get_extension_value(message_type, field_name) {
                return Some(value.clone());
            }
        }

        // Try simple field name lookup
        if let Some(value) = self.get_extension_value(message_type, field_name) {
            return Some(value.clone());
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extension_registry() {
        let mut registry = ExtensionRegistry::new();

        // Register a package-scoped extension
        registry.register_extension(ExtensionDescriptor {
            name: "com.example.my_extension".to_string(),
            extendee: "com.example.MyMessage".to_string(),
            number: 1000,
            is_package_scoped: true,
        });

        assert!(registry.has_extension("com.example.my_extension"));

        // Set an extension value
        registry.set_extension_value(
            "com.example.MyMessage",
            "com.example.my_extension",
            Value::Int(42),
        );

        // Retrieve the extension value
        let value = registry.get_extension_value("com.example.MyMessage", "com.example.my_extension");
        assert_eq!(value, Some(&Value::Int(42)));
    }

    #[test]
    fn test_message_scoped_extension() {
        let mut registry = ExtensionRegistry::new();

        // Register a message-scoped extension
        registry.register_extension(ExtensionDescriptor {
            name: "NestedMessage.nested_ext".to_string(),
            extendee: "com.example.MyMessage".to_string(),
            number: 2000,
            is_package_scoped: false,
        });

        registry.set_extension_value(
            "com.example.MyMessage",
            "NestedMessage.nested_ext",
            Value::String(Arc::new("test".to_string())),
        );

        let value = registry.resolve_extension("com.example.MyMessage", "NestedMessage.nested_ext");
        assert_eq!(
            value,
            Some(Value::String(Arc::new("test".to_string())))
        );
    }
}
