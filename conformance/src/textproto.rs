use prost::Message;
use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

/// Find protoc's well-known types include directory
fn find_protoc_include() -> Option<String> {
    // Try common locations for protoc's include directory
    // Prioritize Homebrew on macOS as it's most common
    let common_paths = [
        "/opt/homebrew/include", // macOS Homebrew (most common)
        "/usr/local/include",
        "/usr/include",
        "/usr/local/opt/protobuf/include", // macOS Homebrew protobuf
    ];

    for path in &common_paths {
        let well_known = std::path::Path::new(path).join("google").join("protobuf");
        // Verify wrappers.proto exists (needed for Int32Value, etc.)
        if well_known.join("wrappers.proto").exists() {
            return Some(path.to_string());
        }
    }

    // Try to get it from protoc binary location (for Homebrew)
    if let Ok(protoc_path) = which::which("protoc") {
        if let Some(bin_dir) = protoc_path.parent() {
            // Homebrew structure: /opt/homebrew/bin/protoc -> /opt/homebrew/include
            if let Some(brew_prefix) = bin_dir.parent() {
                let possible_include = brew_prefix.join("include");
                let well_known = possible_include.join("google").join("protobuf");
                if well_known.join("wrappers.proto").exists() {
                    return Some(possible_include.to_string_lossy().to_string());
                }
            }
        }
    }

    None
}

/// Build a descriptor set that includes all necessary proto files
fn build_descriptor_set(
    proto_files: &[&str],
    include_paths: &[&str],
) -> Result<tempfile::NamedTempFile, TextprotoParseError> {
    let descriptor_file = tempfile::NamedTempFile::new()?;
    let descriptor_path = descriptor_file.path().to_str().unwrap();

    let mut protoc_cmd = Command::new("protoc");
    protoc_cmd
        .arg("--descriptor_set_out")
        .arg(descriptor_path)
        .arg("--include_imports");

    // Add well-known types include path
    if let Some(well_known_include) = find_protoc_include() {
        protoc_cmd.arg("-I").arg(&well_known_include);
    }

    for include in include_paths {
        protoc_cmd.arg("-I").arg(include);
    }

    for proto_file in proto_files {
        protoc_cmd.arg(proto_file);
    }

    let output = protoc_cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(TextprotoParseError::ProtocError(format!(
            "Failed to build descriptor set: {}",
            stderr
        )));
    }

    Ok(descriptor_file)
}

/// Parse textproto using protoc to convert to binary format, then parse with prost
pub fn parse_textproto_to_prost<T: Message + Default>(
    text: &str,
    message_type: &str,
    proto_files: &[&str],
    include_paths: &[&str],
) -> Result<T, TextprotoParseError> {
    // Write textproto to a temporary file
    let mut textproto_file = NamedTempFile::new()?;
    textproto_file.write_all(text.as_bytes())?;

    // Build descriptor set (this helps with Any message resolution)
    let _descriptor_set = build_descriptor_set(proto_files, include_paths)?;

    // Use protoc to convert textproto to binary
    let mut protoc_cmd = Command::new("protoc");
    protoc_cmd.arg("--encode").arg(message_type);

    // Add well-known types include path
    if let Some(well_known_include) = find_protoc_include() {
        protoc_cmd.arg("-I").arg(&well_known_include);
    }

    for include in include_paths {
        protoc_cmd.arg("-I").arg(include);
    }

    for proto_file in proto_files {
        protoc_cmd.arg(proto_file);
    }

    let output = protoc_cmd
        .stdin(std::process::Stdio::from(textproto_file.reopen()?))
        .output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Check if this is an Any message resolution error
        if stderr.contains("Could not find type") && stderr.contains("google.protobuf.Any") {
            return Err(TextprotoParseError::AnyMessageUnsupported(
                "protoc --encode does not support Any messages with type URLs. This test file will be skipped.".to_string(),
            ));
        }
        return Err(TextprotoParseError::ProtocError(format!(
            "protoc failed: {}",
            stderr
        )));
    }

    // Parse the binary output with prost
    let message = T::decode(&output.stdout[..])?;
    Ok(message)
}

#[derive(Debug, thiserror::Error)]
pub enum TextprotoParseError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Protoc error: {0}")]
    ProtocError(String),
    #[error("Any message unsupported: {0}")]
    AnyMessageUnsupported(String),
    #[error("Protobuf decode error: {0}")]
    Decode(#[from] prost::DecodeError),
}
