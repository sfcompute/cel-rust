fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Tell cargo to rerun this build script if the proto files change
    println!("cargo:rerun-if-changed=../cel-spec/proto");

    // Configure prost to generate Rust code from proto files
    let mut config = prost_build::Config::new();
    config.protoc_arg("--experimental_allow_proto3_optional");
    
    // Add well-known types from prost-types
    config.bytes(&["."]);
    
    // Compile the proto files
    config.compile_protos(
        &[
            "../cel-spec/proto/cel/expr/value.proto",
            "../cel-spec/proto/cel/expr/syntax.proto",
            "../cel-spec/proto/cel/expr/checked.proto",
            "../cel-spec/proto/cel/expr/eval.proto",
            "../cel-spec/proto/cel/expr/conformance/test/simple.proto",
        ],
        &["../cel-spec/proto"],
    )?;

    Ok(())
}

