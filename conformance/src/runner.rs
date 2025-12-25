use cel::context::{Context, VariableResolver};
use cel::objects::{Struct, Value as CelValue};
use cel::Program;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use walkdir::WalkDir;

use crate::proto::cel::expr::conformance::test::{
    simple_test::ResultMatcher, SimpleTest, SimpleTestFile,
};
use crate::textproto::parse_textproto_to_prost;
use crate::value_converter::proto_value_to_cel_value;

/// Container-aware variable resolver that tries prefixed names first
struct ContainerResolver {
    container: String,
    variables: BTreeMap<String, CelValue>,
}

impl VariableResolver for ContainerResolver {
    fn resolve(&self, variable: &str) -> Option<CelValue> {
        // If container is non-empty and the variable is not already fully qualified,
        // try the prefixed version first
        if !self.container.is_empty() && !variable.starts_with(&format!("{}.", self.container)) {
            let prefixed = format!("{}.{}", self.container, variable);
            if let Some(value) = self.variables.get(&prefixed) {
                return Some(value.clone());
            }
        }

        // Fall back to exact match
        self.variables.get(variable).cloned()
    }
}

/// Get a list of proto type names to register for a given container.
///
/// These types need to be available as variables so expressions like
/// `GlobalEnum.GAZ` can resolve `GlobalEnum` to the type name string.
fn get_container_type_names(container: &str) -> Vec<(String, String)> {
    let mut types = Vec::new();

    match container {
        "cel.expr.conformance.proto2" => {
            types.push((
                "cel.expr.conformance.proto2.TestAllTypes".to_string(),
                "cel.expr.conformance.proto2.TestAllTypes".to_string(),
            ));
            types.push((
                "cel.expr.conformance.proto2.NestedTestAllTypes".to_string(),
                "cel.expr.conformance.proto2.NestedTestAllTypes".to_string(),
            ));
            types.push((
                "cel.expr.conformance.proto2.GlobalEnum".to_string(),
                "cel.expr.conformance.proto2.GlobalEnum".to_string(),
            ));
            types.push((
                "cel.expr.conformance.proto2.TestAllTypes.NestedEnum".to_string(),
                "cel.expr.conformance.proto2.TestAllTypes.NestedEnum".to_string(),
            ));
        }
        "cel.expr.conformance.proto3" => {
            types.push((
                "cel.expr.conformance.proto3.TestAllTypes".to_string(),
                "cel.expr.conformance.proto3.TestAllTypes".to_string(),
            ));
            types.push((
                "cel.expr.conformance.proto3.NestedTestAllTypes".to_string(),
                "cel.expr.conformance.proto3.NestedTestAllTypes".to_string(),
            ));
            types.push((
                "cel.expr.conformance.proto3.GlobalEnum".to_string(),
                "cel.expr.conformance.proto3.GlobalEnum".to_string(),
            ));
            types.push((
                "cel.expr.conformance.proto3.TestAllTypes.NestedEnum".to_string(),
                "cel.expr.conformance.proto3.TestAllTypes.NestedEnum".to_string(),
            ));
        }
        "google.protobuf" => {
            types.push((
                "google.protobuf.NullValue".to_string(),
                "google.protobuf.NullValue".to_string(),
            ));
            types.push((
                "google.protobuf.Value".to_string(),
                "google.protobuf.Value".to_string(),
            ));
            types.push((
                "google.protobuf.ListValue".to_string(),
                "google.protobuf.ListValue".to_string(),
            ));
            types.push((
                "google.protobuf.Struct".to_string(),
                "google.protobuf.Struct".to_string(),
            ));
            // Wrapper types
            types.push((
                "google.protobuf.Int32Value".to_string(),
                "google.protobuf.Int32Value".to_string(),
            ));
            types.push((
                "google.protobuf.UInt32Value".to_string(),
                "google.protobuf.UInt32Value".to_string(),
            ));
            types.push((
                "google.protobuf.Int64Value".to_string(),
                "google.protobuf.Int64Value".to_string(),
            ));
            types.push((
                "google.protobuf.UInt64Value".to_string(),
                "google.protobuf.UInt64Value".to_string(),
            ));
            types.push((
                "google.protobuf.FloatValue".to_string(),
                "google.protobuf.FloatValue".to_string(),
            ));
            types.push((
                "google.protobuf.DoubleValue".to_string(),
                "google.protobuf.DoubleValue".to_string(),
            ));
            types.push((
                "google.protobuf.BoolValue".to_string(),
                "google.protobuf.BoolValue".to_string(),
            ));
            types.push((
                "google.protobuf.StringValue".to_string(),
                "google.protobuf.StringValue".to_string(),
            ));
            types.push((
                "google.protobuf.BytesValue".to_string(),
                "google.protobuf.BytesValue".to_string(),
            ));
        }
        _ => {}
    }

    types
}

pub struct ConformanceRunner {
    test_data_dir: PathBuf,
    category_filter: Option<String>,
}

impl ConformanceRunner {
    pub fn new(test_data_dir: PathBuf) -> Self {
        Self {
            test_data_dir,
            category_filter: None,
        }
    }

    pub fn with_category_filter(mut self, category: String) -> Self {
        self.category_filter = Some(category);
        self
    }

    pub fn run_all_tests(&self) -> Result<TestResults, RunnerError> {
        let mut results = TestResults::default();

        // Get the proto directory path
        let proto_dir = self
            .test_data_dir
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("proto");

        // Walk through all .textproto files
        for entry in WalkDir::new(&self.test_data_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .map(|s| s == "textproto")
                    .unwrap_or(false)
            })
        {
            let path = entry.path();
            let file_results = self.run_test_file(path, &proto_dir)?;
            results.merge(file_results);
        }

        Ok(results)
    }

    fn run_test_file(&self, path: &Path, proto_dir: &Path) -> Result<TestResults, RunnerError> {
        let content = fs::read_to_string(path)?;

        // Parse textproto using prost-reflect (with protoc fallback)
        let test_file: SimpleTestFile = parse_textproto_to_prost(
            &content,
            "cel.expr.conformance.test.SimpleTestFile",
            &["cel/expr/conformance/test/simple.proto"],
            &[proto_dir.to_str().unwrap()],
        )
        .map_err(|e| {
            RunnerError::ParseError(format!("Failed to parse {}: {}", path.display(), e))
        })?;

        let mut results = TestResults::default();

        // Run all tests in all sections
        for section in &test_file.section {
            for test in &section.test {
                // Filter by category if specified
                if let Some(ref filter_category) = self.category_filter {
                    if !test_name_matches_category(&test.name, filter_category) {
                        continue;
                    }
                }

                // Catch panics so we can continue running all tests
                let test_result =
                    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| self.run_test(test)));

                let result = match test_result {
                    Ok(r) => r,
                    Err(_) => TestResult::Failed {
                        name: test.name.clone(),
                        error: "Test panicked during execution".to_string(),
                    },
                };
                results.merge(result.into());
            }
        }

        Ok(results)
    }

    fn run_test(&self, test: &SimpleTest) -> TestResult {
        let test_name = &test.name;

        // Skip tests that are check-only or have features we don't support yet
        if test.check_only {
            return TestResult::Skipped {
                name: test_name.clone(),
                reason: "check_only not yet implemented".to_string(),
            };
        }

        // Parse the expression - catch panics here too
        let program = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            Program::compile(&test.expr)
        })) {
            Ok(Ok(p)) => p,
            Ok(Err(e)) => {
                return TestResult::Failed {
                    name: test_name.clone(),
                    error: format!("Parse error: {}", e),
                };
            }
            Err(_) => {
                return TestResult::Failed {
                    name: test_name.clone(),
                    error: "Panic during parsing".to_string(),
                };
            }
        };

        // Build context with bindings
        let mut context = Context::default();

        // Set container if present (for type name qualification)
        if !test.container.is_empty() {
            context.set_container(test.container.clone());
        }

        // Prepare container resolver if needed (must be declared outside the block to live long enough)
        let mut variables_map = BTreeMap::new();
        let container = test.container.clone();

        // Add proto type names for container-aware type resolution
        if !container.is_empty() {
            for (type_name, type_value) in get_container_type_names(&container) {
                variables_map.insert(type_name, CelValue::String(Arc::new(type_value)));
            }
        }

        // Always create resolver if container is set (even without bindings)
        let use_resolver = !container.is_empty();

        if !test.bindings.is_empty() {
            for (key, expr_value) in &test.bindings {
                // Extract Value from ExprValue
                let proto_value = match expr_value.kind.as_ref() {
                    Some(crate::proto::cel::expr::expr_value::Kind::Value(v)) => v,
                    _ => {
                        return TestResult::Skipped {
                            name: test_name.clone(),
                            reason: format!("Binding '{}' is not a value (error/unknown)", key),
                        };
                    }
                };

                match proto_value_to_cel_value(proto_value) {
                    Ok(cel_value) => {
                        if use_resolver {
                            // Store in map for resolver
                            variables_map.insert(key.clone(), cel_value);
                        } else {
                            // Add directly to context
                            context.add_variable(key, cel_value);
                        }
                    }
                    Err(e) => {
                        return TestResult::Failed {
                            name: test_name.clone(),
                            error: format!("Failed to convert binding '{}': {}", key, e),
                        };
                    }
                }
            }
        }

        // Create resolver outside the bindings block so it lives long enough
        let resolver = if use_resolver {
            Some(ContainerResolver {
                container,
                variables: variables_map,
            })
        } else {
            None
        };

        // Set the resolver if we have one
        if let Some(ref resolver) = resolver {
            context.set_variable_resolver(resolver);
        }

        // Execute the program - catch panics
        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| program.execute(&context)))
                .unwrap_or_else(|_| {
                    Err(cel::ExecutionError::function_error(
                        "execution",
                        "Panic during execution",
                    ))
                });

        // Check the result against the expected result
        match &test.result_matcher {
            Some(ResultMatcher::Value(expected_value)) => {
                match proto_value_to_cel_value(expected_value) {
                    Ok(expected_cel_value) => match result {
                        Ok(actual_value) => {
                            // Unwrap wrapper types before comparison
                            let actual_unwrapped = unwrap_wrapper_if_needed(actual_value.clone());
                            let expected_unwrapped = unwrap_wrapper_if_needed(expected_cel_value.clone());

                            if values_equal(&actual_unwrapped, &expected_unwrapped) {
                                TestResult::Passed {
                                    name: test_name.clone(),
                                }
                            } else {
                                TestResult::Failed {
                                    name: test_name.clone(),
                                    error: format!(
                                        "Expected {:?}, got {:?}",
                                        expected_unwrapped, actual_unwrapped
                                    ),
                                }
                            }
                        }
                        Err(e) => TestResult::Failed {
                            name: test_name.clone(),
                            error: format!("Execution error: {:?}", e),
                        },
                    },
                    Err(e) => TestResult::Failed {
                        name: test_name.clone(),
                        error: format!("Failed to convert expected value: {}", e),
                    },
                }
            }
            Some(ResultMatcher::EvalError(_)) => {
                // Test expects an error
                match result {
                    Ok(_) => TestResult::Failed {
                        name: test_name.clone(),
                        error: "Expected error but got success".to_string(),
                    },
                    Err(_) => TestResult::Passed {
                        name: test_name.clone(),
                    },
                }
            }
            Some(ResultMatcher::Unknown(_)) => TestResult::Skipped {
                name: test_name.clone(),
                reason: "Unknown result matching not yet implemented".to_string(),
            },
            Some(ResultMatcher::AnyEvalErrors(_)) => TestResult::Skipped {
                name: test_name.clone(),
                reason: "Any eval errors matching not yet implemented".to_string(),
            },
            Some(ResultMatcher::AnyUnknowns(_)) => TestResult::Skipped {
                name: test_name.clone(),
                reason: "Any unknowns matching not yet implemented".to_string(),
            },
            Some(ResultMatcher::TypedResult(_)) => TestResult::Skipped {
                name: test_name.clone(),
                reason: "Typed result matching not yet implemented".to_string(),
            },
            None => {
                // Default to expecting true
                match result {
                    Ok(CelValue::Bool(true)) => TestResult::Passed {
                        name: test_name.clone(),
                    },
                    Ok(v) => TestResult::Failed {
                        name: test_name.clone(),
                        error: format!("Expected true, got {:?}", v),
                    },
                    Err(e) => TestResult::Failed {
                        name: test_name.clone(),
                        error: format!("Execution error: {:?}", e),
                    },
                }
            }
        }
    }
}

fn values_equal(a: &CelValue, b: &CelValue) -> bool {
    use CelValue::*;
    match (a, b) {
        (Null, Null) => true,
        (Bool(a), Bool(b)) => a == b,
        (Int(a), Int(b)) => a == b,
        (UInt(a), UInt(b)) => a == b,
        (Float(a), Float(b)) => {
            // Handle NaN specially
            if a.is_nan() && b.is_nan() {
                true
            } else {
                a == b
            }
        }
        (String(a), String(b)) => a == b,
        (Bytes(a), Bytes(b)) => a == b,
        (List(a), List(b)) => {
            if a.len() != b.len() {
                return false;
            }
            a.iter().zip(b.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Map(a), Map(b)) => {
            if a.map.len() != b.map.len() {
                return false;
            }
            for (key, a_val) in a.map.iter() {
                match b.map.get(key) {
                    Some(b_val) => {
                        if !values_equal(a_val, b_val) {
                            return false;
                        }
                    }
                    None => return false,
                }
            }
            true
        }
        (Struct(a), Struct(b)) => structs_equal(a, b),
        (Timestamp(a), Timestamp(b)) => a == b,
        (Duration(a), Duration(b)) => a == b,
        _ => false,
    }
}

fn structs_equal(a: &Struct, b: &Struct) -> bool {
    // Type names must match
    if a.type_name != b.type_name {
        return false;
    }

    // Field counts must match
    if a.fields.len() != b.fields.len() {
        return false;
    }

    // All fields must have equal values
    for (key, value_a) in a.fields.iter() {
        match b.fields.get(key) {
            Some(value_b) => {
                if !values_equal(value_a, value_b) {
                    return false;
                }
            }
            None => return false,
        }
    }

    true
}

fn unwrap_wrapper_if_needed(value: CelValue) -> CelValue {
    match value {
        CelValue::Struct(s) => {
            // Check if this is a wrapper type
            let type_name = s.type_name.as_str();

            // Check if it's a Google protobuf wrapper type
            if !type_name.starts_with("google.protobuf.") || !type_name.ends_with("Value") {
                return CelValue::Struct(s);
            }

            // Check if the wrapper has a value field
            if let Some(v) = s.fields.get("value") {
                // Unwrap to the inner value
                return v.clone();
            }

            // Empty wrapper - return default value for the type
            match type_name {
                "google.protobuf.Int32Value" | "google.protobuf.Int64Value" => CelValue::Int(0),
                "google.protobuf.UInt32Value" | "google.protobuf.UInt64Value" => CelValue::UInt(0),
                "google.protobuf.FloatValue" | "google.protobuf.DoubleValue" => CelValue::Float(0.0),
                "google.protobuf.StringValue" => CelValue::String(Arc::new(String::new())),
                "google.protobuf.BytesValue" => CelValue::Bytes(Arc::new(Vec::new())),
                "google.protobuf.BoolValue" => CelValue::Bool(false),
                _ => CelValue::Struct(s),
            }
        }
        other => other,
    }
}

#[derive(Debug, Default, Clone)]
pub struct TestResults {
    pub passed: Vec<String>,
    pub failed: Vec<(String, String)>,
    pub skipped: Vec<(String, String)>,
}

impl TestResults {
    pub fn merge(&mut self, other: TestResults) {
        self.passed.extend(other.passed);
        self.failed.extend(other.failed);
        self.skipped.extend(other.skipped);
    }

    pub fn total(&self) -> usize {
        self.passed.len() + self.failed.len() + self.skipped.len()
    }

    pub fn print_summary(&self) {
        let total = self.total();
        let passed = self.passed.len();
        let failed = self.failed.len();
        let skipped = self.skipped.len();

        println!("\nConformance Test Results:");
        println!(
            "  Passed:  {} ({:.1}%)",
            passed,
            if total > 0 {
                (passed as f64 / total as f64) * 100.0
            } else {
                0.0
            }
        );
        println!(
            "  Failed:  {} ({:.1}%)",
            failed,
            if total > 0 {
                (failed as f64 / total as f64) * 100.0
            } else {
                0.0
            }
        );
        println!(
            "  Skipped: {} ({:.1}%)",
            skipped,
            if total > 0 {
                (skipped as f64 / total as f64) * 100.0
            } else {
                0.0
            }
        );
        println!("  Total:   {}", total);

        if !self.failed.is_empty() {
            self.print_grouped_failures();
        }

        if !self.skipped.is_empty() && self.skipped.len() <= 20 {
            println!("\nSkipped tests:");
            for (name, reason) in &self.skipped {
                println!("  - {}: {}", name, reason);
            }
        } else if !self.skipped.is_empty() {
            println!(
                "\nSkipped {} tests (use --verbose to see details)",
                self.skipped.len()
            );
        }
    }

    fn print_grouped_failures(&self) {
        use std::collections::HashMap;

        // Group by test category based on test name patterns
        let mut category_groups: HashMap<String, Vec<&(String, String)>> = HashMap::new();

        for failure in &self.failed {
            let category = categorize_test(&failure.0, &failure.1);
            category_groups
                .entry(category)
                .or_default()
                .push(failure);
        }

        // Sort categories by count (descending)
        let mut categories: Vec<_> = category_groups.iter().collect();
        categories.sort_by(|a, b| b.1.len().cmp(&a.1.len()));

        println!("\nFailed tests by category:");
        for (category, failures) in &categories {
            let count = failures.len();
            let failure_word = if count == 1 { "failure" } else { "failures" };
            println!("\n  {} ({} {}):", category, count, failure_word);
            // Show up to 5 examples per category
            let examples_to_show = failures.len().min(5);
            for failure in failures.iter().take(examples_to_show) {
                println!("    - {}: {}", failure.0, failure.1);
            }
            if failures.len() > examples_to_show {
                println!("    ... and {} more", failures.len() - examples_to_show);
            }
        }
    }
}

fn categorize_test(name: &str, error: &str) -> String {
    // First, categorize by error type
    if error.starts_with("Parse error:") {
        if name.contains("optional") || name.contains("opt") {
            return "Optional/Chaining (Parse errors)".to_string();
        }
        return "Parse errors".to_string();
    }

    if error.starts_with("Execution error:") {
        // Categorize by error content
        if error.contains("UndeclaredReference") {
            let ref_name = extract_reference_name(error);
            if ref_name == "dyn" {
                return "Dynamic type operations".to_string();
            } else if ref_name == "format" {
                return "String formatting".to_string();
            } else if ref_name == "greatest" || ref_name == "least" {
                return "Math functions (greatest/least)".to_string();
            } else if ref_name == "exists" || ref_name == "all" || ref_name == "existsOne" {
                return "List/map operations (exists/all/existsOne)".to_string();
            } else if ref_name == "optMap" || ref_name == "optFlatMap" {
                return "Optional operations (optMap/optFlatMap)".to_string();
            } else if ref_name == "bind" {
                return "Macro/binding operations".to_string();
            } else if ref_name == "encode" || ref_name == "decode" {
                return "Encoding/decoding operations".to_string();
            } else if ref_name == "transformList" || ref_name == "transformMap" {
                return "Transform operations".to_string();
            } else if ref_name == "type" || ref_name == "google" {
                return "Type operations".to_string();
            } else if ref_name == "a" {
                return "Qualified identifier resolution".to_string();
            }
            return format!("Undeclared references ({})", ref_name);
        }

        if error.contains("FunctionError") && error.contains("Panic") {
            if name.contains("to_any") || name.contains("to_json") || name.contains("to_null") {
                return "Type conversions (to_any/to_json/to_null)".to_string();
            }
            if name.contains("eq_") || name.contains("ne_") {
                return "Equality operations (proto/type conversions)".to_string();
            }
            return "Function panics".to_string();
        }

        if error.contains("NoSuchKey") {
            return "Map key access errors".to_string();
        }

        if error.contains("UnsupportedBinaryOperator") {
            return "Binary operator errors".to_string();
        }

        if error.contains("ValuesNotComparable") {
            return "Comparison errors (bytes/unsupported)".to_string();
        }

        if error.contains("UnsupportedMapIndex") {
            return "Map index errors".to_string();
        }

        if error.contains("UnexpectedType") {
            return "Type mismatch errors".to_string();
        }

        if error.contains("DivisionByZero") {
            return "Division by zero errors".to_string();
        }

        if error.contains("NoSuchOverload") {
            return "Overload resolution errors".to_string();
        }
    }

    // Categorize by test name patterns
    if name.contains("optional") || name.contains("opt") {
        return "Optional/Chaining operations".to_string();
    }

    if name.contains("struct") {
        return "Struct operations".to_string();
    }

    if name.contains("string") || name.contains("String") {
        return "String operations".to_string();
    }

    if name.contains("format") {
        return "String formatting".to_string();
    }

    if name.contains("timestamp") || name.contains("Timestamp") {
        return "Timestamp operations".to_string();
    }

    if name.contains("duration") || name.contains("Duration") {
        return "Duration operations".to_string();
    }

    if name.contains("eq_") || name.contains("ne_") {
        return "Equality/inequality operations".to_string();
    }

    if name.contains("lt_")
        || name.contains("gt_")
        || name.contains("lte_")
        || name.contains("gte_")
    {
        return "Comparison operations (lt/gt/lte/gte)".to_string();
    }

    if name.contains("bytes") || name.contains("Bytes") {
        return "Bytes operations".to_string();
    }

    if name.contains("list") || name.contains("List") {
        return "List operations".to_string();
    }

    if name.contains("map") || name.contains("Map") {
        return "Map operations".to_string();
    }

    if name.contains("unicode") {
        return "Unicode operations".to_string();
    }

    if name.contains("conversion") || name.contains("Conversion") {
        return "Type conversions".to_string();
    }

    if name.contains("math") || name.contains("Math") {
        return "Math operations".to_string();
    }

    // Default category
    "Other failures".to_string()
}

fn extract_reference_name(error: &str) -> &str {
    // Extract the reference name from "UndeclaredReference(\"name\")"
    if let Some(start) = error.find("UndeclaredReference(\"") {
        let start = start + "UndeclaredReference(\"".len();
        if let Some(end) = error[start..].find('"') {
            return &error[start..start + end];
        }
    }
    "unknown"
}

/// Check if a test name matches a category filter (before running the test).
/// This is an approximation based on test name patterns.
fn test_name_matches_category(test_name: &str, category: &str) -> bool {
    let name_lower = test_name.to_lowercase();
    let category_lower = category.to_lowercase();

    // Match category names to test name patterns
    match category_lower.as_str() {
        "dynamic type operations" | "dynamic" => {
            name_lower.contains("dyn") || name_lower.contains("dynamic")
        }
        "string formatting" | "format" => {
            name_lower.contains("format") || name_lower.starts_with("format_")
        }
        "math functions (greatest/least)" | "greatest" | "least" | "math functions" => {
            name_lower.contains("greatest") || name_lower.contains("least")
        }
        "optional/chaining (parse errors)"
        | "optional/chaining operations"
        | "optional"
        | "chaining" => {
            name_lower.contains("optional")
                || name_lower.contains("opt")
                || name_lower.contains("chaining")
        }
        "struct operations" | "struct" => name_lower.contains("struct"),
        "string operations" | "string" => {
            name_lower.contains("string") && !name_lower.contains("format")
        }
        "timestamp operations" | "timestamp" => {
            name_lower.contains("timestamp") || name_lower.contains("time")
        }
        "duration operations" | "duration" => name_lower.contains("duration"),
        "equality/inequality operations" | "equality" | "inequality" => {
            name_lower.starts_with("eq_") || name_lower.starts_with("ne_")
        }
        "comparison operations (lt/gt/lte/gte)" | "comparison" => {
            name_lower.starts_with("lt_")
                || name_lower.starts_with("gt_")
                || name_lower.starts_with("lte_")
                || name_lower.starts_with("gte_")
        }
        "bytes operations" | "bytes" => name_lower.contains("bytes") || name_lower.contains("byte"),
        "list operations" | "list" => name_lower.contains("list") || name_lower.contains("elem"),
        "map operations" | "map" => name_lower.contains("map") && !name_lower.contains("optmap"),
        "unicode operations" | "unicode" => name_lower.contains("unicode"),
        "type conversions" | "conversion" => {
            name_lower.contains("conversion") || name_lower.starts_with("to_")
        }
        "parse errors" => {
            // We can't predict parse errors from the name, so include all tests
            // that might have parse errors (optional syntax, etc.)
            name_lower.contains("optional") || name_lower.contains("opt")
        }
        _ => {
            // Try partial matching
            category_lower
                .split_whitespace()
                .any(|word| name_lower.contains(word))
        }
    }
}

#[derive(Debug)]
pub enum TestResult {
    Passed { name: String },
    Failed { name: String, error: String },
    Skipped { name: String, reason: String },
}

impl From<TestResult> for TestResults {
    fn from(result: TestResult) -> Self {
        match result {
            TestResult::Passed { name } => TestResults {
                passed: vec![name],
                failed: vec![],
                skipped: vec![],
            },
            TestResult::Failed { name, error } => TestResults {
                passed: vec![],
                failed: vec![(name, error)],
                skipped: vec![],
            },
            TestResult::Skipped { name, reason } => TestResults {
                passed: vec![],
                failed: vec![],
                skipped: vec![(name, reason)],
            },
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RunnerError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Textproto parse error: {0}")]
    ParseError(String),
}
