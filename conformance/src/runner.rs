use cel::context::Context;
use cel::objects::Value as CelValue;
use cel::Program;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

use crate::proto::cel::expr::conformance::test::{
    simple_test::ResultMatcher, SimpleTest, SimpleTestFile,
};
use crate::textproto::parse_textproto_to_prost;
use crate::value_converter::proto_value_to_cel_value;

pub struct ConformanceRunner {
    test_data_dir: PathBuf,
}

impl ConformanceRunner {
    pub fn new(test_data_dir: PathBuf) -> Self {
        Self { test_data_dir }
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

        // Parse textproto using protoc
        let test_file: SimpleTestFile = match parse_textproto_to_prost(
            &content,
            "cel.expr.conformance.test.SimpleTestFile",
            &["cel/expr/conformance/test/simple.proto"],
            &[proto_dir.to_str().unwrap()],
        ) {
            Ok(file) => file,
            Err(crate::textproto::TextprotoParseError::AnyMessageUnsupported(msg)) => {
                // Some test files contain Any messages that protoc --encode can't handle
                // Skip these files for now with a warning (printed in dim)
                use std::io::Write;
                use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
                let mut stderr = StandardStream::stderr(ColorChoice::Auto);
                let _ = stderr.set_color(ColorSpec::new().set_fg(Some(Color::Rgb(100, 100, 100))));
                let _ = writeln!(stderr, "Warning: Skipping {}: {}", path.display(), msg);
                let _ = stderr.reset();
                return Ok(TestResults::default());
            }
            Err(e) => {
                return Err(RunnerError::ParseError(format!(
                    "Failed to parse {}: {}",
                    path.display(),
                    e
                )));
            }
        };

        let mut results = TestResults::default();

        // Run all tests in all sections
        for section in &test_file.section {
            for test in &section.test {
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
                        context.add_variable(key, cel_value);
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
                            if values_equal(&actual_value, &expected_cel_value) {
                                TestResult::Passed {
                                    name: test_name.clone(),
                                }
                            } else {
                                TestResult::Failed {
                                    name: test_name.clone(),
                                    error: format!(
                                        "Expected {:?}, got {:?}",
                                        expected_cel_value, actual_value
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
        _ => false,
    }
}

#[derive(Debug, Default)]
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
            println!("\nFailed tests:");
            for (name, error) in &self.failed {
                println!("  - {}: {}", name, error);
            }
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
