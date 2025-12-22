use conformance::ConformanceRunner;
use std::panic;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Set a panic hook that suppresses the default panic output
    // We'll catch panics in the test runner and report them as failures
    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(move |panic_info| {
        // Suppress panic output - we'll handle it in the test runner
        // Only show panics if RUST_BACKTRACE is set
        if std::env::var("RUST_BACKTRACE").is_ok() {
            default_hook(panic_info);
        }
    }));
    // Get the test data directory from the cel-spec submodule
    let test_data_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("cel-spec")
        .join("tests")
        .join("simple")
        .join("testdata");

    if !test_data_dir.exists() {
        eprintln!(
            "Error: Test data directory not found at: {}",
            test_data_dir.display()
        );
        eprintln!("Make sure the cel-spec submodule is initialized:");
        eprintln!("  git submodule update --init --recursive");
        std::process::exit(1);
    }

    println!(
        "Running conformance tests from: {}",
        test_data_dir.display()
    );

    let runner = ConformanceRunner::new(test_data_dir);
    let results = match runner.run_all_tests() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error running tests: {}", e);
            std::process::exit(1);
        }
    };

    results.print_summary();

    // Exit with error code if there are failures, but still show all results
    if !results.failed.is_empty() {
        std::process::exit(1);
    }

    Ok(())
}
