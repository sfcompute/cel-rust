# CEL Conformance Tests

This crate provides a test harness for running the official CEL conformance tests from the [cel-spec](https://github.com/google/cel-spec) repository against the cel-rust implementation.

## Setup

The conformance tests are pulled in as a git submodule. To initialize the submodule:

```bash
git submodule update --init --recursive
```

## Running the Tests

To run all conformance tests:

```bash
cargo run --bin run_conformance
```

Or from the workspace root:

```bash
cargo run --package conformance --bin run_conformance
```

## Test Structure

The conformance tests are located in `cel-spec/tests/simple/testdata/` and are written in textproto format. Each test file contains:

- **SimpleTestFile**: A collection of test sections
- **SimpleTestSection**: A group of related tests
- **SimpleTest**: Individual test cases with:
  - CEL expression to evaluate
  - Variable bindings (if any)
  - Expected result (value, error, or unknown)

## Current Status

The test harness currently supports:
- ✅ Basic value matching (int, uint, float, string, bytes, bool, null, list, map)
- ✅ Error result matching
- ✅ Variable bindings
- ⚠️ Type checking (check_only tests are skipped)
- ⚠️ Unknown result matching (skipped)
- ⚠️ Typed result matching (skipped)
- ⚠️ Test files with `google.protobuf.Any` messages (skipped - `protoc --encode` limitation)

## Known Limitations

Some test files (like `dynamic.textproto`) contain `google.protobuf.Any` messages with type URLs. The `protoc --encode` command doesn't support resolving types inside Any messages, so these test files are automatically skipped with a warning. This is a limitation of the protoc tool, not the test harness.

## Requirements

- `protoc` (Protocol Buffers compiler) must be installed and available in PATH
- The cel-spec submodule must be initialized

