# Parallel Common Test Provider for Rebar3

This is a new provider that extends rebar3 with parallel Common Test suite execution capabilities. It allows you to run multiple test suites in parallel on separate nodes while sharing compiled artifacts to avoid re-compilation.

## Features

- **Parallel Execution**: Runs each suite specified in spec files in separate parallel processes
- **Isolated Execution**: Each suite runs in its own isolated environment to prevent interference
- **Shared Compilation**: Avoids re-compilation and cover compilation for each parallel node by sharing artifacts
- **Spec File Based**: Uses Common Test spec files to define which suites to run in parallel
- **Configurable Workers**: Supports custom number of parallel workers (defaults to number of schedulers)

## Usage

### Basic Usage

```bash
# Run CT suites in parallel using a spec file
rebar3 ct_parallel --spec test/parallel.spec

# Run with verbose output
rebar3 ct_parallel --spec test/parallel.spec --verbose

# Specify custom log directory
rebar3 ct_parallel --spec test/parallel.spec --logdir /tmp/ct_logs

# Enable cover analysis
rebar3 ct_parallel --spec test/parallel.spec --cover
```

### Multiple Spec Files

```bash
# Run multiple spec files in parallel
rebar3 ct_parallel --spec test/suite1.spec,test/suite2.spec,test/suite3.spec
```

### Command Line Options

- `--spec`: List of test specifications for parallel execution (required)
- `--logdir`: Base log folder for parallel execution
- `--verbose`: Verbose output
- `--cover`: Generate cover data
- `--cover_export_name`: Base name of the coverdata file to write
- `--max_workers`: Maximum number of parallel workers (default: number of schedulers)
- `--compile_only`: Compile modules in the project with the test configuration but do not run the tests
- `--name`: Distributed node name
- `--sname`: Distributed short node name
- `--setcookie`: Distributed node cookie
- `--sys_config`: List of application config files

## Spec File Format

The parallel CT provider supports standard Common Test spec files. Each suite defined in the spec file will be executed in parallel.

### Example Spec File

```erlang
%% test/parallel.spec

%% Run all suites in the test directory
{suites, "test", all}.

%% Or specify individual suites
{suites, "test", [my_SUITE, other_SUITE]}.

%% Run suites from different directories
{suites, "apps/app1/test", [app1_SUITE]}.
{suites, "apps/app2/test", [app2_SUITE]}.
```

## How It Works

1. **Compilation Phase**: The provider first compiles all project code and test suites once using the standard CT compilation process
2. **Spec Parsing**: Parses the provided spec files to extract individual test suites
3. **Suite Isolation**: Creates individual spec files for each suite to ensure isolated execution
4. **Parallel Execution**: Uses `rebar_parallel` to execute each suite in a separate worker process
5. **Result Aggregation**: Collects and reports results from all parallel executions
6. **Logging**: Each suite gets its own log directory under `_build/test/logs/parallel/<suite_name>`

## Benefits

### Avoiding Re-compilation

Unlike other approaches that spawn separate rebar3 processes, this provider:
- Compiles the project only once at the beginning
- Shares compiled artifacts across all parallel test executions
- Avoids the overhead of re-compiling dependencies and test code
- Significantly reduces total execution time for large test suites

### Isolation

Each test suite runs in isolation:
- Separate log directories prevent log file conflicts
- Independent CT processes avoid state interference
- Failed suites don't affect other running suites

### Scalability

- Automatically uses the number of available CPU schedulers as default workers
- Configurable worker count for different hardware configurations
- Efficient resource utilization without oversubscription

## Example Project Structure

```
my_project/
├── apps/
│   ├── app1/
│   │   └── test/
│   │       ├── app1_SUITE.erl
│   │       └── app1_integration_SUITE.erl
│   └── app2/
│       └── test/
│           ├── app2_SUITE.erl
│           └── app2_api_SUITE.erl
├── test/
│   ├── system_SUITE.erl
│   └── parallel.spec
└── rebar.config
```

### Spec File for This Project

```erlang
%% test/parallel.spec
{suites, "apps/app1/test", [app1_SUITE, app1_integration_SUITE]}.
{suites, "apps/app2/test", [app2_SUITE, app2_api_SUITE]}.
{suites, "test", [system_SUITE]}.
```

This will execute all 5 test suites in parallel, each in its own isolated environment.

## Implementation Details

The provider extends rebar3's existing CT infrastructure by:

1. **Reusing CT Compilation**: Leverages `rebar_prv_common_test:compile/2` for test compilation
2. **Parallel Framework**: Uses `rebar_parallel` for efficient parallel task execution
3. **CT Integration**: Calls `ct:run_test/1` for each suite with appropriate isolation
4. **Error Handling**: Provides comprehensive error reporting and suite-level failure tracking
5. **Coverage Support**: Integrates with rebar3's cover analysis when enabled

The implementation is designed to be a drop-in addition to rebar3 that doesn't interfere with existing CT functionality while providing significant performance improvements for projects with multiple test suites.
