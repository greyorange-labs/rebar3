# Parallel Common Test Execution in Rebar3

This implementation adds parallel Common Test execution to Rebar3, allowing each test suite to run on its own Erlang node for better isolation and performance.

## Features

- **Parallel Suite Execution**: Each test suite runs on a separate Erlang node
- **Coverage Support**: Code coverage is compiled once on the master node and distributed to all slave nodes
- **Suite Discovery**: Automatically discovers suites from CT spec files, directories, or explicit suite lists
- **Result Aggregation**: Collects and reports results from all parallel executions
- **Failed Suite Reporting**: Clearly identifies which suites failed
- **Backwards Compatibility**: Falls back to sequential execution when parallel is not possible

## Usage

### Command Line

Enable parallel execution with the `--parallel` flag:

```bash
# Run all suites in parallel
rebar3 ct --parallel

# Run with coverage
rebar3 ct --parallel --cover

# Run suites from specific directories in parallel
rebar3 ct --parallel --dir apps/myapp/test

# Run specific suites in parallel
rebar3 ct --parallel --suite apps/myapp/test/my_SUITE,apps/myapp/test/other_SUITE

# Run from spec file in parallel
rebar3 ct --parallel --spec master.spec
```

### Configuration

You can also enable parallel execution via rebar.config:

```erlang
{ct_opts, [
    {parallel, true},
    {spec, "master.spec"}
]}.
```

## How It Works

1. **Suite Discovery**: The system discovers all test suites from:
   - CT spec files (parsing `{suites, Dir, all}` directives)
   - Test directories (scanning for `*_SUITE.beam` files)
   - Explicitly specified suites

2. **Distributed Setup**: The master node initializes distributed mode with a unique name and cookie

3. **Coverage Compilation**: If `--cover` is enabled, code is instrumented once on the master node

4. **Slave Node Creation**: For each suite, a unique slave node is spawned with:
   - Same code paths as the master
   - Same cookie for authentication
   - Coverage instrumentation if enabled

5. **Parallel Execution**: Each suite runs on its dedicated node using `ct:run_test/1`

6. **Result Collection**: Results are collected from all nodes and aggregated

7. **Coverage Export**: Combined coverage data is written to a single `.coverdata` file

8. **Cleanup**: All slave nodes are stopped after execution

## Example with Your CT Spec

For your master CT spec file:

```erlang
{event_handler, [ct_event_handler]}.

{define, 'PICK_API_DIR', "../apps/pick/test/ct/api"}.
{suites, 'PICK_API_DIR', all}.

{define, 'PICK_PACT_PROVIDER_TESTS', "../apps/pick/test/pact_provider_test/"}.
{suites, 'PICK_PACT_PROVIDER_TESTS', all}.

{define, 'PICK_IMS_DIR', "../apps/pick/test/ct/ims_algo"}.
{suites, 'PICK_IMS_DIR', all}.

{define, 'PICK_PF_DIR', "../apps/pick/test/ct/flows"}.
{suites, 'PICK_PF_DIR', all}.
{skip_suites, 'PICK_PF_DIR', [pick_front_tote_flow_SUITE, pick_n_msio_binfull_SUITE], "Reason"}.
% ... other configurations
```

Run with:
```bash
rebar3 ct --parallel --spec master.spec --cover
```

This will:
- Parse the spec file to discover all suites from the defined directories
- Skip the suites marked with `skip_suites`
- Run each remaining suite in parallel on its own node
- Collect coverage data from all nodes
- Report which suites passed/failed

## Limitations

- Requires all test suites to be discoverable via spec files or directories
- Individual test cases cannot be run in parallel (only suites)
- Memory usage increases due to multiple Erlang nodes
- Some CT options may not be fully supported in parallel mode

## Error Handling

The implementation includes comprehensive error handling:
- Slave node startup failures
- Test execution exceptions
- Coverage compilation issues
- Result collection timeouts (5 minute default)

Failed suites are clearly reported with their names, making it easy to identify and fix issues.

## Implementation Details

The parallel execution is implemented in `rebar_prv_common_test.erl` with the following key functions:

- `run_tests_parallel/3`: Main parallel orchestration
- `discover_suites_for_parallel/2`: Suite discovery from specs/dirs
- `run_suite_on_slave/6`: Individual suite execution on slave nodes
- `collect_parallel_results/4`: Result aggregation
- `process_parallel_results/2`: Final result processing and reporting

The implementation follows the design outlined in `parallel_ct.txt`, ensuring proper cover compilation, node management, and result aggregation.
