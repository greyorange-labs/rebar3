# Resource Conflict Demonstration

This document demonstrates how the `ct_parallel` provider successfully isolates Common Test suites that would conflict when running in a single node.

## Resource Conflicts Handled

### 1. Process Registration Conflicts

Both `resource_conflict_suite1` and `resource_conflict_suite2` try to register a process with the same name `shared_test_process`. In a single node execution, this would cause:

```erlang
** exception error: badarg
     in function  register/2
        called as register(shared_test_process, <0.xxx.0>)
```

**Parallel Solution**: Each suite runs in its own distributed node, so both can successfully register `shared_test_process` without conflict.

### 2. ETS Table Name Conflicts

Both suites try to create an ETS table named `shared_test_table`. In single node execution, this would cause:

```erlang
** exception error: badarg
     in function  ets:new/2
        called as ets:new(shared_test_table, [named_table, public, set])
```

**Parallel Solution**: Each node has its own ETS table namespace, allowing both suites to create and use `shared_test_table` independently.

### 3. Application Environment Conflicts

Both suites set the same application environment key `shared_config_key` but with different values:
- `resource_conflict_suite1` sets it to `suite1_value`
- `resource_conflict_suite2` sets it to `suite2_value`

In single node execution, the second suite would overwrite the first suite's value, potentially causing test failures.

**Parallel Solution**: Each node maintains its own application environment, allowing both suites to set and verify their expected values independently.

### 4. Application Lifecycle Conflicts

The `application_conflict_suite` demonstrates conflicts with:
- Starting/stopping applications (e.g., `inets`)
- Setting persistent terms with the same keys
- Managing code paths and module loading

### 5. File System Conflicts

The `file_system_conflict_suite` shows potential conflicts with:
- Creating directories with the same names
- Creating lock files
- Managing temporary files

## Test Execution Results

When running with `rebar3 ct_parallel --spec test/parallel.spec`, all 17 tests pass successfully:

```
%%% math_SUITE: ..
%%% division_SUITE: ..
%%% integration_SUITE: .
%%% resource_conflict_suite1: ...
%%% resource_conflict_suite2: ...
%%% application_conflict_suite: ...
%%% file_system_conflict_suite: ...
All 17 tests passed.
```

Each suite runs independently in its own Erlang node, demonstrating true isolation.

## Benefits of Parallel Execution

1. **True Isolation**: No resource conflicts between test suites
2. **Faster Execution**: Multiple suites run simultaneously
3. **Safer Testing**: Process crashes in one suite don't affect others
4. **Realistic Testing**: Can test actual resource conflicts that occur in production
5. **Shared Compilation**: Compiled code is shared between nodes, avoiding recompilation overhead

## Node Information During Execution

During execution, you can see in the logs that each suite runs on a different node:
- Main node: `nonode@nohost` or similar
- Parallel nodes: `ct_parallel_N@hostname` where N is the suite number

This demonstrates the true distributed nature of the parallel execution.
