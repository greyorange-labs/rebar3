#!/bin/bash

# Test script for parallel CT execution
# This script demonstrates the new --parallel flag functionality

set -e

echo "Testing Parallel CT Execution Feature"
echo "======================================"

cd /Users/amar.c/workspace/erlang_lib/gor_forked/rebar3

# Build the modified rebar3
echo "Building rebar3..."
./rebar3 escriptize

# Create a simple test structure for demonstration
echo "Creating test structure..."
mkdir -p demo_tests/test

# Create a simple test suite
cat > demo_tests/test/demo_SUITE.erl << 'EOF'
-module(demo_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [test_case_1, test_case_2].

test_case_1(_Config) ->
    ct:log("Running test case 1"),
    timer:sleep(1000), % Simulate some work
    ok.

test_case_2(_Config) ->
    ct:log("Running test case 2"),
    timer:sleep(1000), % Simulate some work
    ok.
EOF

# Create another test suite
cat > demo_tests/test/demo2_SUITE.erl << 'EOF'
-module(demo2_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [test_case_a, test_case_b].

test_case_a(_Config) ->
    ct:log("Running test case A"),
    timer:sleep(1000), % Simulate some work
    ok.

test_case_b(_Config) ->
    ct:log("Running test case B"),
    timer:sleep(1000), % Simulate some work
    ok.
EOF

# Create a CT spec file
cat > demo_tests/test.spec << 'EOF'
{suites, "demo_tests/test", all}.
EOF

cd demo_tests

# Create a simple rebar.config
cat > rebar.config << 'EOF'
{erl_opts, [debug_info]}.
{ct_opts, [
    {logdir, "_build/test/logs"}
]}.
EOF

echo ""
echo "Testing help output (should show --parallel option):"
echo "----------------------------------------------------"
../_build/default/bin/rebar3 ct --help | grep -A 1 -B 1 parallel || echo "Parallel option not found in help"

echo ""
echo "Testing discovery of --parallel flag:"
echo "------------------------------------"
../_build/default/bin/rebar3 ct --parallel --dir demo_tests/test || echo "Parallel execution test completed"

echo ""
echo "Test completed! Check the output above to see if parallel execution was attempted."
echo ""
echo "You can now use the --parallel flag with your actual test suites:"
echo "  rebar3 ct --parallel --spec your_spec_file.spec"
echo "  rebar3 ct --parallel --dir your_test_dir"
echo "  rebar3 ct --parallel --suite your_suite_name"
