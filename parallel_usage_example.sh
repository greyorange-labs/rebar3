#!/bin/bash

# Example usage with your master CT spec
# This shows how to convert your existing CT spec to use parallel execution

echo "Example: Converting your CT spec to use parallel execution"
echo "========================================================="

echo ""
echo "Your current command (sequential):"
echo "  rebar3 ct --spec master.spec --cover"
echo ""
echo "New parallel command:"
echo "  rebar3 ct --parallel --spec master.spec --cover"
echo ""

echo "What this does:"
echo "1. Parses your master.spec file to discover all suites"
echo "2. Honors skip_suites and skip_groups directives"
echo "3. Creates a separate Erlang node for each discovered suite"
echo "4. Runs each suite in parallel on its own node"
echo "5. Compiles cover once on master, distributes to all nodes"
echo "6. Collects results from all nodes"
echo "7. Reports which suites passed/failed"
echo "8. Writes combined coverage data"

echo ""
echo "Expected output format:"
echo "-----------------------"
cat << 'EOF'
===> Running Common Test suites in parallel...
===> Discovered 25 suites for parallel execution
===> Starting parallel execution of 25 suites
===> Started peer node 'ct_slave_pick_api_auth_SUITE_12345@hostname' for suite pick_api_auth_SUITE
===> Started peer node 'ct_slave_pick_flow_SUITE_67890@hostname' for suite pick_flow_SUITE
... (more nodes starting)
===> Suite pick_api_auth_SUITE: 15 passed, 0 failed, 0 skipped
===> Suite pick_flow_SUITE: 28 passed, 2 failed, 1 skipped
... (results from other suites)
===> Parallel execution completed:
===> Total passed: 450
===> Total failed: 5
===> Total skipped: 3
===> Failed suites: [pick_flow_SUITE, pick_mru_SUITE]
===> Results written to _build/test/logs/index.html.
EOF

echo ""
echo "Performance benefits:"
echo "- Suites run concurrently instead of sequentially"
echo "- Better resource utilization on multi-core systems"
echo "- Faster overall test execution time"
echo "- Isolated environments reduce inter-suite interference"

echo ""
echo "Configuration options that work with parallel:"
echo "- --cover (coverage compilation and aggregation)"
echo "- --verbose (detailed per-suite output)"
echo "- --logdir (custom log directory)"
echo "- --config (CT config files)"
echo "- All skip_* directives from spec files"

echo ""
echo "To test with a subset of your suites:"
echo "  rebar3 ct --parallel --dir ../apps/pick/test/ct/api"
echo "  rebar3 ct --parallel --suite pick_api_auth_SUITE,pick_flow_SUITE"

echo ""
echo "Ready to use! Your existing master.spec file will work as-is."
