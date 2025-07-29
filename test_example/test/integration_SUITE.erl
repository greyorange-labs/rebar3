-module(integration_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [test_complex_calculation].

test_complex_calculation(_Config) ->
    %% Test combining operations
    Result = example_math:divide(example_math:multiply(example_math:add(2, 3), 4), 2),
    10.0 = Result,
    ct:pal("Complex calculation test passed: ~p", [Result]),
    timer:sleep(2000), % Simulate longer work
    ok.
