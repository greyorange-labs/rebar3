-module(math_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [test_addition, test_multiplication].

test_addition(_Config) ->
    3 = example_math:add(1, 2),
    0 = example_math:add(-1, 1),
    ct:pal("Addition tests passed"),
    timer:sleep(1000), % Simulate some work
    ok.

test_multiplication(_Config) ->
    4 = example_math:multiply(2, 2),
    0 = example_math:multiply(0, 5),
    ct:pal("Multiplication tests passed"),
    timer:sleep(1000), % Simulate some work
    ok.
