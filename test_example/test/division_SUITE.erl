-module(division_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [test_division, test_division_by_zero].

test_division(_Config) ->
    2.0 = example_math:divide(4, 2),
    0.5 = example_math:divide(1, 2),
    ct:pal("Division tests passed"),
    timer:sleep(1500), % Simulate some work
    ok.

test_division_by_zero(_Config) ->
    ct:pal("Testing division by zero handling"),
    timer:sleep(800), % Simulate some work
    %% This would normally test error handling
    ok.
