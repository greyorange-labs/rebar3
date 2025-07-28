-module(example_math).
-export([add/2, multiply/2, divide/2]).

add(A, B) -> A + B.
multiply(A, B) -> A * B.
divide(A, B) when B =/= 0 -> A / B.
