-module(recursion).
-export([fib/1, test_fib/0,
         pieces/1, test_pieces/0]).

%~ Fibonacci numbers

%~ The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, … where subsequent values are given by adding the two previous values in the sequence.

%~ Give a recursive definition of the function fib/1 computing the Fibonacci numbers, and give a step-by-step evaluation of fib(4).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when N>0 ->
    fib(N-2) + fib(N-1).

test_fib() ->
    0 = fib(0),
    1 = fib(1),
    1 = fib(2),
    2 = fib(3),
    3 = fib(4),
    5 = fib(5),
    ok.
    
% calculate fib(4) by hand

%~ fib(4)
    %~ fib(2) + fib(3)
    %~ 1 + 2
    %~ 3

%~ fib(3)
    %~ fib(1) + fib(2)
    %~ 1 + 1
    %~ 2
    
%~ fib(2)
    %~ fib(0) + fib(1)
    %~ 0 + 1
    %~ 1


%~ How many pieces?

%~ Define a function pieces so that pieces(N) tells you the maximum number
%~ of pieces into which you can cut a piece of paper with N straight line cuts.

%~ You can see an illustration of this problem at the top of this step.

%~ If you’d like to take this problem further, think about the 3-dimensional case.
%~ Into how many pieces can you cut a wooden block with N saw cuts?

%~ Taking it even further: What is the general problem in n dimensions?

pieces(0) ->
    1;
pieces(N) ->
    N + pieces(N-1).

test_pieces() ->
    1 = pieces(0),
    2 = pieces(1),
    4 = pieces(2),
    7 = pieces(3),
    11 = pieces(4),
    ok.
