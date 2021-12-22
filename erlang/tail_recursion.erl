-module(tail_recursion).
-export([fac/1, test_fac/0,
         loop/1,
         fib/1, fib/3, test_fib/0,
         perfect/1, perfect/3, test_perfect/0]).

% factorial
fac(N) ->
    fac(N,1).
    
fac(0,P) ->
    P;
fac(N,P) when N>0 ->
    fac(N-1,P*N).

test_fac() ->
    1 = fac(0),
    1 = fac(1),
    2 = fac(2),
    6 = fac(3),
    24 = fac(4),
    ok.


% countdown loop from N to 1 
loop(N) when N>0 ->
    % io:format - each ~p replaced w/ corresponding value from a list
    % io:format - each ~n replaced w/ newline
    io:format("~p~n", [N]),
    loop(N-1);

loop(_N) ->
    io:format("bye~n").


%~ Fibonacci numbers

%~ The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, … where subsequent 
%~ values are given by adding the two previous values in the sequence.

%~ The function fib/1 that we defined earlier is exponentially complex … 
%~ ouch! Define an efficient Fibonacci function fib/3 using a tail 
%~ recursion with two accumulating parameters that hold the last two 
%~ Fibonacci numbers. Give a step-by-step evaluation of fib(4).

fib(N) ->
    fib(N, 0, 1).
    
fib(N, Last, Penultimate) when N>0 ->
    fib(N-1, Penultimate, Last+Penultimate);
fib(0, P, _) ->
    P.


test_fib() ->
    0 = fib(0),
    1 = fib(1),
    1 = fib(2),
    2 = fib(3),
    3 = fib(4),
    5 = fib(5),
    ok.

%~ fib(4)
    %~ fib(4,0,1)
        %~ fib(3,1,1)
            %~ fib(2,1,2)
                %~ fib(1,2,3)
                    %~ fib(0,3,5)
                        %~ 3


%~ Perfect numbers

%~ A positive integer is perfect when it is the sum of its divisors, e.g. 
%~ 6=1+2+3, 28=1+2+4+7+14.

%~ Define a function perfect/1 that takes a positive number N and returns 
%~ a boolean which indicates whether or not the number is perfect. You may 
%~ well want to use an accumulating parameter to hold the sum of the 
%~ divisors “so far”.
perfect(N) ->
    perfect(N,1,0).

perfect(N,N,S) ->
    N==S;
perfect(N,M,S) when N rem M == 0 ->
    perfect(N,M+1,S+M);
perfect(N,M,S) ->
    perfect(N,M+1,S).

test_perfect() ->
    false = perfect(5),
    true = perfect(6),
    true = perfect(28),
    ok.


