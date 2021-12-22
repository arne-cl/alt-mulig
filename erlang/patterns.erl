-module(patterns).
% -export([double/1, mult/2, area/3, square/1, treble/1]).
- export([xor1/2, xor2/2, xor3/2, test_xor/0, maxThree/3, test_maxThree/0, equal/2, count_equals/3, howManyEqual/3, test_howManyEqual/0]).


% exclusive or (xor)

% =/= and == are the operations for inequality and equality in Erlang;
% not is the Erlang negation function; and,
% and and or are the Erlang conjunction and disjunction (infix) operators.

xor1(A,B) ->
    A =/= B.

xor2(A,B) ->
    not(A == B).

xor3(A,B) ->
    A and not(B) or not(A) and B.


% Maximum of three

% Give a definition of the function maxThree which takes three integers
% and returns the maximum of the three. 
% You can use the max function, which gives the maximum of two numbers,
% in writing your definition.
% maxThree(34,25,36) = 36
maxThree(A,B,C) ->
    Tmp = max(A,B),
    max(Tmp,C).


% How many equal?
%
% Give a definition of the function howManyEqual which takes three integers
% and returns an integer, counting how many of its three arguments are equal, so that:
%
% howManyEqual(34,25,36) = 0
% howManyEqual(34,25,34) = 2
% howManyEqual(34,34,34) = 3
equal(X,Y) ->
    X =:= Y. % same type and value

count_equals(true,true,true) ->
    3;
count_equals(true,false,false) ->
    2;
count_equals(false,true,false) ->
    2;
count_equals(false,false,true) ->
    2;
count_equals(false,false,false) ->
    0.

howManyEqual(A,B,C) ->
    count_equals(equal(A,B), equal(A,C), equal(B,C)).




% tests

%% tests xor functions
test_xor() ->
    false = xor1(true,true),
    false = xor2(true,true),
    false = xor3(true,true),

    false = xor1(false,false),
    false = xor2(false,false),
    false = xor3(false,false),

    true = xor1(true,false),
    true = xor2(true,false),
    true = xor3(true,false),    

    true = xor1(false,true),
    true = xor2(false,true),
    true = xor3(false,true),
    ok.

test_maxThree() ->
    3 = maxThree(1,2,3),
    3 = maxThree(2,1,3),
    3 = maxThree(3,1,2),
    3 = maxThree(3,2,1),
    3 = maxThree(1,3,2),
    3 = maxThree(2,3,1),
    3 = maxThree(3,3,3),
    10 = maxThree(10,0,-5),
    ok.

test_howManyEqual() ->
    0 = howManyEqual(1,2,3),
    0 = howManyEqual(-15,0,15),
    2 = howManyEqual(1,1,3),
    2 = howManyEqual(3,1,3),
    2 = howManyEqual(1,3,3),
    3 = howManyEqual(1,1,1),
    3 = howManyEqual("a","a","a"),
    2 = howManyEqual("a","a","b"),
    0 = howManyEqual("a","b","c"),
    ok.
