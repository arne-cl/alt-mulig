-module(second).
-export([area/2, perimeter/3]).

-import(first, [mult/2]).

% area of a right-angled triagle
area(A,B) ->
    first:mult(A, B) / 2.

% perimeter (Umfang) of a right-angled triangle
perimeter(A,B,H) ->
    A+B+H.
    
