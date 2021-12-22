-module(week2).
-export([perimeter/1, area/1, enclose/1]).

%~ Shapes

%~ Define a function perimeter/1 which takes a shape and returns the 
%~ perimeter of the shape.

%~ Choose a suitable representation of triangles, and augment area/1 and 
%~ perimeter/1 to handle this case too.

%~ Define a function enclose/1 that takes a shape and returns the smallest 
%~ enclosing rectangle of the shape.



area({circle, {_X,_Y}, R}) ->
    math:pi()*R*R;
area({rectangle, {_X,_Y}, H, W}) -> % H: heigt, W: width
    H+W.
%~ area({triangle, {_X,_Y}, A, B, C) -> % A,B,C: length of the sides
    %~ (A*Height_A)/2

% perimeter: Umfang
perimeter({circle, {_X,_Y}, R}) ->
    2*math:pi()*R;
perimeter({rectangle, {_X,_Y}, H, W}) ->
    2*(H+W).






%~ Summing the bits

%~ Define a function bits/1 that takes a positive integer N and returns 
%~ the sum of the bits in the binary representation. For example bits(7) 
%~ is 3 and bits(8) is 1.

%~ See whether you can make both a direct recursive and a tail recursive 
%~ definition.

%~ Which do you think is better? Why?

bits(N) when N>0 ->
    string:length(integer_to_list(N,2)).
