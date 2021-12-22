-module(simple).
-export([howManyEqual/2, exOr/2]).

howManyEqual(X,X) ->
    2;
howManyEqual(_X,_Y) ->
    0.

% WARNING: this only works if X is a boolean!
%
% e.g. if X is 3, then exOr(false,3) will return 3 and exOr(true,3)
% will return an error
exOr(true,X) ->
    not(X);
exOr(false,X) ->
    X.
