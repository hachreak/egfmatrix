%% Copyright (C) 2015 Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%
%% This source code is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% This source code is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this source code; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-module(galoisfield_8).
-export([sum/2, sub/2, mul/2, divide/2]).
-include_lib("eunit/include/eunit.hrl").

%%
%% Galois Field - GF(2^3)
%%
%% This is the implementation of basic math for this kind of mathematical field.
%%
%% Finite fields are fundamental in a number of areas of mathematics and
%% computer science, including number theory, algebraic geometry, Galois
%% theory, finite geometry, cryptography and coding theory.
%%
%% @see http://en.wikipedia.org/wiki/Finite_field
%%

-define(ZERO, 7).
-define(ONE, 0).
-define(Q_1, ?ZERO).

zechlogs(1) ->
  3;
zechlogs(2) ->
  6;
zechlogs(3) ->
  1;
zechlogs(4) ->
  5;
zechlogs(5) ->
  4;
zechlogs(6) ->
  2.

convertToRepr(?ZERO) ->
  ?Q_1;
convertToRepr(?ONE) ->
  0;
convertToRepr(A) ->
  A.

convertToSymbol(0) ->
  ?ONE;
convertToSymbol(?Q_1) ->
  ?ZERO;
convertToSymbol(A) ->
  A.

sum_(A, B) when A == B ->
  ?ZERO;
sum_(?ZERO, B) ->
  B;
sum_(A, ?ZERO) ->
  A;
sum_(?ONE, B) ->
  zechlogs(B);
sum_(A, ?ONE) ->
  zechlogs(A);
sum_(A, B) when A /= B ->
  Zecharg = (?Q_1 + B - A) rem ?Q_1,
  (zechlogs(Zecharg) + A) rem ?Q_1.

sum(A, B) ->
  convertToRepr(sum_(convertToSymbol(A), convertToSymbol(B))).

sub(A, B) ->
  sum(A, B).

mul_(?ZERO, _) ->
  ?ZERO;
mul_(_, ?ZERO) ->
  ?ZERO;
mul_(?ONE, B) ->
  B;
mul_(A, ?ONE) ->
  A;
mul_(A, B) ->
  (A + B) rem ?Q_1.

mul(A, B) ->
  convertToRepr(mul_(convertToSymbol(A), convertToSymbol(B))).

div_(?ZERO, _) ->
  ?ZERO;
div_(_, ?ZERO) ->
  erlang:error("Division by Zero");
div_(?ONE, ?ONE) ->
  ?ONE;
div_(?ONE, B) ->
  ?Q_1 - B;
div_(A, B) ->
  (?Q_1 + A - B) rem ?Q_1.

divide(A, B) ->
  convertToRepr(div_(convertToSymbol(A), convertToSymbol(B))).

% Example:
%
% main(X, Y) ->
%  lists:foreach(
%    fun(A)->
%        lists:foreach(
%          fun(B)->
%              io:format("~w = ~w~n",[[A - 1,B - 1], divide(A - 1, B - 1)])
%          end, lists:seq(1, X))
%    end, lists:seq(1, Y)).

sum_test() ->
  [?assert(sum(0, 0) == 7),
  ?assert(sum(0, 1) == 3),
  ?assert(sum(0, 2) == 6),
  ?assert(sum(0, 3) == 1),
  ?assert(sum(0, 4) == 5),
  ?assert(sum(0, 5) == 4),
  ?assert(sum(0, 6) == 2),
  ?assert(sum(1, 0) == 3),
  ?assert(sum(1, 1) == 7),
  ?assert(sum(1, 2) == 4),
  ?assert(sum(1, 3) == 0),
  ?assert(sum(1, 4) == 2),
  ?assert(sum(1, 5) == 6),
  ?assert(sum(1, 6) == 5),
  ?assert(sum(2, 0) == 6),
  ?assert(sum(2, 1) == 4),
  ?assert(sum(2, 2) == 7),
  ?assert(sum(2, 3) == 5),
  ?assert(sum(2, 4) == 1),
  ?assert(sum(2, 5) == 3),
  ?assert(sum(2, 6) == 0),
  ?assert(sum(3, 0) == 1),
  ?assert(sum(3, 1) == 0),
  ?assert(sum(3, 2) == 5),
  ?assert(sum(3, 3) == 7),
  ?assert(sum(3, 4) == 6),
  ?assert(sum(3, 5) == 2),
  ?assert(sum(3, 6) == 4),
  ?assert(sum(4, 0) == 5),
  ?assert(sum(4, 1) == 2),
  ?assert(sum(4, 2) == 1),
  ?assert(sum(4, 3) == 6),
  ?assert(sum(4, 4) == 7),
  ?assert(sum(4, 5) == 0),
  ?assert(sum(4, 6) == 3),
  ?assert(sum(5, 0) == 4),
  ?assert(sum(5, 1) == 6),
  ?assert(sum(5, 2) == 3),
  ?assert(sum(5, 3) == 2),
  ?assert(sum(5, 4) == 0),
  ?assert(sum(5, 5) == 7),
  ?assert(sum(5, 6) == 1),
  ?assert(sum(6, 0) == 2),
  ?assert(sum(6, 1) == 5),
  ?assert(sum(6, 2) == 0),
  ?assert(sum(6, 3) == 4),
  ?assert(sum(6, 4) == 3),
  ?assert(sum(6, 5) == 1),
  ?assert(sum(6, 6) == 7),
  ?assert(mul(0, 0) == 0),
  ?assert(mul(0, 1) == 1),
  ?assert(mul(0, 2) == 2),
  ?assert(mul(0, 3) == 3),
  ?assert(mul(0, 4) == 4),
  ?assert(mul(0, 5) == 5),
  ?assert(mul(0, 6) == 6),
  ?assert(mul(1, 0) == 1),
  ?assert(mul(1, 1) == 2),
  ?assert(mul(1, 2) == 3),
  ?assert(mul(1, 3) == 4),
  ?assert(mul(1, 4) == 5),
  ?assert(mul(1, 5) == 6),
  ?assert(mul(1, 6) == 0),
  ?assert(mul(2, 0) == 2),
  ?assert(mul(2, 1) == 3),
  ?assert(mul(2, 2) == 4),
  ?assert(mul(2, 3) == 5),
  ?assert(mul(2, 4) == 6),
  ?assert(mul(2, 5) == 0),
  ?assert(mul(2, 6) == 1),
  ?assert(mul(3, 0) == 3),
  ?assert(mul(3, 1) == 4),
  ?assert(mul(3, 2) == 5),
  ?assert(mul(3, 3) == 6),
  ?assert(mul(3, 4) == 0),
  ?assert(mul(3, 5) == 1),
  ?assert(mul(3, 6) == 2),
  ?assert(mul(4, 0) == 4),
  ?assert(mul(4, 1) == 5),
  ?assert(mul(4, 2) == 6),
  ?assert(mul(4, 3) == 0),
  ?assert(mul(4, 4) == 1),
  ?assert(mul(4, 5) == 2),
  ?assert(mul(4, 6) == 3),
  ?assert(mul(5, 0) == 5),
  ?assert(mul(5, 1) == 6),
  ?assert(mul(5, 2) == 0),
  ?assert(mul(5, 3) == 1),
  ?assert(mul(5, 4) == 2),
  ?assert(mul(5, 5) == 3),
  ?assert(mul(5, 6) == 4),
  ?assert(mul(6, 0) == 6),
  ?assert(mul(6, 1) == 0),
  ?assert(mul(6, 2) == 1),
  ?assert(mul(6, 3) == 2),
  ?assert(mul(6, 4) == 3),
  ?assert(mul(6, 5) == 4),
  ?assert(mul(6, 6) == 5),
  ?assert(divide(0, 0) == 0),
  ?assert(divide(0, 1) == 6),
  ?assert(divide(0, 2) == 5),
  ?assert(divide(0, 3) == 4),
  ?assert(divide(0, 4) == 3),
  ?assert(divide(0, 5) == 2),
  ?assert(divide(0, 6) == 1),
  ?assert(divide(1, 0) == 1),
  ?assert(divide(1, 1) == 0),
  ?assert(divide(1, 2) == 6),
  ?assert(divide(1, 3) == 5),
  ?assert(divide(1, 4) == 4),
  ?assert(divide(1, 5) == 3),
  ?assert(divide(1, 6) == 2),
  ?assert(divide(2, 0) == 2),
  ?assert(divide(2, 1) == 1),
  ?assert(divide(2, 2) == 0),
  ?assert(divide(2, 3) == 6),
  ?assert(divide(2, 4) == 5),
  ?assert(divide(2, 5) == 4),
  ?assert(divide(2, 6) == 3),
  ?assert(divide(3, 0) == 3),
  ?assert(divide(3, 1) == 2),
  ?assert(divide(3, 2) == 1),
  ?assert(divide(3, 3) == 0),
  ?assert(divide(3, 4) == 6),
  ?assert(divide(3, 5) == 5),
  ?assert(divide(3, 6) == 4),
  ?assert(divide(4, 0) == 4),
  ?assert(divide(4, 1) == 3),
  ?assert(divide(4, 2) == 2),
  ?assert(divide(4, 3) == 1),
  ?assert(divide(4, 4) == 0),
  ?assert(divide(4, 5) == 6),
  ?assert(divide(4, 6) == 5),
  ?assert(divide(5, 0) == 5),
  ?assert(divide(5, 1) == 4),
  ?assert(divide(5, 2) == 3),
  ?assert(divide(5, 3) == 2),
  ?assert(divide(5, 4) == 1),
  ?assert(divide(5, 5) == 0),
  ?assert(divide(5, 6) == 6),
  ?assert(divide(6, 0) == 6),
  ?assert(divide(6, 1) == 5),
  ?assert(divide(6, 2) == 4),
  ?assert(divide(6, 3) == 3),
  ?assert(divide(6, 4) == 2),
  ?assert(divide(6, 5) == 1),
  ?assert(divide(6, 6) == 0)].
