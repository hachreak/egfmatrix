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
