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
%%
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

-module(galoisfield).
-export([load_file/0, sum/3, sub/3, mul/2, divide/2]).
-include_lib("eunit/include/eunit.hrl").

% Possible values:
% to use GF(2^3).
% -define(TYPE, 8).
% to use GF(2^8).
% -define(TYPE, 256).
% default is GF(2^16).
-define(TYPE, 65536).

-define(ZERO, ?TYPE - 1).
-define(ONE, 0).
-define(Q_1, ?ZERO).

load_file() ->
  {ok, S} = file:open(io_lib:format('../GaloisField/ZechLogGF~w.txt', [?TYPE]), read),
  read_line(S, []).

read_line(S, List) ->
  case io:get_line(S, '') of
    eof ->
      % list is full, load dictionary!
      dict:from_list(List);
    Line ->
      % Load key/value
      [SKey, SValue] = string:tokens(Line,"\n\t"),
      {Key, _} = string:to_integer(SKey),
      {Value, _} = string:to_integer(SValue),
      % Load next row
      read_line(S, lists:append([{Key, Value}], List))
  end.

zechlogs(ZechLogsDict, Num) ->
  {ok, Ret} = dict:find(Num, ZechLogsDict),
  Ret.

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

sum_(_, A, B) when A == B ->
  ?ZERO;
sum_(_, ?ZERO, B) ->
  B;
sum_(_, A, ?ZERO) ->
  A;
sum_(ZechLogsDict, ?ONE, B) ->
  zechlogs(ZechLogsDict, B);
sum_(ZechLogsDict, A, ?ONE) ->
  zechlogs(ZechLogsDict, A);
sum_(ZechLogsDict, A, B) when A /= B ->
  Zecharg = (?Q_1 + B - A) rem ?Q_1,
  (zechlogs(ZechLogsDict, Zecharg) + A) rem ?Q_1.

sum(ZechLogsDict, A, B) ->
  convertToRepr(sum_(ZechLogsDict, convertToSymbol(A), convertToSymbol(B))).

sub(ZechLogsDict, A, B) ->
  sum(ZechLogsDict, A, B).

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
% ZechList = galoisfield:load_file().
% galoisfield:sum(ZechList, Number_A, Number_B).
% galoisfield:sub(ZechList, Number_A, Number_B).
% galoisfield:mul(Number_A, Number_B).
% galoisfield:divide(Number_A, Number_B).
