%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%%  @description
%%     k-ordered unique identity
-module(uid).

-export([
   l/0
  ,l/1
  ,g/0
  ,g/1
  ,d/2
]).

-export_type([l/0, g/0]).
-type(l()  ::  binary()).
-type(g()  ::  binary()).
-type(t()  ::  {integer(), integer(), integer()}).

-define(is_l(X), is_binary(X), byte_size(X) =:=  8).
-define(is_g(X), is_binary(X), byte_size(X) =:= 16).

%%
%% generate local 64-bit identity
-spec(l/0 :: () -> l()).
-spec(l/1 :: (g() | t()) -> l()).

l() ->
   l(erlang:now()).

l(<<X:8/binary, Node:16, _/binary>>=Global)
 when ?is_g(Global) -> 
   case erlang:phash(erlang:node(), 1 bsl 16) of
      Node ->
         X;
      _ ->
         exit(badarg)
   end;

l({A, B, C}) ->
   <<A:24, B:20, C:20>>.


%%
%% generate global 128-bit identity
-spec(g/0 :: () -> g()).
-spec(g/1 :: (l()) -> g()).

g() ->
   g(l()).

g(Local)
 when ?is_l(Local) ->
   {ok,  Id} = application:get_env(uid, worker),
   Node = erlang:phash(erlang:node(), 1 bsl 16),
   <<Local/binary, Node:16, Id/binary>>.

%%
%% approximate distance between uid
-spec(d/2 :: (l(), l()) -> integer()).

d(X, Y)
 when ?is_l(X), ?is_l(Y) ->
   <<A:64>> = X,
   <<B:64>> = Y,
   A - B.


