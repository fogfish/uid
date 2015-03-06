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
  ,gtol/1
  ,d/2
  ,before/1
  ,behind/1
]).

-export_type([l/0, g/0]).
-type(l()  ::  {uid, binary()}).
-type(g()  ::  {uid, binary()}).
-type(t()  ::  {integer(), integer(), integer()}).

-define(is_l(X), is_binary(X), byte_size(X) =:=  8).
-define(is_g(X), is_binary(X), byte_size(X) =:= 16).
-define(BASE,    1000000).
%%
%% generate local 64-bit identity
-spec(l/0 :: () -> l()).
-spec(l/1 :: (g() | t()) -> l()).

l() ->
   l(erlang:now()).

l({uid, <<_:6/binary, Node:16, X:8/binary>>=Global})
 when ?is_g(Global) -> 
   case erlang:phash(erlang:node(), 1 bsl 16) of
      Node ->
         {uid, X};
      _ ->
         exit(badarg)
   end;

l({A, B, C}) ->
   {uid, <<A:24, B:20, C:20>>}.


%%
%% generate global 128-bit identity
-spec(g/0 :: () -> g()).
-spec(g/1 :: (l()) -> g()).

g() ->
   g(l()).

g({uid, Local})
 when ?is_l(Local) ->
   {ok,  Id} = application:get_env(uid, worker),
   Node = erlang:phash(erlang:node(), 1 bsl 16),
   {uid, <<Id:6/binary, Node:16, Local/binary>>};

g({uid, Global}=X) 
 when ?is_g(Global) ->
   X.

%%
%% cast global to local uid with force
-spec(gtol/1 :: (g()) -> l()).

gtol({uid, <<_:6/binary, _:16, X:8/binary>>=Global})
 when ?is_g(Global) ->
   {uid, X}.

%%
%% approximate distance between uid
-spec(d/2 :: (l(), l()) -> integer()).

d({uid, X}, {uid, Y})
 when ?is_l(X), ?is_l(Y) ->
   <<A:64>> = X,
   <<B:64>> = Y,
   A - B.

%%
%% approximate k-order before given one 
-spec(before/1 :: (l()) -> l()).

before({uid, X})
 when ?is_l(X) ->
   <<A0:24, B0:20, C0:20>> = X,
   {C1, Q0} = sub(C0,  1, B0),
   {B1, Q1} = sub(Q0,  0, A0),
   {A1,  0} = sub(Q1,  0,  0),
   l({A1, B1, C1}).

%%
%% approximate k-order behind given one
-spec(behind/1 :: (l()) -> l()).

behind({uid, X})
 when ?is_l(X) ->
   <<A0:24, B0:20, C0:20>> = X,
   {C1, Q0} = add(C0, 1,  0),
   {B1, Q1} = add(B0, 0, Q0),
   {A1,  _} = add(A0, 0, Q1),
   l({A1, B1, C1}).


%%
%% arithmetic with carry
add(X, Y, Q) ->
   T = X + Y + Q,
   {T rem ?BASE, T div ?BASE}.   

sub(X, Y, A)
 when X >=Y ->
   {X - Y, A};

sub(X, Y, A) ->
   {?BASE + X - Y, A - 1}.

