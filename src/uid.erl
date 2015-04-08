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
  ,before/2
  ,behind/1
  ,behind/2
]).

%%
%% data types
-export_type([l/0, g/0]).
-type(l()  ::  {uid, binary()}).
-type(g()  ::  {uid, binary()}).
-type(t()  ::  {integer(), integer(), integer()}).

-define(is_l(X), is_binary(X), byte_size(X) =:=  8).
-define(is_g(X), is_binary(X), byte_size(X) =:= 16).
-define(BASE,    1000000).

%%%----------------------------------------------------------------------------   
%%%
%%% unique identifier
%%%
%%%----------------------------------------------------------------------------   

%%
%% @doc
%% generate locally unique 64-bit k-order identifier
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
%% @doc
%% generate globally unique 128-bit k-order identifier
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

%%%----------------------------------------------------------------------------   
%%%
%%% unique identifier utility
%%%
%%%----------------------------------------------------------------------------   

%%
%% @doc
%% cast with force global to local k-order identifier
%% the operation do not guarantee uniqueness of the result
-spec(gtol/1 :: (g()) -> l()).

gtol({uid, <<_:6/binary, _:16, X:8/binary>>=Global})
 when ?is_g(Global) ->
   {uid, X}.

%%
%% @doc
%% approximate distance between identifiers
-spec(d/2 :: (l(), l()) -> integer()).

d({uid, X}, {uid, Y})
 when ?is_l(X), ?is_l(Y) ->
   <<A:64>> = X,
   <<B:64>> = Y,
   A - B;

d({uid, X}, {uid, Y})
 when ?is_g(X), ?is_g(Y) ->
   <<Prefix:8/binary, A:64>> = X,
   <<Prefix:8/binary, B:64>> = Y,
   A - B.

%%
%% @doc
%% approximate a new k-order in the distance before given one 
-spec(before/1 :: (l()) -> l()).
-spec(before/2 :: (l(), integer()) -> l()).

before(Uid) ->
   before(Uid, 1).

before({uid, X}, D)
 when ?is_l(X) ->
   <<A0:24, B0:20, C0:20>> = X,
   {A, B, C} = int(D),
   {C1,  Q0} = sub(C0,  C, B0),
   {B1,  Q1} = sub(Q0,  B, A0),
   {A1,   0} = sub(Q1,  A,  0),
   {uid, <<A1:24, B1:20, C1:20>>};

before({uid, X}, I)
 when ?is_g(X) ->
   <<Prefix:8/binary, A0:24, B0:20, C0:20>> = X,
   {A, B, C} = int(I),
   {C1,  Q0} = sub(C0,  C, B0),
   {B1,  Q1} = sub(Q0,  B, A0),
   {A1,   0} = sub(Q1,  A,  0),
   {uid, <<Prefix:8/binary, A1:24, B1:20, C1:20>>}.

%%
%% @doc
%% approximate a new k-order in the distance behind given one 
-spec(behind/1 :: (l()) -> l()).
-spec(behind/2 :: (l(), integer()) -> l()).

behind(Uid) ->
   behind(Uid, 1).

behind({uid, X}, I)
 when ?is_l(X) ->
   <<A0:24, B0:20, C0:20>> = X,
   {C1, Q0} = add(C0, I,  0),
   {B1, Q1} = add(B0, 0, Q0),
   {A1,  _} = add(A0, 0, Q1),
   {uid, <<A1:24, B1:20, C1:20>>};

behind({uid, X}, I)
 when ?is_g(X) ->
   <<Prefix:8/binary, A0:24, B0:20, C0:20>> = X,
   {C1, Q0} = add(C0, I,  0),
   {B1, Q1} = add(B0, 0, Q0),
   {A1,  _} = add(A0, 0, Q1),
   {uid, <<Prefix:8/binary, A1:24, B1:20, C1:20>>}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

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

%%
%% split integer to triplet
int(I)
 when I < ?BASE ->
   {0, 0, I};

int(I) ->
   C = I rem ?BASE,
   B = (I div ?BASE) rem ?BASE,
   A = (I div ?BASE) div ?BASE,
   {A, B, C}.  

