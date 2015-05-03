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
-include("uid.hrl").

%% k-order interface
-export([
   l/0
  ,l/1
  ,g/0
  ,g/1
]).
%% k-order utility
-export([
   gtol/1
  ,d/2
  ,before/1
  ,before/2
  ,behind/1
  ,behind/2
]).
%% v-clock interface
-export([
   vclock/0,
   vclock/1,
   join/2,
   descend/2,
   descend/3,
   diff/2
]).


%%
%% data types
-export_type([l/0, g/0, vclock/0]).
-type(l()      ::  {uid, binary()}).
-type(g()      ::  {uid, binary()}).
-type(t()      ::  {integer(), integer(), integer()}).
-type(vclock() ::  [{node(), l()}]).

%%%----------------------------------------------------------------------------   
%%%
%%% k-order interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% @doc
%% generate locally unique 64-bit k-order identifier
-spec(l/0 :: () -> l()).
-spec(l/1 :: (g() | t()) -> l()).

l() -> l(erlang:now()).

l({A, B, C}) -> 
   {uid, <<A:24, B:20, C:20>>};

l({uid, Uid}=Global)
 when ?is_g(Uid) -> 
   ?GUID:l(Global).

%%
%% @doc
%% generate globally unique 128-bit k-order identifier
-spec(g/0 :: () -> g()).
-spec(g/1 :: (l()) -> g()).

g() ->
   g(l()).

g({uid, Local})
 when ?is_l(Local) ->
   ?GUID:g(Local);

g({uid, Global}=X) 
 when ?is_g(Global) ->
   X.

%%%----------------------------------------------------------------------------   
%%%
%%% k-order utility
%%%
%%%----------------------------------------------------------------------------   

%%
%% @doc
%% cast with force global to local k-order identifier
%% the operation do not guarantee uniqueness of the result
-spec(gtol/1 :: (g()) -> l()).

gtol({uid, Uid}=Global)
 when ?is_g(Uid) ->
   ?GUID:gtol(Global).


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
   ?GUID:i(X) - ?GUID:i(Y).

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
   {uid, <<A1:24, B1:20, C1:20>>}.

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
   {uid, <<A1:24, B1:20, C1:20>>}.

%%%----------------------------------------------------------------------------   
%%%
%%% v-clock interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% create new v-clock
-spec(vclock/0 :: () -> vclock()).

vclock() ->
   [{erlang:node(), l()}].

%%
%% increment v-clock
-spec(vclock/1 :: (vclock()) -> vclock()).

vclock(Vclock) ->
   Node = erlang:node(),
   case lists:keytake(Vclock, 1, Vclock) of
      false ->
         [{Node, l()} | Vclock];
      {value, _, List} ->
         [{Node, l()} | List]
   end.

%%
%% join two v-clock
-spec(join/2 :: (vclock(), vclock()) -> vclock()). 

join(A, B) ->
   do_join(lists:keysort(1, A), lists:keysort(1, B)).

do_join([{NodeA, X}|A], [{NodeB, _}|_]=B)
 when NodeA < NodeB ->
   [{NodeA, X} | do_join(A, B)];

do_join([{NodeA, _}|_]=A, [{NodeB, X}|B])
 when NodeA > NodeB ->
   [{NodeB, X} | do_join(A, B)];

do_join([{Node, X}|A], [{Node, Y}|B]) ->
   [{Node, erlang:max(X, Y)} | do_join(A, B)];

do_join([], B) ->
   B;

do_join(A, []) ->
   A.

%%
%% return true if A v-clock is descend of B v-clock : A -> B 
-spec(descend/2 :: (vclock(), vclock()) -> boolean()).

descend(_, []) ->
   true;
descend(A, [{Node, X}|B]) ->
   case lists:keyfind(Node, 1, A) of
      false ->
         (X =< 0) andalso descend(A, B);
      {_, Y}  ->
         (X =< Y) andalso descend(A, B)
   end.

%%
%% return true if A clock is descend B with an exception to given peer 
%% the method allows to discover local conflicts
-spec(descend/3 :: (node(), vclock(), vclock()) -> boolean()).

descend(Node, A, B) ->
   descend(lists:keydelete(Node, 1, A), lists:keydelete(Node, 1, B)).

%%
%% return difference of A clock to compare with B 
-spec(diff/2 :: (vclock(), vclock()) -> [node()]).

diff(_, []) ->
   [];
diff(A, [{Node, X}|B]) ->
   case lists:keyfind(Node, 1, A) of
      false ->
         [{Node, X} | diff(A, B)];
      {_, Y} when X =< Y  ->
         diff(A, B);
      {_, Y} ->
         [{Node, d(X, Y)} | diff(A, B)]
   end.



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

