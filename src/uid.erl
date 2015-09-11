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
  ,encode/1
  ,decode/1
]).
%% k-order utility
-export([
   gtol/1
  ,d/2
  ,t/1
  ,bind/1
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
-type(g()      ::  {uid, binary(), binary()}).
-type(k()      ::  {uid, node(), t(), id(), seq()}).
-type(t()      ::  {integer(), integer(), integer()}).
-type(id()     ::  integer()).
-type(seq()    ::  integer()).
-type(vclock() ::  [g()]).

%%%----------------------------------------------------------------------------   
%%%
%%% k-order interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% @doc
%% generate locally unique 64-bit k-order identifier
-spec(l/0 :: ()    -> l()).
-spec(l/1 :: (g()) -> l()).

l() -> 
   #uid{t = {A, B, C}, id = Id, seq = Seq} = k(),
   {uid, <<A:20, B:20, C:10, Id:4, Seq:10>>}.

l({uid, Node, Uid})
 when is_binary(Node) ->
   case ?CONFIG_UID:node() of
      Node ->
         {uid, Uid};
      _    ->
         exit(badarg)
   end;

l({uid, Node, Uid})
 when is_atom(Node) ->
   case erlang:node() of
      Node ->
         {uid, Uid};
      _    ->
         exit(badarg)
   end.

%%
%% @doc
%% generate globally unique 128-bit k-order identifier
-spec(g/0 :: () -> g()).
-spec(g/1 :: (l()) -> g()).

g() ->
   g(l()).

g({uid, Uid}) ->
   {uid, erlang:node(), Uid};

g({uid, _, _} = Uid) ->
   Uid.

%%
%% @doc
%% encode k-order number to binary format
-spec(encode/1 :: (l() | g()) -> binary()).

encode({uid, Uid}) ->
   Uid;
encode({uid, Node, Uid})
 when Node =:= erlang:node() ->
   <<(?CONFIG_UID:node())/binary, Uid/binary>>;
encode({uid, Node, Uid})
 when is_binary(Node) ->
   <<Node/binary, Uid/binary>>.

%%
%% @doc
%% decode k-order number from binary format
-spec(decode/1 :: (binary()) -> l() | g()).

decode(Uid)
 when byte_size(Uid) =:= 8 ->
   {uid, Uid};
decode(<<Node:4/binary, Uid:8/binary>>) ->
   case ?CONFIG_UID:node() of
      Node ->
         {uid, erlang:node(), Uid};
      _    ->
         {uid, Node, Uid}
   end;

decode(<<Node:8/binary, Uid:8/binary>>) ->
   case ?CONFIG_UID:node() of
      Node ->
         {uid, erlang:node(), Uid};
      _    ->
         {uid, Node, Uid}
   end.


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

gtol({uid, _Node, Uid}) ->
   {uid, Uid}.

%%
%% @doc
%% approximate distance between k-order values
-spec(d/2 :: (l() | g(), l() | g()) -> k()).

d({uid, A}, {uid, B}) ->
   {uid, d(A, B)}; 

d({uid, Node, A}, {uid, Node, B}) ->
   {uid, Node, d(A, B)};

d(<<A2:20, A1:20, A0:10, As:14>>, <<B2:20, B1:20, B0:10, Bs:14>>) ->
   A = {A2, A1, A0 * 1000},
   B = {B2, B1, B0 * 1000},
   <<(timer:now_diff(A,B) div 1000):50, (As - Bs):14>>.

%%
%% @doc
%% helper function to extract time-stamp in milliseconds from k-order value
-spec(t/1 :: (l() | g()) -> integer()).

t({uid, <<T:50, _:14>>}) ->
   T;
t({uid, _, <<T:50, _:14>>}) ->
   T.

%%
%% @doc
%% bind process with sequence
-spec(bind/1 :: (any()) -> ok).

bind(Id)
 when is_integer(Id) ->
   I = case Id rem ?SEQ of
      0 -> ?SEQ;
      X -> X
   end,
   {_, Pid, _, _} = lists:keyfind(I, 1, supervisor:which_children(uid_seq_sup)),
   erlang:put(uid_seq, Pid),
   ok;

bind(<<Id:4, _/bits>>) ->
   bind(Id).

%%%----------------------------------------------------------------------------   
%%%
%%% v-clock interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% create new v-clock
-spec(vclock/0 :: () -> vclock()).

vclock() ->
   [g()].

%%
%% increment v-clock
-spec(vclock/1 :: (vclock()) -> vclock()).

vclock(Vclock) ->
   case lists:keytake(erlang:node(), 2, Vclock) of
      false ->
         [g() | Vclock];
      {value, _, List} ->
         [g() | List]
   end.

%%
%% join two v-clock
-spec(join/2 :: (vclock(), vclock()) -> vclock()). 

join(A, B) ->
   do_join(lists:keysort(2, A), lists:keysort(2, B)).

do_join([{uid, NodeA, _}=X|A], [{uid, NodeB, _}|_]=B)
 when NodeA < NodeB ->
   [X | do_join(A, B)];

do_join([{uid, NodeA, _}|_]=A, [{uid, NodeB, _}=X|B])
 when NodeA > NodeB ->
   [X | do_join(A, B)];

do_join([X|A], [Y|B]) ->
   [erlang:max(X, Y) | do_join(A, B)];

do_join([], B) ->
   B;

do_join(A, []) ->
   A.

%%
%% return true if A v-clock is descend of B v-clock : A -> B 
-spec(descend/2 :: (vclock(), vclock()) -> boolean()).

descend(_, []) ->
   true;
descend(A, [{uid, Node, _} = X|B]) ->
   case lists:keyfind(Node, 2, A) of
      false ->
         false;
      Y  ->
         (X =< Y) andalso descend(A, B)
   end.

%%
%% return true if A clock is descend B with an exception to given peer 
%% the method allows to discover local conflicts
%%
%% Note: requires 96bit configuration schema
-spec(descend/3 :: (node(), vclock(), vclock()) -> boolean()).

descend(Node, A, B)
 when is_atom(Node) ->
   descend(lists:keydelete(Node, 2, A), lists:keydelete(Node, 2, B)).

%%
%% return difference of A clock to compare with B 
-spec(diff/2 :: (vclock(), vclock()) -> [node()]).

diff(_, []) ->
   [];
diff(A, [{uid, Node, _} = X|B]) ->
   case lists:keyfind(Node, 2, A) of
      false ->
         [X | diff(A, B)];
      Y when X =< Y  ->
         diff(A, B);
      Y ->
         [d(X, Y) | diff(A, B)]
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% @doc
%% generate unique k-order identifier
-spec(k/0 :: () -> k()).

k() ->
   gen_server:call(whereis(), seq, infinity).

%%
%% where is sequence
whereis() ->
   case erlang:get(uid_seq) of
      undefined ->
         Pid = gen_server:call(uid_master, pid, infinity),
         erlang:put(uid_seq, Pid),
         Pid;
      Pid when is_pid(Pid) ->
         Pid
   end.
