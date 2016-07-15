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
-spec l()    -> l().
-spec l(g()) -> l().

l() -> 
   k().


l({uid, Node, T, Seq})
 when is_binary(Node) ->
   case ?CONFIG_UID:node() of
      Node ->
         {uid, T, Seq};
      _    ->
         exit(badarg)
   end;

l({uid, Node, T, Seq})
 when is_atom(Node) ->
   case erlang:node() of
      Node ->
         {uid, T, Seq};
      _    ->
         exit(badarg)
   end.

%%
%% @doc
%% generate globally unique 128-bit k-order identifier
-spec g() -> g().
-spec g(l()) -> g().

g() ->
   g(l()).

g({uid, T, Seq}) ->
   {uid, erlang:node(), T, Seq};

g({uid, _, _, _} = Uid) ->
   Uid.

%%
%% @doc
%% encode k-order number to binary format
-spec encode(l() | g()) -> binary().

encode({uid, {A, B, C}, Seq}) ->
   <<A:20, B:20, C:10, Seq:14>>;
encode({uid, Node, {A, B, C}, Seq})
 when Node =:= erlang:node() ->
   <<(?CONFIG_UID:node())/binary, A:20, B:20, C:10, Seq:14>>;
encode({uid, Node, {A, B, C}, Seq})
 when is_binary(Node) ->
   <<Node/binary, A:20, B:20, C:10, Seq:14>>.

%%
%% @doc
%% decode k-order number from binary format
-spec decode(binary()) -> l() | g().

decode(<<A:20, B:20, C:10, Seq:14>>) ->
   {uid, {A, B, C}, Seq};
decode(<<Node:4/binary, A:20, B:20, C:10, Seq:14>>) ->
   case ?CONFIG_UID:node() of
      Node ->
         {uid, erlang:node(), {A, B, C}, Seq};
      _    ->
         {uid, Node, {A, B, C}, Seq}
   end;

decode(<<Node:8/binary, A:20, B:20, C:10, Seq:14>>) ->
   case ?CONFIG_UID:node() of
      Node ->
         {uid, erlang:node(), {A, B, C}, Seq};
      _    ->
         {uid, Node, {A, B, C}, Seq}
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
-spec gtol(g()) -> l().

gtol({uid, _Node, T, Seq}) ->
   {uid, T, Seq}.

%%
%% @doc
%% approximate distance between k-order values
-spec d(l() | g(), l() | g()) -> k().

d({uid, A, Sa}, {uid, B, Sb}) ->
   {uid, d(A, B), Sa - Sb}; 

d({uid, Node, A, Sa}, {uid, Node, B, Sb}) ->
   {uid, Node, d(A, B), Sa - Sb};

d({A2, A1, A0}, {B2, B1, B0}) ->
   X = timer:now_diff({A2, A1, A0}, {B2, B1, B0}),
   C = X rem ?BASE,
   Y = X div ?BASE,
   B = Y rem ?BASE,
   A = Y div ?BASE,
   {A, B, C}.

%%
%% @doc
%% helper function to extract time-stamp in milliseconds from k-order value
-spec t(l() | g()) -> integer().

t({uid, {A, B, C}, _}) ->
   (A * ?BASE + B) * 1000 + C div 1000;
t({uid, _, {A, B, C}, _}) ->
   (A * ?BASE + B) * 1000 + C div 1000.

%%
%% @doc
%% bind process with sequence
-spec bind(any()) -> ok.

-ifndef(CONFIG_NATIVE).
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
-else.
bind(_) ->
   ok.
-endif.

%%%----------------------------------------------------------------------------   
%%%
%%% v-clock interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% create new v-clock
-spec vclock() -> vclock().

vclock() ->
   [g()].

%%
%% increment v-clock
-spec vclock(vclock()) -> vclock().

vclock(Vclock) ->
   case lists:keytake(erlang:node(), 2, Vclock) of
      false ->
         [g() | Vclock];
      {value, _, List} ->
         [g() | List]
   end.

%%
%% join two v-clock
-spec join(vclock(), vclock()) -> vclock(). 

join(A, B) ->
   do_join(lists:keysort(2, A), lists:keysort(2, B)).

do_join([{uid, NodeA, _, _}=X|A], [{uid, NodeB, _, _}|_]=B)
 when NodeA < NodeB ->
   [X | do_join(A, B)];

do_join([{uid, NodeA, _, _}|_]=A, [{uid, NodeB, _, _}=X|B])
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
-spec descend(vclock(), vclock()) -> boolean().

descend(_, []) ->
   true;
descend(A, [{uid, Node, _, _} = X|B]) ->
   case lists:keyfind(Node, 2, A) of
      false ->
         false;
      Y  ->
         (X =< Y) andalso descend(A, B)
   end.

%%
%% return true if A clock is descend B with an exception to given peer 
%% the method allows to discover local conflicts
-spec descend(node(), vclock(), vclock()) -> boolean().

descend(Node, A, B)
 when is_atom(Node) ->
   descend(lists:keydelete(Node, 2, A), lists:keydelete(Node, 2, B)).

%%
%% return difference of A clock to compare with B 
-spec diff(vclock(), vclock()) -> [node()].

diff(_, []) ->
   [];
diff(A, [{uid, Node, _, _} = X|B]) ->
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
-spec k() -> k().

-ifndef(CONFIG_NATIVE).
k() ->
   #uid{t = T, id = Id, seq = Seq} = gen_server:call(whereis(), seq, infinity),
   {uid, T, (Id bsl 10) + Seq}.
-else.
k() ->
   {uid, os:timestamp(), erlang:unique_integer([monotonic])}.
-endif.


%%
%% where is sequence
-ifndef(CONFIG_NATIVE).
whereis() ->
   case erlang:get(uid_seq) of
      undefined ->
         Pid = gen_server:call(uid_master, pid, infinity),
         erlang:put(uid_seq, Pid),
         Pid;
      Pid when is_pid(Pid) ->
         Pid
   end.
-endif.

