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
   z/0
  ,l/0
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
-export_type([uid/0, l/0, g/0, vclock/0]).
-type uid()    :: l() | g().
-type l()      :: {uid, lt(), seq()}.
-type g()      :: {uid, gt(), seq()}.

-type lt()     :: {integer(), integer(), integer(), integer()}.
-type gt()     :: {integer(), integer(), id(), integer(), integer()}.
-type seq()    :: <<_:14>>.
-type id()     :: node() | <<_:32>>.

-type vclock() ::  [g()].

%%%----------------------------------------------------------------------------   
%%%
%%% k-order interface
%%%
%%%----------------------------------------------------------------------------   

%%
%%
-spec z() -> l().
z() ->
   {uid, {0,0,0,0}, 0}.
   

%%
%% @doc
%% generate locally unique 64-bit k-order identifier
-spec l()    -> l().
-spec l(g()) -> l().

l() -> 
   {uid, lt(), seq()}.

l({uid, {_, _, _, _}, _} = Uid) ->
   Uid;

l({uid, {A, B, Node, C, D}, Seq})
 when is_binary(Node) ->
   case hid(erlang:node()) of
      Node ->
         {uid, {A, B, C, D}, Seq};
      _    ->
         exit(badarg)
   end;

l({uid, {A, B, Node, C, D}, Seq})
 when is_atom(Node) ->
   case erlang:node() of
      Node ->
         {uid, {A, B, C, D}, Seq};
      _    ->
         exit(badarg)
   end.

%%
%% @doc
%% generate globally unique 96-bit k-order identifier
-spec g() -> g().
-spec g(l()) -> g().

g() ->
   {uid, gt(erlang:node()), seq()}.

g({uid, {A, B, C, D}, Seq}) ->
   {uid, {A, B, erlang:node(), C, D}, Seq};

g({uid, {_, _, _, _, _}, _} = Uid) ->
   Uid.

%%
%% @doc
%% encode k-order number to binary format
-spec encode(l() | g()) -> binary().

encode({uid, T, Seq}) ->
   <<(encode_t(T))/bits, Seq:14>>.

encode_t({A, B, C, D}) ->
   L0 = 20 - ?CONFIG_DRIFT,
   L1 = ?CONFIG_DRIFT, 
   <<T:50/bits, _/bits>> = <<A:20, B:L0, C:L1, D:20>>,
   T;

encode_t({A, B, Node, C, D})
 when Node =:= erlang:node() ->
   ID = hid(erlang:node()),
   L0 = 20 - ?CONFIG_DRIFT,
   L1 = ?CONFIG_DRIFT, 
   <<T:82/bits, _/bits>> = <<A:20, B:L0, ID/binary, C:L1, D:20>>,
   T;

encode_t({A, B, Node, C, D})
 when is_binary(Node) ->
   L0 = 20 - ?CONFIG_DRIFT,
   L1 = ?CONFIG_DRIFT, 
   <<T:82/bits, _/bits>> = <<A:20, B:L0, Node/binary, C:L1, D:20>>,
   T.

%%
%% @doc
%% decode k-order number from binary format
-spec decode(binary()) -> l() | g().

decode(<<T:50/bits, Seq:14>>) ->
   {uid, decode_lt(T), Seq};

decode(<<T:82/bits, Seq:14>>) ->
   {uid, decode_gt(T), Seq}.

decode_lt(T) ->
   L0 = 20 - ?CONFIG_DRIFT,
   L1 = ?CONFIG_DRIFT, 
   <<A:20, B:L0, C:L1, D:10>> = T,
   {A, B, C, D bsl 10}.

decode_gt(T) ->
   L0 = 20 - ?CONFIG_DRIFT,
   L1 = ?CONFIG_DRIFT, 
   <<A:20, B:L0, Node:4/binary, C:L1, D:10>> = T,
   case hid(erlang:node()) of
      Node ->
         {A, B, erlang:node(), C, D bsl 10};
      _    ->
         {A, B, Node, C, D bsl 10}
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

gtol({uid, {A, B, _, C, D}, Seq}) ->
   {uid, {A, B, C, D}, Seq}.

%%
%% @doc
%% approximate distance between k-order values
-spec d(uid(), uid()) -> uid().

d({uid, TA, SeqA}, {uid, TB, SeqB}) ->
   {uid, d(TA, TB), SeqA - SeqB}; 

d({A1, B1, C1, D1}, {A2, B2, C2, D2}) ->
   X = timer:now_diff(
      {A1, (B1 bsl ?CONFIG_DRIFT) + C1, D1}, 
      {A2, (B2 bsl ?CONFIG_DRIFT) + C2, D2}
   ),
   D  = X rem ?BASE,
   Y0 = X div ?BASE,
   B0 = Y0 rem ?BASE,
   B  = B0 bsr  ?CONFIG_DRIFT,
   C  = B0 band ((1 bsl ?CONFIG_DRIFT) - 1),
   A  = Y0 div ?BASE,
   {A, B, C, D};

d({A1, B1, Node, C1, D1}, {A2, B2, Node, C2, D2}) ->
   d({A1, B1, C1, D1}, {A2, B2, C2, D2}).   

%%
%% @doc
%% helper function to extract time-stamp in milliseconds from k-order value
-spec t(l() | g()) -> integer().

t({uid, {A, B, C, D}, _}) ->
   (A * ?BASE + (B bsl ?CONFIG_DRIFT) + C) * 1000 + D div 1000;
t({uid, {A, B, _, C, D}, _}) ->
   (A * ?BASE + (B bsl ?CONFIG_DRIFT) + C) * 1000 + D div 1000.


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
%% local time stamp
lt() ->
   {A, B0, C0} = os:timestamp(),
   B = B0 bsr  ?CONFIG_DRIFT,
   C = B0 band ((1 bsl ?CONFIG_DRIFT) - 1),
   D = C0 band 16#ffc00,
   {A, B, C, D}.

%%
%% global time stamp
gt(Node) ->
   {A, B0, C0} = os:timestamp(),
   B = B0 bsr  ?CONFIG_DRIFT,
   C = B0 band ((1 bsl ?CONFIG_DRIFT) - 1),
   D = C0 band 16#ffc00,
   {A, B, Node, C, D}.

%%
%% locally unique sequential number 14-bit length 
seq() ->
   erlang:unique_integer([monotonic, positive]) band 16#3fff.

%%
%% host unique identifier
hid(Node) ->
   <<(erlang:phash(Node, 1 bsl 32)):32>>.
