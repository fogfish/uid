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
,  l/0
,  l/1
,  g/0
,  g/1
,  encode/1
,  encode/2
,  decode/1
,  decode/2
,  encode64/1
,  encode64/2
,  decode64/1
,  decode64/2
]).
%% k-order utility
-export([
   gtol/1
,  d/2
,  t/1
]).
%% v-clock interface
-export([
   vclock/0
,  vclock/1
,  join/2
,  descend/2
,  descend/3
,  diff/2
]).

%%
%% data types
-export_type([uid/0, l/0, g/0, vclock/0]).
-type uid()    :: l() | g().
-type l()      :: {uid, t(), seq()}.
-type g()      :: {uid, id(), t(), seq()}.

-type t()      :: {integer(), integer(), integer()}.
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
   {uid, {0,0,0}, 0}.
   

%%
%% @doc
%% generate locally unique 64-bit k-order identifier
-spec l()    -> l().
-spec l(g()) -> l().

l() -> 
   {uid, t(), seq()}.

l({uid, _, _} = Uid) ->
   Uid;

l({uid, Node, T, Seq})
 when is_binary(Node) ->
   case hid(erlang:node()) of
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
%% generate globally unique 96-bit k-order identifier
-spec g() -> g().
-spec g(l()) -> g().

g() ->
   {uid, erlang:node(), t(), seq()}.

g({uid, T, Seq}) ->
   {uid, erlang:node(), T, Seq};

g({uid, _, _, _} = Uid) ->
   Uid.

%%
%% @doc
%% encode k-order number to binary format
-spec encode(l() | g()) -> binary().
-spec encode(integer(), l() | g()) -> binary().

encode(Uid) ->
   encode(?CONFIG_DRIFT, Uid).

encode(_Bits, {uid, T, Seq}) ->
   <<(encode_t(T))/bits, Seq:14>>;

encode(Bits, {uid, Node, T, Seq}) ->
   <<(encode_t(Bits, Node, T))/bits, Seq:14>>.

encode_t({A, B, C}) ->
   <<T:50/bits, _/bits>> = <<A:20, B:20, C:20>>,
   T.

encode_t(Bits, Node, {A, B, C})
 when is_binary(Node), Bits >= ?CONFIG_DRIFT_LOW, Bits =< ?CONFIG_DRIFT_HIGH ->
   L0 = 20 - Bits,
   L1 = Bits, 
   B0 = B bsr  Bits,
   B1 = B band ((1 bsl Bits) - 1),
   <<T:82/bits, _/bits>> = <<A:20, B0:L0, Node/binary, B1:L1, C:20>>,
   T;

encode_t(Bits, Node, T) ->
   encode_t(Bits, hid(Node), T).


%%
%% @doc
%% decode k-order number from binary format
-spec decode(binary()) -> l() | g().
-spec decode(integer(), binary()) -> l() | g().

decode(Uid) ->
   decode(?CONFIG_DRIFT, Uid).

decode(_Bits, <<Prefix:50/bits, Seq:14>>) ->
   {uid, decode_lt(Prefix), Seq};

decode(Bits, <<Prefix:82/bits, Seq:14>>) ->
   {Node, T} = decode_gt(Bits, Prefix),
   {uid, Node, T, Seq}.

decode_lt(<<A:20, B:20, C:10>> ) ->
   {A, B, C bsl 10}.

decode_gt(Bits, T) ->
   L0 = 20 - Bits,
   L1 = Bits, 
   <<A:20, B:L0, Node:4/binary, C:L1, D:10>> = T,
   case hid(erlang:node()) of
      Node ->
         {erlang:node(), {A, B bsl Bits + C, D bsl 10}};
      _    ->
         {Node, {A, B bsl Bits + C, D bsl 10}}
   end.

%%
%% @doc
%% encode k-order number to base64 url 
-spec encode64(l() | g()) -> binary().
-spec encode64(integer(), l() | g()) -> binary().

encode64(Uid) ->
   encode64(?CONFIG_DRIFT, Uid).

encode64(Drift, Uid) ->
  uid_b64:encode( encode(Drift, Uid) ).

%%
%% @doc
%% decode k-order number from base64 url 
-spec decode64(binary()) -> l() | g().
-spec decode64(integer(), binary()) -> l() | g().

decode64(Uid) ->
   decode64(?CONFIG_DRIFT, Uid).

decode64(Drift, Uid) ->
   decode(Drift, uid_b64:decode(Uid)).

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

gtol({uid, _, T, Seq}) ->
   {uid, T, Seq}.

%%
%% @doc
%% approximate distance between k-order values
-spec d(uid(), uid()) -> uid().

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
t() ->
   {A, B, C} = os:timestamp(),
   {A, B, C band 16#ffc00}.

%%
%% locally unique sequential number 14-bit length 
seq() ->
   erlang:unique_integer([monotonic, positive]) band 16#3fff.

%%
%% host unique identifier
hid(Node) ->
   <<(erlang:phash(Node, 1 bsl 32)):32>>.
