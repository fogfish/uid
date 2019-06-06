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
%%
-module(uid_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   native_format/1
,  cast/1
,  cast_ext/1
,  difference/1
,  codec/1
,  codec64/1
,  vclock/1
,  benchmark/1
]).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

native_format(_) ->
   {uid, {0, 0, 0}, 0} = uid:z(),
   {uid, {_, _, _}, _} = uid:l(),
   {uid, _, {_, _, _}, _} = uid:g(),
   _ = uid:t(uid:l()),
   _ = uid:t(uid:g()).

cast(_) ->
   Local = uid:l(),
   Local = uid:l(uid:g(Local)),
   Local = uid:gtol(uid:g(Local)).

cast_ext(_) ->
   Local  = uid:l(),
   Binary = uid:encode(uid:g(Local)),
   Local  = uid:l(uid:decode(Binary)).

difference(_) ->
   {uid, {_, _, _}, _} = uid:d(uid:l(), uid:l()),
   {uid, _, {_, _, _}, _} = uid:d(uid:g(), uid:g()).

codec(_) ->
   Local = uid:l(),
   Local = uid:decode(uid:encode(Local)),

   Global = uid:g(),
   Global = uid:decode(uid:encode(Global)).

codec64(_) ->
   Local = uid:l(),
   Local = uid:decode64(uid:encode64(Local)),

   Global = uid:g(),
   Global = uid:decode64(uid:encode64(Global)).

benchmark(_) ->
   {TPU, TPS} = uid_benchmark:run(),
   ct:pal("~p TPU, ~p TPS.", [TPU, TPS]).

%%
%% 
vclock(_) ->
   A = uid:vclock(),
   B = uid:vclock(A),
   C = [vtime('a@127.0.0.1') | B],
   D = [vtime('b@127.0.0.1') | B],
   E = uid:join(C, D),
   false = uid:descend(A, B),
   true = uid:descend(B, A),
   true = uid:descend(C, B),
   true = uid:descend(D, B),
   false = uid:descend(C, D),
   false = uid:descend(D, C),
   true = uid:descend(E, C),
   true = uid:descend(E, D).

vtime(Node) ->
   {uid, T, Seq} = uid:l(),
   {uid, Node, T, Seq}.
