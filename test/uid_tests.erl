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
-module(uid_tests).
-include_lib("eunit/include/eunit.hrl").

uid_test_() ->
   {foreach,
      fun init/0,
      fun free/1,
      [
         fun l/1,
         fun g/1,
         fun d/1,
         fun vclock/1
      ]
   }.

init() ->
   application:start(uid).

free(_) ->
   ok.

%%
%%
l(_) ->
   [
      ?_assertMatch({uid, _}, uid:l())
   ].

%%
%%
g(_) ->
   L = uid:l(),
   [
      ?_assertMatch({uid, _, _}, uid:g()),
      ?_assertMatch(L, uid:l(uid:g(L))),
      ?_assertMatch(L, uid:gtol(uid:g(L)))
   ].

%%
%%
d(_) ->
   [
      ?_assertMatch({uid, _}, uid:d(uid:l(), uid:l())),
      ?_assertMatch({uid, _, _}, uid:d(uid:g(), uid:g()))
   ].


%%
%% 
vclock(_) ->
   A = uid:vclock(),
   B = uid:vclock(A),
   C = [vtime('a@127.0.0.1') | B],
   D = [vtime('b@127.0.0.1') | B],
   E = uid:join(C, D),
   [
      ?_assertMatch(false, uid:descend(A, B)),
      ?_assertMatch(true,  uid:descend(B, A)),
      ?_assertMatch(true,  uid:descend(C, B)),   
      ?_assertMatch(true,  uid:descend(D, B)),
      ?_assertMatch(false, uid:descend(C, D)),   
      ?_assertMatch(false, uid:descend(D, C)),
      ?_assertMatch(true,  uid:descend(E, C)),   
      ?_assertMatch(true,  uid:descend(E, D))   
   ].

vtime(Node) ->
   {uid, Uid} = uid:l(),
   {uid, Node, Uid}.




