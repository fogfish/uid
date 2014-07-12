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
]).


%%
%% generate local 64-bit identity
-spec(l/0 :: () -> binary()).
-spec(l/1 :: (binary()) -> binary()).

l() ->
   l(erlang:now()).

l(<<X:8/binary, Node:16, _/binary>>=Global)
 when size(Global) =:= 16 -> 
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
-spec(g/0 :: () -> binary()).
-spec(g/1 :: (binary()) -> binary()).

g() ->
   g(l()).

g(Local)
 when size(Local) =:= 8 ->
   {ok,  Id} = application:get_env(uid, worker),
   Node = erlang:phash(erlang:node(), 1 bsl 16),
   <<Local/binary, Node:16, Id/binary>>.



