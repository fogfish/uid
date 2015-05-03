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
%%     k-ordered unique global identity 96bit
-module(uid_128).

-export([
   l/1,
   g/1,
   gtol/1,
   i/1
]).


l({uid, <<_:6/binary, Node:16, X:8/binary>>}) ->
   case erlang:phash(erlang:node(), 1 bsl 16) of
      Node ->
         {uid, X};
      _ ->
         exit(badarg)
   end.

g(Local) ->
   {ok,  Id} = application:get_env(uid, worker),
   Node = erlang:phash(erlang:node(), 1 bsl 16),
   {uid, <<Id:6/binary, Node:16, Local/binary>>}.


gtol({uid, <<_:6/binary, _:16, X:8/binary>>}) ->
   {uid, X}.

i({uid, <<_:6/binary, _:16, X:64>>}) ->
   X.
