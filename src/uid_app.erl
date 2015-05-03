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
%%     unique identity
%%
-module(uid_app).
-behaviour(application).

-export([start/2, stop/1]).

%%
%%
start(_Type, _Args) -> 
   {ok, Sup} = uid_sup:start_link(),
   config_worker_id(),
   {ok, Sup}.

stop(_State) ->
   ok.

%%
%%
config_worker_id() ->
   case application:get_env(uid, worker) of
      %% xx:xx:xx:xx:xx:xx
      {ok, Mac} when is_list(Mac), length(Mac) =:= 17 ->
         application:set_env(uid, worker, hwaddr_to_binary(Mac));
      {ok, Eth} ->
         application:set_env(uid, worker, hwaddr_to_binary(hwaddr(Eth)));
      _ ->
         application:set_env(uid, worker, hwaddr_to_binary(hwaddr()))
   end.

%%
%%
hwaddr_to_binary(Mac)
 when is_list(Mac) ->
   list_to_binary([ list_to_integer(X, 16) || X <- string:tokens(Mac, ":") ]);

hwaddr_to_binary(Mac) 
 when is_binary(Mac) ->
   Mac.

%%
%%
hwaddr(Eth) ->
   {ok, List} = inet:getifaddrs(),
   case proplists:get_value(Eth, List) of
      undefined ->
         exit({badarg, Eth});
      Interface ->
         list_to_binary(proplists:get_value(hwaddr, Interface))
   end.

hwaddr() ->
   {ok, List} = inet:getifaddrs(),
   hwaddr1(List).

hwaddr1([{_, Head} | Tail]) ->
   case proplists:get_value(hwaddr, Head) of
      undefined ->
         hwaddr1(Tail);
      Mac ->
         list_to_binary(Mac)
   end;
hwaddr1([]) ->
   exit(badarg).



