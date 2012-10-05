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
-module(uid).
-export([start/0, start/1]).
-export([new/1, seq32/1]).

%%
%%
start() ->
   start([]).

start(Config) ->
   lists:foreach(
      fun({K, V}) -> application:set_env(?MODULE, K, V) end,
      Config
   ),
   AppFile = code:where_is_file(atom_to_list(?MODULE) ++ ".app"),
   {ok, [{application, _, List}]} = file:consult(AppFile), 
   Apps = proplists:get_value(applications, List, []),
   lists:foreach(
      fun(X) -> 
         ok = case application:start(X) of
            {error, {already_started, X}} -> ok;
            Ret -> Ret
         end
      end,
      lists:delete(kernel, lists:delete(stdlib, Apps))
   ),
   application:start(?MODULE).

%%
%%
new({seq, Uid}) ->
   supervisor:start_child(uid_sup, {
      Uid,
      {uid_seq, start_link, [Uid]},
      permanent, 1000, worker, dynamic    
   }).

%%
%%
seq32(Uid) ->
   {ok, Val} = gen_server:call({global, Uid}, seq32),
   Val.


