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
-module(uid_sup).
-behaviour(supervisor).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([start_link/0, init/1, spawn/2]).


%%
%%
%%
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init(Cfg) -> 
   {ok,
      {
         {simple_one_for_one, 2, 3600},  % 2 failure in hour
         [{
      		uid_seq, 
      		{uid_seq, start_link, []},
      		permanent, 5000, worker, dynamic
   		}]
      }
   }.

%%
%%
spawn(Ns, Uid) ->
   supervisor:start_child(?MODULE, [Ns, Uid]).


