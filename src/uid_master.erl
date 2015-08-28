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
%%   unique sequential allocator
-module(uid_master).
-behaviour(gen_server).

-include("uid.hrl").

-export([
   start_link/0,
   init/1,
   terminate/2,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   code_change/3
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
   {ok,
      queue:from_list(
         [X || {_, X, _, _} <- supervisor:which_children(uid_seq_sup)]
      )
   }.

terminate(_Reason, _State) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

handle_call(pid, _Tx, Queue) ->
   Head = queue:head(Queue),
   % {reply, Head, queue:cons(Head, queue:tail(Queue))}.
   {reply, Head, queue:snoc(queue:tail(Queue), Head)}.

handle_cast(_, State) ->
   {noreply, State}.

handle_info(_, State) ->
   {noreply, State}.

code_change(_Vsn, State, _Extra) -> 
   {ok, State}.
