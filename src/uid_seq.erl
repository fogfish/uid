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
-module(uid_seq).
-behaviour(gen_server).

-include("uid.hrl").

-export([
   start_link/1,
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

start_link(Id) ->
   gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
   {ok, 
      #uid{
         node = erlang:node(),
         id   = Id,
         seq  = 0
      }
   }.

terminate(_Reason, _State) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

handle_call(seq, _Tx, State0) ->
   State1 = seq(t(), State0),
   {reply, State1, State1}.

handle_cast(_, State) ->
   {noreply, State}.

handle_info(_, State) ->
   {noreply, State}.

code_change(_Vsn, State, _Extra) -> 
   {ok, State}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% 
t() ->
   t(os:timestamp()).

t({A, B, C}) ->
   {A, B, C div 1000}.
   
seq({A, B, C}, #uid{t = {A, B, C}, seq = ?SEQ_MAX} = State) ->
   timer:sleep(1),
   seq(t(), State);

seq({A, B, C}, #uid{t = {A, B, C}, seq = Seq} = State) ->
   State#uid{seq = Seq + 1};

seq(T, #uid{} = State) ->
   State#uid{t = T, seq = 0}.

