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
-module(uid).
-include("uid.hrl").

-export([
   start/0, 
   start_link/2, local/1, global/1, i/1, 
   seq32/1
]).

%%
%%
start() ->
   applib:boot(?MODULE, []).

%%
%%
start_link(local,  {uid, Uid}) ->
   uid_seq:start_link(local,  Uid);

start_link(global, {uid, Uid}) ->
   uid_seq:start_link(global, Uid).


%%
%%
local({seq, Uid}) ->
   uid_local_sup:seq(Uid).

%%
%%
global({seq, Uid}) ->
   uid_global_sup:seq(Uid).

%%
%%
i(Uid) ->
   gen_server:call(Uid, i, ?SEQ_TIMEOUT).

%%
%%
seq32(Uid) ->
   {ok, Val} = gen_server:call(Uid, seq32, ?SEQ_TIMEOUT),
   Val.


