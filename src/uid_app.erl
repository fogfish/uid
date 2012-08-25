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
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([start/2, stop/1]).

%%
%%
start(_Type, _Args) -> 
   case uid_sup:start_link() of
      {ok, Pid} -> 
         def_seq(), 
         {ok, Pid};
      Other     -> 
         {error, Other}
   end.

stop(_State) ->
   pts:drop({aika, stream}),
   ok.

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% define global sequence table
def_seq() ->
   ok = pts:new({uid, seq}, [
      rthrough,
      readonly,
      {iftype, server},
      {factory, fun uid_sup:spawn/1}
   ]).



