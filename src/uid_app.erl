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
   {ok, Sup} = uid_sup:start_link(),
   lists:foreach(fun default_seq/1, opts:val(default, [], uid)),
   {ok, Sup}.

stop(_State) ->
   ok.

%%
%%
default_seq({local,  Type, Name}) ->
   uid:local({Type, Name});

default_seq({global, Type, Name}) ->
   uid:global({Type, Name}).


