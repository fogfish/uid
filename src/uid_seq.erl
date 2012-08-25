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
-module(uid_seq).
-behaviour(gen_server).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([start_link/1, to_list/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% internal state
-record(srv, {
	uid,   % uid
   file,  % file name persist state
   csync, % count to sync  
   tsync, % time  to sync

   cnt,   % number of retriven values
   seq    % current seq value
}).
-define(SEQDIR, "/tmp").
-define(CSYNC,     100).
-define(TSYNC,   10000).


start_link({_Parent, {{uid, seq}, Uid}}) ->
   gen_server:start_link(?MODULE, [Uid], []).

init([Uid]) ->
   % config
   Dir   = config(seqdir, ?SEQDIR),
   CSync = config(csync,  ?CSYNC),
   TSync = config(tsync,  ?TSYNC),
   % define
   File = filename:join([Dir, to_list(Uid) ++ ".seq"]),
   case filelib:is_file(File) of
   	true  ->
   	   {ok, [{seq, Cnt, Seq}]} = file:consult(File),
   	   ok = pts:attach({{uid, seq}, Uid}),
   	   {ok,
   	      #srv{
   	         uid  = Uid,
               file = File,
               tsync= TSync,
               csync= CSync,
               cnt  = Cnt,
               seq  = Seq
   	      }
   	   };
   	false ->
   	   ok = pts:attach({{uid, seq}, Uid}),
   	   {ok, 
   	      #srv{
   	         uid  = Uid,
               file = File,
               tsync= TSync,
               csync= CSync,
               cnt  = 0,
               seq  = 1
   	      }
   	   }
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

%%
%%
handle_call({get, _}, _, #srv{seq=Seq, cnt=Cnt, tsync=Tout}=S) ->
   Val = hash:seq32(Seq), 
   {reply, {ok, Val}, csync(S#srv{seq=Val, cnt=Cnt + 1}), Tout};

handle_call(_, _Tx, S) ->
   {noreply, sync(S)}.

%%
%%
handle_cast(_, S) ->
   {noreply, sync(S)}.

%%
%%
handle_info(timeout, S) ->
   {noreply, sync(S)};

handle_info(_, S) ->
   {noreply, sync(S)}.

terminate(_Reason, #srv{uid=Uid}=S) ->
   sync(S),
   pts:detach({{aika, stream}, Uid}),
   ok.

code_change(_OldVsn, S, _Extra) ->
   {ok, S}.  


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
config(Key, Default) ->
   case application:get_env(uid, Key) of
   	{ok, Val} -> Val;
   	undefined -> Default
   end.

%%
%%
csync(#srv{csync=false}=S) ->
   S;
csync(#srv{csync=Sync, cnt=Cnt}=S)
 when Cnt rem Sync =:= 0 ->
   sync(S);

csync(S) ->
   S.

%%
%%
sync(#srv{file=File, cnt=Cnt, seq=Seq}=S) ->
   ok = file:write_file(File, io_lib:format("~p.", [{seq, Cnt, Seq}])),
   S.

to_list(Uid)
 when is_atom(Uid) ->
   atom_to_list(Uid);

to_list(Uid)
 when is_integer(Uid) ->
   integer_to_list(Uid);

to_list(Uid)
 when is_binary(Uid) ->
    binary_to_list(Uid);

to_list(Uid)
 when is_tuple(Uid) ->
   lists:flatten(
   	[ [$_, to_list(X)] || X <- tuple_to_list(Uid) ]
   ).



