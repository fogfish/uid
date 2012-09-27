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

-export([start_link/2, to_list/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% internal state
-record(srv, {
   ns,    % name space
	uid,   % uid
   file,  % file name persist state
   csync, % count to sync  
   tsync, % time  to sync

   cnt,   % number of retrieve values
   seq    % current seq value
}).
-define(SEQDIR, "/tmp").
-define(CSYNC,     100).
-define(TSYNC,   10000).


start_link(Ns, Uid) ->
   gen_server:start_link(?MODULE, [Ns, Uid], []).

init([Ns, Uid]) ->
   % config
   Dir   = config(seqdir, ?SEQDIR),
   CSync = config(csync,  ?CSYNC),
   TSync = config(tsync,  ?TSYNC),
   % define
   pns:register(Ns, Uid),
   File = filename:join([Dir, to_list(Uid) ++ ".seq"]),
   {Cnt, Seq} = case filelib:is_file(File) of
   	true  ->
   	   {ok, [{seq, Cnt0, Seq0}]} = file:consult(File),
   	   {Cnt0, Seq0}; 
   	false -> 
         {0, 1}
   end,
   {ok,
      #srv{
         ns   = Ns,
         uid  = Uid,
         file = File,
         tsync= TSync,
         csync= CSync,
         cnt  = Cnt,
         seq  = Seq
      }
   }.

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

%%
%%
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

handle_info({get, Tx, _Key}, #srv{seq=Seq, cnt=Cnt, tsync=Tout}=S) ->
   Val = hash:seq32(Seq), 
   gen_server:reply(Tx, {ok, Val}),
   {noreply, maybe_sync(S#srv{seq=Val, cnt=Cnt + 1}), Tout};

handle_info(_, S) ->
   {noreply, sync(S)}.

%%
%%
terminate(_Reason, #srv{ns=Ns, uid=Uid}=S) ->
   sync(S),
   pns:unregister(Ns, Uid),
   ok.

%%
%%
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
%% sync data on count threshold 
maybe_sync(#srv{csync=false}=S) ->
   S;
maybe_sync(#srv{csync=Sync, cnt=Cnt}=S)
 when Cnt rem Sync =:= 0 ->
   sync(S);

maybe_sync(S) ->
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



