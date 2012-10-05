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

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% internal state
-record(srv, {
   file,  % file name persist state
   csync, % count to sync  
   tsync, % time  to sync

   cnt,   % number of retrieve values
   seq    % current seq value
}).
-define(SEQDIR, "/tmp").
-define(CSYNC,   false).
-define(TSYNC,   10000).


start_link(Uid) ->
   gen_server:start_link({global, Uid}, ?MODULE, [Uid], []).

init([Uid]) ->
   % config
   Root  = config(seqdir, ?SEQDIR),
   CSync = config(csync,  ?CSYNC),
   TSync = config(tsync,  ?TSYNC),
   File = filename:join([Root, atom_to_list(Uid) ++ ".seq"]),
   {Cnt, Seq} = case filelib:is_file(File) of
   	true  ->
   	   {ok, [{seq, Cnt0, Seq0}]} = file:consult(File),
   	   {Cnt0, Seq0}; 
   	false -> 
         {0, 1}
   end,
   process_flag(trap_exit, true),
   {ok,
      #srv{
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
handle_call(seq32, _Tx, #srv{seq=Seq, cnt=Cnt, tsync=Timeout}=S) ->
   Val = seq32(Seq), 
   {reply,
      {ok, Val},
      maybe_sync(S#srv{seq=Val, cnt=Cnt + 1}), 
      Timeout
   };

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

%%
%%
terminate(_Reason, S) ->
   sync(S),
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


%% Additive congruential method of generating values in 
%% pseudo-random order bt Roy Hann.
%% Initially the shift register contains the value 0001.
%% The two rightmost bits are XOR-ed, the result is fed into
%% the leftmost bit position and previous regsiter contents shift
%% one bit right. Choosing correct bits tap position is important.
%% see E.J. Watson "Primitive Polynomials", Math of Computation
%%  8bit {0, 2, 3, 4}
%% 16bit {0, 2, 3, 5}
%% 31bit {0, 3}
%% 32bit {0, 1, 2, 3, 5, 7}
%% 64bit {0, 1, 3, 4}
%seq31(N) ->
%   (N bsr 1) bor ((((N bsr 3) bxor N) band 1) bsl 30).

seq32(N) ->
   (N bsr 1) bor ((((N bsr 7) bxor (N bsr 5) bxor (N bsr 3) bxor (N bsr 2) bxor (N bsr 1) bxor N) band 1) bsl 30).



