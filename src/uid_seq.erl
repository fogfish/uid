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
%%     unique sequential identity allocator
-module(uid_seq).
-behaviour(gen_server).
-include("uid.hrl").

-export([
   start_link/2,
   init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

%%
%% internal state
-record(srv, {
   mode,       % mode
   name,       % 
   file,       % file name persist state
   lock,       % global lock object
   pool,       % size of seq pool

   pos,        % current seq position 
   val,        % current seq value
   seq         % pool of allocated seq value
}).
-define(CSYNC,   false).
-define(TSYNC,   10000).


%%
%%
start_link(Mode, Name)
 when is_atom(Name) ->
   gen_server:start_link({local, Name}, ?MODULE, [Mode, Name], []);

start_link(Mode, Name)
 when is_list(Name) ->
   gen_server:start_link(?MODULE, [Mode, Name], []).

init([Mode, Name]) ->
   process_flag(trap_exit, true),
   {ok,
      maybe_global_seq( 
         recover_seq_state(empty_seq(Mode, Name))
      )
   }. 

%%
%%
empty_seq(Mode, Name) ->
   #srv{
      mode = Mode,
      name = Name,
      file = seq_file(Name),
      seq  = [],
      pool = opts:val(alloc, ?POOL_SEQ32, uid)
   }.

%%
%%
terminate(_Reason, S) ->
   persist_seq_state(S),
   ok.


%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

%%
%%
handle_call(i, _, S) ->
   % read sequence meta data
   Result = {S#srv.mode, S#srv.pos, S#srv.val},
   {reply, {ok, Result}, S};

handle_call(seq32, Tx,  #srv{seq=[]}=S) ->
   handle_call(seq32, Tx, allocate_seq_pool(S));

handle_call(seq32, _Tx, #srv{seq=[Val | Seq]}=S) ->
   {reply, {ok, Val}, S#srv{seq=Seq}};

handle_call(_, _, S) ->
   {noreply, S}.

%%
%%
handle_cast(_, S) ->
   {noreply, S}.

%%
%%
handle_info(checkpoint, S) ->
   {noreply, checkpoint(S)};

handle_info(_, S) ->
   {noreply, S}.

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
%%
seq_file(Name)
 when is_atom(Name) ->
   seq_file(atom_to_list(Name));
seq_file(Name)
 when is_list(Name) ->
   filename:join([config(seqdir, default_seq_dir()), Name ++ ".seq"]).

default_seq_dir() ->
   filename:join([config(seqdir, ?SEQDIR), atom_to_list(erlang:node())]).

%%
%% recover seq state from file
recover_seq_state(#srv{file=File}=S) ->
   maybe_recover_seq_state(filelib:is_file(File), S).

maybe_recover_seq_state(true,  #srv{file=File}=S) ->
   {ok, [{seq, Pos, Val}]} = file:consult(File),
   S#srv{pos=Pos, val=Val};
maybe_recover_seq_state(false, S) ->
   {Pos, Val} = ?SEED_SEQ32,
   S#srv{pos=Pos, val=Val}.

%%
%%
persist_seq_state(#srv{file=File, pos=Pos, val=Val}=S) ->
   ok = filelib:ensure_dir(File),
   ok = file:write_file(File, io_lib:format("~p.", [{seq, Pos, Val}])),
   S.

%%
%%
allocate_seq_pool(#srv{seq=[], mode=local, pos=Pos, val=Val, pool=Pool}=S) ->
   [Last | Seq] = seq32(Val, Pool),
   persist_seq_state(
      S#srv{
         seq = lists:reverse([Last | Seq]),
         pos = Pos + Pool,
         val = Last
      }
   );

allocate_seq_pool(#srv{seq=[], mode=global, lock=Lock}=S) ->
   maybe_allocate_seq_pool(ek:lease(Lock, ?SEQ_TIMEOUT), S).

maybe_allocate_seq_pool(Token, #srv{pos=Pos, val=Val, lock=Lock, pool=Pool}=S)
 when is_number(Token) ->
   ?DEBUG("uid global ~s (token: ~p, pos: ~p)", [S#srv.name, Token, Pos]),
   [Last | Seq] = seq32(skip(Val, Token - Pos), Pool),
   ek:release(Lock, Token + Pool),
   %io:format("got token: ~p~n", [Token]),
   persist_seq_state(
      S#srv{
         seq = lists:reverse([Last | Seq]),
         pos = Token + Pool,
         val = Last
      }
   );

maybe_allocate_seq_pool(undefined, #srv{lock=Lock}=S) ->
   % token is not know by local node
   ek:release(Lock, undefined),
   S;

maybe_allocate_seq_pool(timeout, #srv{name=Name}=S) ->
   error_logger:error_msg("seq ~s token timeout", [Name]),
   S.

%%
%%
checkpoint(#srv{lock=Lock}=S) ->
   maybe_checkpoint(ek:lease(Lock, ?SEQ_TIMEOUT), S).

maybe_checkpoint(Token, #srv{pos=Pos, val=Val, lock=Lock}=S)
 when is_number(Token) ->
   timer:send_after(?SEQ_CHECKPOINT, checkpoint),
   ek:release(Lock, Token),
   ?DEBUG("uid checkpoint global ~s (token: ~p)", [S#srv.name, Token]),
   persist_seq_state(
      S#srv{
         pos = Token,
         val = skip(Val, Token - Pos)
      }
   );

maybe_checkpoint(_, #srv{name=Name}=S) ->
   timer:send_after(?SEQ_CHECKPOINT, checkpoint),
   error_logger:error_msg("seq ~s checkpoint failed", [Name]),
   S.

%%
%%
maybe_global_seq(#srv{mode=global, name=Name, pos=Pos}=S) ->
   {ok, Pid} = case config(role, slave) of
      slave  -> 
         ?DEBUG("uid init global ~s (~s)", [S#srv.name, S#srv.file]),
         ek:lock({seq, Name}, undefined);
      leader -> 
         timer:send_after(?SEQ_CHECKPOINT, checkpoint),
         ?DEBUG("uid init global ~s (file: ~s, pos: ~p)", [S#srv.name, S#srv.file, Pos]),
         ek:lock({seq, Name}, Pos)
   end,
   S#srv{lock = Pid};

maybe_global_seq(#srv{mode=local}=S) ->
   ?DEBUG("uid init local  ~s (~s)", [S#srv.name, S#srv.file]),
   S.

%%
%%
seq32(Val, Len)
 when is_number(Val) ->
   seq32([seq32(Val)], Len - 1);

seq32(Seq, 0) ->
   Seq;
seq32([H | _]=Seq, Len) ->
   seq32([seq32(H) | Seq], Len - 1).

%%
%%
skip(Val, N)
 when N > 0  ->
   skip(seq32(Val), N - 1);

skip(Val, 0) -> 
   Val.



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



