%% @description
%%   stress test / benchmark
-module(uid_benchmark).

-export([run/0]).

-define(N,        8).
-define(LOOP,     1 *  100 * 1000).
-define(TIMEOUT, 60 * 1000).

%%
%%
run() ->
   application:start(uid),
   case timer:tc(fun() -> exec(?N) end) of
      {T, ok} ->
         TPU = ?N * ?LOOP / T,
         TPS = ?N * ?LOOP / (T / 1000000),
         {TPU, TPS};
      {_, Error} ->
         Error
   end.

exec(N) ->
   Self = self(),
   Pids = [spawn_link(fun() -> loop(Self, Id, ?LOOP) end) || Id <- lists:seq(1, N)],
   fold(Pids).

fold([]) -> ok;
fold([Pid | Pids]) ->
   receive
      {ok, Pid} -> fold(Pids)
   after ?TIMEOUT ->
      {error, timeout}
   end.

loop(Pid, _Id, 0) ->
   Pid ! {ok, self()};
loop(Pid,  Id, N) ->
   uid:l(),
   loop(Pid, Id, N - 1).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

