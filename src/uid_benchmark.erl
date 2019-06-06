%% @description
%%   stress test / benchmark
-module(uid_benchmark).

-export([run/0]).

-define(N,        4).
-define(LOOP,     1 * 1000 * 1000).
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
   Pids = [spawn_link(fun() -> loop(Self, ?LOOP) end) || _ <- lists:seq(1, N)],
   fold(Pids).

fold([]) -> ok;
fold([Pid | Pids]) ->
   receive
      {ok, Pid} -> fold(Pids)
   after ?TIMEOUT ->
      {error, timeout}
   end.

loop(Pid, 0) ->
   Pid ! {ok, self()};
loop(Pid, N) ->
   _Uid = uid:l(),
   loop(Pid, N - 1).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

