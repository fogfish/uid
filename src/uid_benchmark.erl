%% @description
%%   stress test / benchmark
-module(uid_benchmark).

-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

-define(N,        4).
-define(LOOP,     1 *  100 * 1000).
-define(TIMEOUT, 60 * 1000).

%%
%%
run() ->
   application:start(uid),
   Hashmap = ets:new(undefined, [public, set, {write_concurrency, true}]),
   case timer:tc(fun() -> exec(?N, Hashmap) end) of
      {T, ok} ->
         TPU = ?N * ?LOOP / T,
         TPS = ?N * ?LOOP / (T / 1000000),
         ets:delete(Hashmap),
         {TPU, TPS};
      {_, Error} ->
         ets:delete(Hashmap),
         Error
   end.

exec(N, Hashmap) ->
   Self = self(),
   Pids = [spawn_link(fun() -> loop(Self, Id, Hashmap, ?LOOP) end) || Id <- lists:seq(1, N)],
   fold(Pids).

fold([]) -> ok;
fold([Pid | Pids]) ->
   receive
      {ok, Pid} -> fold(Pids)
   after ?TIMEOUT ->
      {error, timeout}
   end.

loop(Pid, _Id, _Hashmap, 0) ->
   Pid ! {ok, self()};
loop(Pid,  Id,  Hashmap, N) ->
   Uid = uid:l(),
   % case ets:insert_new(Hashmap, {Uid, 1}) of
   %    true  ->
   %       ok;
   %    false ->
   %       io:format("=> uid conflict ~p~n", [Uid])
   % end,
   loop(Pid, Id, Hashmap, N - 1).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

