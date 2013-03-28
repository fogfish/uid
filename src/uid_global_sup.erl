%%
%%
-module(uid_global_sup).
-behaviour(supervisor).

-export([
   start_link/0, init/1,
   seq/1
]).

%%
%%
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init(_) -> 
   {ok,
      {
         {one_for_one, 10, 3600},  % 10 failure in hour
         []
      }
   }.


%%
%%
seq(Uid) ->
   supervisor:start_child(uid_sup, {
      Uid,
      {uid_seq, start_link, [global, Uid]},
      transient, 60000, worker, dynamic    
   }).




