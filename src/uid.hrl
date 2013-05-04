%%%----------------------------------------------------------------------------   
%%%
%%% macro
%%%
%%%----------------------------------------------------------------------------   

-define(VERBOSE, true).
-ifdef(VERBOSE).
   -define(DEBUG(Str, Args), error_logger:info_msg(Str, Args)).
-else.
   -define(DEBUG(Str, Args), ok).
-endif.


-define(SEED_SEQ32,     {0,1}). % initial seed
-define(POOL_SEQ32,       100). % pool of per-allocated values

-define(SEQDIR,        "/tmp").

-define(SEQ_TIMEOUT,    60000). % timeout to acquire unique id
-define(SEQ_CHECKPOINT, 10000). % timeout to check point seq
