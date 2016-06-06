
%% max number of parallel sequences
-define(SEQ,         16).

%% max sequence value
-define(SEQ_MAX,   1023).

%%
%% internal sequence state
-record(uid, {
   node = undefined :: node()
  ,t    = undefined :: any()
  ,id   = undefined :: any()
  ,seq  = undefined :: any()
}).

-define(BASE,          1000000).