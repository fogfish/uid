
%%
%% global uid 96 bit
-ifndef(CONFIG_UID_96BIT).
-define(CONFIG_UID_96BIT,   true).
-endif.

%%
%% global uid 128 bit
%% -ifndef(CONFIG_UID_128BIT).
%% -define(CONFIG_UID_128BIT,  true).
%% -endif.

-ifdef(CONFIG_UID_96BIT).
-define(GUID,     uid_96).
-define(is_l(X), is_binary(X), byte_size(X) =:=  8).
-define(is_g(X), is_binary(X), byte_size(X) =:= 12).
-endif.

-ifdef(CONFIG_UID_128BIT).
-define(GUID,     uid_128).
-define(is_l(X), is_binary(X), byte_size(X) =:=  8).
-define(is_g(X), is_binary(X), byte_size(X) =:= 16).
-endif.

-define(BASE,    1000000).
