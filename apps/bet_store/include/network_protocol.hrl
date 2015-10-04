%% tcp/udp protocol commands
-define(HELP, "help").
-define(SELECT, "select:").
-define(INSERT, "insert:").
-define(UPDATE, "update:").
-define(DELETE, "delete:").

%% tcp/udp protocol responses
-define(OK, {ok, <<"ok">>}).
-define(NOVAL, {ok, <<"noval">>}).
-define(CONFLICT, {ok, <<"conflict">>}).
-define(WRONGKEYVAL, {ok, <<"wrong key_value format">>}).
-define(CREATED, {ok, <<"created">>}).
