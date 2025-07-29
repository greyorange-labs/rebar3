-module(resource_conflict_suite1).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% This suite will conflict with resource_conflict_suite2 if run in same node
%% Both try to register same process name and use same ETS table

all() ->
    [test_process_registration, test_ets_table, test_application_env].

init_per_suite(Config) ->
    ct:pal("Suite1: Starting init_per_suite"),

    %% Try to register a global process name
    case whereis(shared_test_process) of
        undefined ->
            Pid = spawn(fun() -> test_server_loop() end),
            register(shared_test_process, Pid),
            ct:pal("Suite1: Registered shared_test_process with PID ~p", [Pid]);
        ExistingPid ->
            ct:pal("Suite1: shared_test_process already exists with PID ~p", [ExistingPid])
    end,

    %% Create ETS table with same name as suite2 will try to create
    case ets:info(shared_test_table) of
        undefined ->
            TableId = ets:new(shared_test_table, [named_table, public]),
            ct:pal("Suite1: Created ETS table shared_test_table with ID ~p", [TableId]);
        _ ->
            ct:pal("Suite1: ETS table shared_test_table already exists")
    end,

    %% Set application environment that suite2 will also try to set
    application:set_env(test_example, shared_config_key, suite1_value),
    ct:pal("Suite1: Set application env shared_config_key = suite1_value"),

    %% Add some test data
    ets:insert(shared_test_table, {suite1_key, "Suite1 was here", erlang:system_time()}),

    Config.

end_per_suite(_Config) ->
    ct:pal("Suite1: Cleaning up in end_per_suite"),

    %% Clean up registered process
    case whereis(shared_test_process) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            unregister(shared_test_process),
            ct:pal("Suite1: Unregistered shared_test_process")
    end,

    %% Clean up ETS table
    case ets:info(shared_test_table) of
        undefined -> ok;
        _ ->
            ets:delete(shared_test_table),
            ct:pal("Suite1: Deleted ETS table shared_test_table")
    end,

    ok.

test_process_registration(_Config) ->
    ct:pal("Suite1: Testing process registration"),

    %% Verify our process is registered
    Pid = whereis(shared_test_process),
    true = is_pid(Pid),
    true = is_process_alive(Pid),

    %% Send a message to our process
    shared_test_process ! {ping, self()},
    receive
        {pong, suite1} ->
            ct:pal("Suite1: Received expected pong from our process");
        Other ->
            ct:fail("Suite1: Received unexpected message: ~p", [Other])
    after 5000 ->
        ct:fail("Suite1: Timeout waiting for pong")
    end,

    %% Simulate some work
    timer:sleep(2000),
    ok.

test_ets_table(_Config) ->
    ct:pal("Suite1: Testing ETS table operations"),

    %% Verify our table exists (should exist from init_per_suite)
    case ets:info(shared_test_table) of
        undefined ->
            ct:pal("Suite1: ETS table doesn't exist, creating it now"),
            ets:new(shared_test_table, [named_table, public, set]);
        TableInfo when is_list(TableInfo) ->
            ct:pal("Suite1: ETS table exists with info: ~p", [length(TableInfo)])
    end,

    %% Insert some data
    Key = {suite1, test_data, erlang:unique_integer()},
    Value = {suite1_data, erlang:system_time(), node()},
    true = ets:insert(shared_test_table, {Key, Value}),

    %% Read it back
    [{Key, Value}] = ets:lookup(shared_test_table, Key),
    ct:pal("Suite1: Successfully stored and retrieved data: ~p -> ~p", [Key, Value]),

    %% Check all data in table
    AllData = ets:tab2list(shared_test_table),
    ct:pal("Suite1: Current ETS table contents: ~p", [AllData]),

    %% Simulate some work
    timer:sleep(2000),
    ok.

test_application_env(_Config) ->
    ct:pal("Suite1: Testing application environment"),

    %% Check our environment value
    {ok, Value} = application:get_env(test_example, shared_config_key),
    suite1_value = Value,  %% This should be our value
    ct:pal("Suite1: Application env shared_config_key = ~p (expected suite1_value)", [Value]),

    %% Set another conflicting value
    application:set_env(test_example, another_shared_key, {suite1, erlang:system_time()}),

    %% Simulate some work
    timer:sleep(2000),
    ok.

%% Helper function for test server process
test_server_loop() ->
    receive
        {ping, From} ->
            From ! {pong, suite1},
            test_server_loop();
        stop ->
            ct:pal("Suite1: Test server process stopping"),
            ok;
        Other ->
            ct:pal("Suite1: Test server received unexpected message: ~p", [Other]),
            test_server_loop()
    end.
