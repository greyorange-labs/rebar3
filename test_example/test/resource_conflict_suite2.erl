-module(resource_conflict_suite2).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% This suite conflicts with resource_conflict_suite1 when run in same node

all() ->
    [test_process_registration, test_ets_table, test_application_env].

init_per_suite(Config) ->
    ct:pal("Suite2: Starting resource conflict test suite 2"),
    
    %% Try to register a process with same name as suite1
    TestProcess = spawn(fun test_server_loop/0),
    case catch register(shared_test_process, TestProcess) of
        true -> 
            ct:pal("Suite2: Registered shared_test_process successfully");
        {'EXIT', {badarg, _}} ->
            ct:pal("Suite2: shared_test_process already registered (would conflict in single node)");
        Error ->
            ct:pal("Suite2: Error registering process: ~p", [Error])
    end,
    
    %% Try to create ETS table with same name as suite1
    case catch ets:new(shared_test_table, [named_table, public, set]) of
        shared_test_table ->
            ct:pal("Suite2: Created ETS table shared_test_table");
        {'EXIT', {badarg, _}} ->
            ct:pal("Suite2: ETS table shared_test_table already exists (would conflict in single node)");
        Error2 ->
            ct:pal("Suite2: Error creating ETS table: ~p", [Error2])
    end,
    
    %% Set application environment variable with same key as suite1
    application:set_env(test_example, shared_config_key, suite2_value),
    ct:pal("Suite2: Set application env shared_config_key to suite2_value"),
    
    Config.

end_per_suite(_Config) ->
    ct:pal("Suite2: Cleaning up resource conflict suite 2"),
    
    %% Clean up process
    case whereis(shared_test_process) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            timer:sleep(100)
    end,
    
    %% Clean up ETS table
    case catch ets:delete(shared_test_table) of
        true -> ct:pal("Suite2: Deleted ETS table");
        _ -> ok
    end,
    
    %% Clean up application env
    application:unset_env(test_example, shared_config_key),
    
    ok.

test_process_registration(_Config) ->
    ct:pal("Suite2: Testing process registration"),
    
    %% Verify our process is registered
    case whereis(shared_test_process) of
        undefined ->
            ct:pal("Suite2: Process not registered, creating it now"),
            TestProcess = spawn(fun test_server_loop/0),
            register(shared_test_process, TestProcess);
        Pid when is_pid(Pid) ->
            ct:pal("Suite2: Process is registered with PID: ~p", [Pid])
    end,
    
    %% Send a message to our process
    shared_test_process ! {ping, self()},
    receive
        {pong, suite2} -> 
            ct:pal("Suite2: Received expected pong from our process");
        Other -> 
            ct:fail("Suite2: Received unexpected message: ~p", [Other])
    after 5000 ->
        ct:fail("Suite2: Timeout waiting for pong")
    end,
    
    %% Simulate some work
    timer:sleep(2000),
    ok.

test_ets_table(_Config) ->
    ct:pal("Suite2: Testing ETS table operations"),
    
    %% Verify our table exists or create it
    case ets:info(shared_test_table) of
        undefined ->
            ct:pal("Suite2: ETS table doesn't exist, creating it now"),
            ets:new(shared_test_table, [named_table, public, set]);
        TableInfo when is_list(TableInfo) ->
            ct:pal("Suite2: ETS table exists with ~p properties", [length(TableInfo)])
    end,
    
    %% Insert some data (different from suite1)
    Key = {suite2, test_data, erlang:unique_integer()},
    Value = {suite2_data, erlang:system_time(), node()},
    true = ets:insert(shared_test_table, {Key, Value}),
    
    %% Read it back
    [{Key, Value}] = ets:lookup(shared_test_table, Key),
    ct:pal("Suite2: Successfully stored and retrieved data: ~p -> ~p", [Key, Value]),
    
    %% Check all data in table
    AllData = ets:tab2list(shared_test_table),
    ct:pal("Suite2: Current ETS table contents: ~p", [AllData]),
    
    timer:sleep(1500),
    ok.

test_application_env(_Config) ->
    ct:pal("Suite2: Testing application environment"),
    
    %% Check our application environment value
    {ok, Value} = application:get_env(test_example, shared_config_key),
    suite2_value = Value,  % This would fail if suite1 was also running in same node
    ct:pal("Suite2: Application env shared_config_key = ~p (expected suite2_value)", [Value]),
    
    %% Modify it
    application:set_env(test_example, shared_config_key, {suite2_modified, erlang:system_time()}),
    {ok, NewValue} = application:get_env(test_example, shared_config_key),
    ct:pal("Suite2: Modified application env to: ~p", [NewValue]),
    
    timer:sleep(1500),
    ok.

%% Test server process for handling messages
test_server_loop() ->
    receive
        {ping, From} ->
            From ! {pong, suite2},
            test_server_loop();
        stop ->
            ct:pal("Suite2: Test server process stopping"),
            ok;
        Other ->
            ct:pal("Suite2: Test server received unexpected message: ~p", [Other]),
            test_server_loop()
    end.
