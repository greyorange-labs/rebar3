-module(application_conflict_suite).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% This suite tests application-level conflicts that would occur in same node

all() ->
    [test_application_start_stop, test_persistent_term, test_code_loading].

init_per_suite(Config) ->
    ct:pal("AppSuite: Starting application conflict test suite"),

    %% Try to start an application that others might also try to start
    case application:start(inets) of
        ok ->
            ct:pal("AppSuite: Started inets application");
        {error, {already_started, inets}} ->
            ct:pal("AppSuite: inets already started (would conflict in single node)");
        Error ->
            ct:pal("AppSuite: Failed to start inets: ~p", [Error])
    end,

    %% Set some persistent terms that could conflict
    persistent_term:put({test_suite, marker}, {application_conflict_suite, erlang:system_time()}),
    ct:pal("AppSuite: Set persistent term marker"),

    Config.

end_per_suite(_Config) ->
    ct:pal("AppSuite: Cleaning up application conflict suite"),

    %% Clean up persistent terms
    try
        persistent_term:erase({test_suite, marker}),
        ct:pal("AppSuite: Erased persistent term marker")
    catch
        _:_ -> ok
    end,

    %% Stop application
    case application:stop(inets) of
        ok -> ct:pal("AppSuite: Stopped inets application");
        _ -> ok
    end,

    ok.

test_application_start_stop(_Config) ->
    ct:pal("AppSuite: Testing application lifecycle"),

    %% Check if our application is running
    RunningApps = application:which_applications(),
    ct:pal("AppSuite: Currently running applications: ~p", [length(RunningApps)]),

    %% Try to manipulate application environment
    application:set_env(test_example, app_conflict_key, {app_suite, node(), erlang:system_time()}),
    {ok, Value} = application:get_env(test_example, app_conflict_key),
    ct:pal("AppSuite: Set and verified app env: ~p", [Value]),

    timer:sleep(1500),
    ok.

test_persistent_term(_Config) ->
    ct:pal("AppSuite: Testing persistent term conflicts"),

    %% Check our persistent term
    {application_conflict_suite, _Time} = persistent_term:get({test_suite, marker}),
    ct:pal("AppSuite: Verified our persistent term exists"),

    %% Set more persistent terms that could conflict
    persistent_term:put({test_data, shared_counter}, 0),

    %% Increment counter multiple times
    lists:foreach(fun(I) ->
        OldVal = persistent_term:get({test_data, shared_counter}),
        persistent_term:put({test_data, shared_counter}, OldVal + I),
        timer:sleep(100)
    end, lists:seq(1, 10)),

    FinalVal = persistent_term:get({test_data, shared_counter}),
    ct:pal("AppSuite: Final counter value: ~p", [FinalVal]),

    timer:sleep(1500),
    ok.

test_code_loading(_Config) ->
    ct:pal("AppSuite: Testing code loading conflicts"),

    %% Get current loaded modules
    LoadedModules = code:all_loaded(),
    ct:pal("AppSuite: Currently loaded modules: ~p", [length(LoadedModules)]),

    %% Try to purge and reload a module (this would conflict in same node)
    ModuleName = example_math,
    case code:is_loaded(ModuleName) of
        {file, _} ->
            ct:pal("AppSuite: Module ~p is loaded", [ModuleName]);
        false ->
            ct:pal("AppSuite: Module ~p is not loaded", [ModuleName])
    end,

    %% Add some paths that could conflict
    TestPath = "/tmp/test_code_path_" ++ integer_to_list(erlang:unique_integer([positive])),
    case file:make_dir(TestPath) of
        ok ->
            code:add_path(TestPath),
            ct:pal("AppSuite: Added code path: ~p", [TestPath]),
            file:del_dir(TestPath);
        _ -> ok
    end,

    timer:sleep(1500),
    ok.
