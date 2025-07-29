-module(file_system_conflict_suite).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% This suite tests file system conflicts that would occur in same node

all() ->
    [test_file_operations, test_directory_locks, test_temp_files].

init_per_suite(Config) ->
    ct:pal("FileSuite: Starting file system conflict test suite"),

    %% Create a shared directory that other suites might also create
    SharedDir = "/tmp/shared_test_dir",
    case file:make_dir(SharedDir) of
        ok ->
            ct:pal("FileSuite: Created shared directory: ~p", [SharedDir]);
        {error, eexist} ->
            ct:pal("FileSuite: Shared directory already exists (would conflict in single node)");
        Error ->
            ct:pal("FileSuite: Failed to create directory: ~p", [Error])
    end,

    %% Create a lock file
    LockFile = SharedDir ++ "/test.lock",
    case file:write_file(LockFile, integer_to_binary(erlang:system_time())) of
        ok -> ct:pal("FileSuite: Created lock file");
        Error2 -> ct:pal("FileSuite: Failed to create lock file: ~p", [Error2])
    end,

    [{shared_dir, SharedDir}, {lock_file, LockFile} | Config].

end_per_suite(Config) ->
    ct:pal("FileSuite: Cleaning up file system conflict suite"),

    %% Clean up our files and directories
    LockFile = proplists:get_value(lock_file, Config),
    SharedDir = proplists:get_value(shared_dir, Config),

    file:delete(LockFile),
    file:del_dir(SharedDir),
    ct:pal("FileSuite: Cleaned up shared directory and lock file"),

    ok.

test_file_operations(Config) ->
    ct:pal("FileSuite: Testing file operation conflicts"),

    SharedDir = proplists:get_value(shared_dir, Config),

    %% Multiple operations on same files that would conflict
    TestFile = SharedDir ++ "/conflict_test.txt",

    %% Write some data
    Data = <<"FileSuite data at ", (integer_to_binary(erlang:system_time()))/binary>>,
    ok = file:write_file(TestFile, Data),
    ct:pal("FileSuite: Wrote to test file"),

    %% Read it back multiple times with delays (simulating conflicts)
    lists:foreach(fun(I) ->
        {ok, ReadData} = file:read_file(TestFile),
        ct:pal("FileSuite: Read ~p: ~p", [I, byte_size(ReadData)]),

        %% Append more data
        AppendData = <<"Append ", (integer_to_binary(I))/binary, "\n">>,
        ok = file:write_file(TestFile, AppendData, [append]),
        timer:sleep(200)
    end, lists:seq(1, 5)),

    timer:sleep(1000),
    ok.

test_directory_locks(Config) ->
    ct:pal("FileSuite: Testing directory lock conflicts"),

    SharedDir = proplists:get_value(shared_dir, Config),

    %% Create subdirectories that could conflict
    SubDirs = [SharedDir ++ "/sub" ++ integer_to_list(I) || I <- lists:seq(1, 3)],

    lists:foreach(fun(Dir) ->
        case file:make_dir(Dir) of
            ok ->
                ct:pal("FileSuite: Created subdir: ~p", [Dir]);
            {error, eexist} ->
                ct:pal("FileSuite: Subdir already exists: ~p", [Dir]);
            Error ->
                ct:pal("FileSuite: Failed to create subdir ~p: ~p", [Dir, Error])
        end,

        %% Create a file in each subdirectory
        TestFile = Dir ++ "/test.dat",
        file:write_file(TestFile, <<"file_suite_", (list_to_binary(Dir))/binary>>),

        timer:sleep(300)
    end, SubDirs),

    timer:sleep(1000),
    ok.

test_temp_files(Config) ->
    ct:pal("FileSuite: Testing temporary file conflicts"),

    %% Create multiple temp files with same naming pattern
    TempFiles = ["/tmp/test_temp_" ++ integer_to_list(I) ++ ".tmp" || I <- lists:seq(1, 5)],

    %% Write to all temp files concurrently
    lists:foreach(fun(TempFile) ->
        spawn(fun() ->
            Data = <<"FileSuite temp data ", (list_to_binary(TempFile))/binary>>,
            case file:write_file(TempFile, Data) of
                ok ->
                    ct:pal("FileSuite: Created temp file: ~p", [TempFile]),
                    timer:sleep(500),
                    file:delete(TempFile),
                    ct:pal("FileSuite: Deleted temp file: ~p", [TempFile]);
                Error ->
                    ct:pal("FileSuite: Failed to create temp file ~p: ~p", [TempFile, Error])
            end
        end)
    end, TempFiles),

    %% Wait for all operations to complete
    timer:sleep(2000),

    %% Verify cleanup
    RemainingFiles = [F || F <- TempFiles, filelib:is_file(F)],
    ct:pal("FileSuite: Remaining temp files: ~p", [RemainingFiles]),

    ok.
