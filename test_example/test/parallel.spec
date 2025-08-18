%% Parallel execution spec file
{suites, ".", [math_SUITE]}.
{suites, ".", [division_SUITE]}.
{suites, ".", [integration_SUITE]}.

%% Resource conflict suites - these would conflict in single node execution
{suites, ".", [resource_conflict_suite1]}.
{suites, ".", [resource_conflict_suite2]}.

%% Application and file system conflict suites
{suites, ".", [application_conflict_suite]}.
{suites, ".", [file_system_conflict_suite]}.
