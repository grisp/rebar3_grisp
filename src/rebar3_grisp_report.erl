-module(rebar3_grisp_report).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [debug/2]).
-import(rebar3_grisp_util, [info/1]).
-import(rebar3_grisp_util, [console/1]).
-import(rebar3_grisp_util, [console/2]).
-import(rebar3_grisp_util, [abort/1]).
-import(rebar3_grisp_util, [abort/2]).
-import(rebar3_grisp_util, [warn/2]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, report},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}]},
            {example, "rebar3 grisp report"},
            {opts, [
                {tar, $t, "tar", {boolean, false},
                    "Packs all report files in a tarball"
                }
            ]},
            {profiles, [default]},
            {short_desc, "Bug Report Utility"},
            {desc,"Gathers data about the current grisp project."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {Opts, _Rest} = rebar_state:command_parsed_args(RState),
    Config = rebar3_grisp_util:config(RState),

    CustomBuild = rebar3_grisp_util:should_build(Config),
    Board = rebar3_grisp_util:platform(Config),
    Version = rebar3_grisp_util:otp_version(Config),

    Apps = rebar3_grisp_util:apps(RState),

    ProjectRoot = rebar_dir:root_dir(RState),

    ReportDir = rebar3_grisp_util:report_dir(RState),

    Flags = maps:from_list([
        {F, rebar3_grisp_util:get(F, Opts, D)}
        || {F, D} <- [{tar, false}]
    ]),

    try
        State = grisp_tools:report(#{
            project_root => ProjectRoot,
            report_dir => ReportDir,
            flags => Flags,
            apps => Apps,
            otp_version_requirement => Version,
            custom_build => CustomBuild,
            platform => Board,
            handlers => grisp_tools:handlers_init(#{
                event => {fun event_handler/2, #{}},
                shell => {fun rebar3_grisp_handler:shell/3, #{}}
            })
        }),
        _ = grisp_tools:handlers_finalize(State),
        info("Done"),
        {ok, RState}
    catch
        error:E ->
            abort(
                "Unexpected error: ~p",
                [E]
            )
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

event_handler(Event, State) ->
    event(Event),
    {ok, State}.
event([report]) ->
    console("Grisp report");
event([report, clean, info, Path]) ->
    console("======================"),
    console(io_lib:format("Please check the content of ~s.~n"
                          "Feel free to remove any private information.",
                          [Path])),
    console("----------------------");
event([report, copy, files, {init, _Dest}]) ->
    console("* Copied files...");
event([report, project_settings, files, {copy, Filename}]) ->
    console("Copied -> ~p",[Filename]);
event([report, validate, version]) ->
    console("* Resolving OTP version");
event([report, validate, version, {selected, Version, Target}]) ->
    io:format("    ~s (requirement was \"~s\")~n", [Version, Target]);
event([report, collect, {hash, Hash, Index}]) ->
    debug("GRiSP hash:~n~s~n~n~p", [Hash, Index]);
event([report, _, write, Filename]) ->
    console("Written -> ~p",[Filename]);
event([report, tar, Filename]) ->
    console("Created tarball -> ~p",[Filename]);
event(Event) ->
    case lists:last(Event) of
        {output, _Output} -> ok; % Output is printed by rebar3_grisp_util
        _Else -> debug("[rebar3_grisp] ~p", [Event])
    end.
