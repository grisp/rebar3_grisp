-module(rebar3_grisp_build).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [debug/2]).
-import(rebar3_grisp_util, [info/1]).
-import(rebar3_grisp_util, [abort/1]).
-import(rebar3_grisp_util, [abort/2]).
-import(rebar3_grisp_util, [warn/2]).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, build},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}]},
            {example, "rebar3 grisp build"},
            {opts, [
                {clean, $c, "clean", {boolean, false},
                    "Completely clean Git repository before building"
                },
                {configure, $g, "configure", {boolean, true},
                    "Run autoconf & configure"
                },
                {tar, $t, "tar", {boolean, false},
                    "Create tarball with OTP installation for online repository"
                }
            ]},
            {profiles, [default]},
            {short_desc, "Build a custom Erlang/OTP system for GRiSP"},
            {desc,
"Build a custom Erlang/OTP system for GRiSP.
"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {Opts, _Rest} = rebar_state:command_parsed_args(RState),

    Config = rebar3_grisp_util:config(RState),
    [abort_no_build() || not rebar3_grisp_util:should_build(Config)],

    Board = rebar3_grisp_util:platform(Config),
    Version = rebar3_grisp_util:otp_version(Config),

    Apps = rebar3_grisp_util:apps(RState),

    ProjectRoot = rebar_dir:root_dir(RState),
    ToolchainRoot = toolchain_root(Config),

    Flags = maps:from_list([
        {F, rebar3_grisp_util:get(F, Opts, D)}
        || {F, D} <- [{clean, false}, {configure, true}, {tar, false}]
    ]),

    try
        State = grisp_tools:build(#{
            project_root => ProjectRoot,
            apps => Apps,
            otp_version_requirement => Version,
            platform => Board,
            build => #{
                flags => Flags
            },
            paths => #{
                toolchain => ToolchainRoot
            },
            handlers => grisp_tools:handlers_init(#{
                event => {fun event_handler/2, #{}},
                shell => {fun rebar3_grisp_handler:shell/3, #{}}
            })
        }),
        _ = grisp_tools:handlers_finalize(State),
        info("Done"),
        {ok, RState}
    catch
        error:{missing_toolchain_revision, Source} ->
            abort(
                "Could not determine toolchain revision "
                "(missing file: ~s)",
                [Source]
            );
        error:{otp_version_not_found, Configured} ->
            abort(
                "Could not find an OTP version matching the configured "
                "version ~p",
                [Configured]
            )
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

toolchain_root(Config) ->
    Conf = rebar3_grisp_util:get([build, toolchain, directory], Config, error),
    case os:getenv("GRISP_TOOLCHAIN", Conf) of
        error ->
            abort("Please specify the full path to the toolchain directory " ++
"in your rebar.conf:

{grisp, [
    ...,
    {build, [
        {toolchain, [
                {directory, \"/PATH/TO/TOOLCHAIN\"}
        ]}
    ]}
]}.

Alternatively, you can set the GRISP_TOOLCHAIN environment variable."
        );
        Directory ->
            Directory
    end.

abort_no_build() ->
    abort("There was no build section found in your rebar.conf").

event_handler(Event, State) ->
    event(Event),
    {ok, State}.

event([build]) ->
    info("Building OTP for GRiSP");
event([build, validate, apps, {grisp_dir_without_dep, A}]) ->
    warn(
        "WARNING! Application ~p has a 'grisp' directory but does not depend"
        " on the grisp application. Its contents will be ignored when compiling"
        " Erlang",
        [A]
    );
event([build, validate, version]) ->
    io:format("* Resolving OTP version~n");
event([build, validate, version, {selected, Version, Target}]) ->
    io:format("    Selected version ~s (requirement was ~s)~n", [Version, Target]);
event([build, download]) ->
    io:format("* Downloading~n");
event([build, download, '_skip']) ->
    io:format("    (skipped, using existing download)~n");
event([build, prepare]) ->
    info("Preparing");
event([build, prepare, clean, '_run']) ->
    io:format("* Cleaning...~n");
event([build, prepare, patch]) ->
    io:format("* Patching~n");
event([build, prepare, patch, {apply, #{app := App, name := File}}]) ->
    io:format(["    [", atom_to_list(App), "]", File] ++ "~n");
event([build, prepare, patch, {skip, #{app := App, name := File}}]) ->
    io:format(["    [", atom_to_list(App), "] ", File, " (already applied, skipping)"] ++ "~n");
event([build, prepare, patch, '_skip']) ->
    io:format("    (no patches found)~n");
event([build, prepare, files]) ->
    io:format("* Copying files~n");
event([build, prepare, files, {copy, #{app := App, name := File}}]) ->
    io:format(["    [", atom_to_list(App), "] ", File] ++ "~n");
event([build, prepare, files, '_skip']) ->
    io:format("    (no files found)~n");
event([build, prepare, drivers]) ->
    io:format("* Copying drivers~n");
event([build, prepare, drivers, {copy, #{app := App, name := File}}]) ->
    io:format(["    [", atom_to_list(App), "] ", File] ++ "~n");
event([build, prepare, drivers, '_skip']) ->
    io:format("    (no drivers found)~n");
event([build, prepare, nifs]) ->
    io:format("* Copying NIFs~n");
event([build, prepare, nifs, {copy, #{app := App, name := File}}]) ->
    io:format(["    [", atom_to_list(App), "] ", File] ++ "~n");
event([build, prepare, nifs, '_skip']) ->
    io:format("    (no NIFs found)~n");
event([build, compile]) ->
    info("Compiling");
event([build, compile, configure]) ->
    io:format("* Configuring~n");
event([build, compile, configure, '_skip']) ->
    io:format("    (skipped)~n");
event([build, compile, boot]) ->
    io:format("* Compiling (this may take a while)~n");
event([build, compile, install]) ->
    io:format("* Installing~n");
event([build, compile, install, hook, post_install, {run, Hook}]) ->
    #{app := App, name := Name} = Hook,
    io:format("    [~p] ~s~n", [App, Name]);
event([build, tar]) ->
    io:format("* Packaging~n");
event([build, tar, {file, File}]) ->
    io:format("    ~s~n", [File]);
event(Event) ->
    case lists:last(Event) of
        {output, _Output} -> ok; % Output is printed by rebar3_grisp_util
        _Else -> debug("[rebar3_grisp] ~p", [Event])
    end.
