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
                {docker, $d, "docker", {boolean, false},
                "Mounts the entire project as docker volume and uses a docker"
                " image to build OTP"
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
    CustomBuild = rebar3_grisp_util:should_build(Config),
    [abort_no_build() || not CustomBuild],

    Board = rebar3_grisp_util:platform(Config),
    Version = rebar3_grisp_util:otp_version(Config),

    Apps = rebar3_grisp_util:apps(RState),

    ProjectRoot = rebar_dir:root_dir(RState),

    Flags = maps:from_list([
        {F, rebar3_grisp_util:get(F, Opts, D)}
        || {F, D} <- [
            {clean, false},
            {docker, false},
            {configure, true},
            {tar, false}]
    ]),

    try
        State = grisp_tools:build(#{
            project_root => ProjectRoot,
            apps => Apps,
            otp_version_requirement => Version,
            platform => Board,
            custom_build => CustomBuild,
            build => #{
                flags => Flags
            },
            paths => case Flags of
                #{docker := false} ->
                    #{toolchain => toolchain_root(Config)};
                _ -> #{}
            end,
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
        error:{toolchain_root_invalid, Dir} ->
            abort(
                "The toolchain dir is not valid: ~p",
                [Dir]
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

Alternatively, you can set the GRISP_TOOLCHAIN environment variable.

If you don't have a toolchain we provide one in form of a docker image:
rebar3 grisp build --docker"
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
event([build, {platform, Platform}]) ->
    io:format("* Platform: ~p~n", [Platform]);
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
    io:format("    ~s (requirement was \"~s\")~n", [Version, Target]);
event([build, validate, version, {error, Cmd, Output}]) ->
    warn("Could not fetch OTP versions from remote:~n$ ~s~n~s", [Cmd, Output]);
event([build, repo, check, {error, Error}]) ->
    warn("Repository integrity check failed: ~p ~n", [Error]);
event([build, collect, {hash, Hash, Index}]) ->
    debug("GRiSP hash:~n~s~n~n~p", [Hash, Index]);
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
    io:format("    [~p] ~s~n", [App, File]);
event([build, prepare, patch, {skip, #{app := App, name := File}}]) ->
    io:format("    [~p] ~s (already applied, skipping)~n", [App, File]);
event([build, prepare, copy, Type]) ->
    io:format("* Copying ~p~n", [Type]);
event([build, prepare, copy, _Type, '_skip']) ->
    io:format("    (none found)~n");
event([build, prepare, copy, _Type, #{app := App, name := File}]) ->
    io:format("    [~p] ~s~n", [App, File]);
event([build, compile]) ->
    info("Compiling");
event([build, compile, configure]) ->
    io:format("* Configuring~n");
event([build, compile, configure, {'_override', download}]) ->
    io:format("    (forced, fresh download)~n");
event([build, compile, configure, {'_override', clean}]) ->
    io:format("    (forced, repository cleaned)~n");
event([build, compile, configure, '_skip']) ->
    io:format("    (skipped)~n");
event([build, compile, boot]) ->
    io:format("* Compiling (this may take a while)~n");
event([build, compile, install]) ->
    io:format("* Installing~n");
event([build, compile, install, hook, post_install, {run, Hook}]) ->
    #{app := App, name := Name} = Hook,
    io:format("    [~p] ~s~n", [App, Name]);
event([build, tar, {file, File}]) ->
    io:format("* Packaging~n    ~s~n", [File]);
event(Event) ->
    case lists:last(Event) of
        {output, _Output} -> ok; % Output is printed by rebar3_grisp_util
        _Else -> debug("[rebar3_grisp] ~p", [Event])
    end.
