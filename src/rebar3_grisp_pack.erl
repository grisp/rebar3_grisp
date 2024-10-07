-module(rebar3_grisp_pack).

% Callbacks
-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-import(rebar3_grisp_util, [
    debug/2,
    info/1,
    info/2,
    console/1,
    console/2,
    warn/1,
    warn/2,
    abort/1,
    abort/2
]).

-define(MAX_DDOT, 2).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, grisp},
            {name, pack},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}, {default, compile}]},
            {example, "rebar3 grisp pack"},
            {opts, [
                {relname, $n, "relname", string, "Specify the name for the release that will be deployed"},
                {relvsn, $v, "relvsn", string, "Specify the version of the release"},
                {system, undefined, "system", string, "An explicit system firmware to use in the update package"},
                {bootloader, undefined, "bootloader", string, "An explicit bootloader firmware to use in the update package"},
                {block_size, undefined, "block-size", integer, "The size of the blocks in bytes, before compression"},
                {key, $k, "key", string, "Private key PEM file to use to sign the update package"},
                {with_bootloader, $b, "with-bootloader", {boolean, false}, "Include a bootloader firmware in the software update package"},
                {refresh, $r, "refresh", {boolean, false}, "Force firmware building even if it already exists"},
                {force, $f, "force", {boolean, false}, "Replace existing files"},
                {quiet, $q, "quiet", {boolean, false}, "Do not show the instructions on how to update a GRiSP2 board"} 
            ]},
            {profiles, [grisp]},
            {short_desc, "Generate a GRiSP software update package"},
            {desc,
                "Generate a tarball that can be used with grisp software update.\n"
                "\n"
                "If no firmware file is specified, it will be generated by "
                "calling 'rebar3 grisp firmware' with the optional release name "
                "and version. As for the firmware command, options passed after "
                "'--' are sent to the rebar 3 release task.\n"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    try
        {Args, ExtraArgs} = rebar_state:command_parsed_args(RState),
        RelNameArg = proplists:get_value(relname, Args, undefined),
        RelVsnArg = proplists:get_value(relvsn, Args, undefined),
        {RelName, RelVsn}
            = rebar3_grisp_util:select_release(RState, RelNameArg, RelVsnArg),
        case {proplists:get_value(firmware, Args, undefined),
              proplists:get_value(bootloader, Args, undefined),
              proplists:get_value(with_bootloader, Args)} of
            {undefined, undefined, true} ->
                Refresh = proplists:get_value(refresh, Args, false),
                case get_firmwares(RState, Refresh, RelName, RelVsn, ExtraArgs) of
                    {error, _Reason} = Error -> Error;
                    {ok, SysFile, BootFile, RState2} ->
                        grisp_tools_pack(RState2, RelName, RelVsn,
                                         [{system, SysFile},
                                          {bootloader, BootFile} | Args])
                end;
            {undefined, undefined, false} ->
                Force = proplists:get_value(refresh, Args, false),
                case get_firmware(RState, Force, RelName, RelVsn, ExtraArgs) of
                    {error, _Reason} = Error -> Error;
                    {ok, SysFile, RState2} ->
                        grisp_tools_pack(RState2, RelName, RelVsn,
                                         [{system, SysFile} | Args])
                end;
            {SysFile, undefined, false}
              when SysFile =/= undefined ->
                case filelib:is_file(SysFile) of
                    false ->
                        abort("System firmware file not found: ~s", [SysFile]);
                    true ->
                        SysRelPath = grisp_tools_util:maybe_relative(SysFile, ?MAX_DDOT),
                        console("* Using provided system firmware: ~s", [SysRelPath]),
                        grisp_tools_pack(RState, RelName, RelVsn, Args)
                end;
            {SysFile, BootFile, _}
              when SysFile =/= undefined, BootFile =/= undefined ->
                case {filelib:is_file(SysFile),
                      filelib:is_file(BootFile)} of
                    {false, _} ->
                        abort("System firmware file not found: ~s", [SysFile]);
                    {_, false} ->
                        abort("Bootloader firmware file not found: ~s", [BootFile]);
                    {true, true} ->
                        SysRelPath = grisp_tools_util:maybe_relative(SysFile, ?MAX_DDOT),
                        BootRelPath = grisp_tools_util:maybe_relative(BootFile, ?MAX_DDOT),
                        console("* Using provided system firmware: ~s", [SysRelPath]),
                        console("* Using provided bootloader firmware: ~s", [BootRelPath]),
                        grisp_tools_pack(RState, RelName, RelVsn, Args)
                end;
            _ ->
                abort("When including the bootloader firmware and an explicit system firmware, both must be explicit; "
                      "the options --system and --bootloader must be specified together.", [])
        end
    catch
        error:{release_not_selected, [{Name, [Version|_]}|_]} ->
            abort(
                "Multiple releases defined!~n"
                "You must specify a name and optionally a version. Examples:~n"
                "~n"
                "    rebar3 grisp pack --relname ~p~n"
                "    rebar3 grisp pack --relname ~p --relvsn ~s~n",
                [Name, Name, Version]
            );
        error:no_release_configured ->
            App = rebar_app_info:name(hd(rebar_state:project_apps(RState))),
            abort(
                "No release configured"
                "~n"
                "You must specify at least one release in 'rebar.config' to be "
                "able to create a software update package.~nExample:~n"
                "~n"
                "    {relx,~n"
                "        {~s, \"0.1.0\", [~s]}~n"
                "    }.~n",
                [App, App]
            );
        error:{unknown_release_name, {Name, _Version}, Names} ->
            abort(
                "Unknown release '~p'~n"
                "~n"
                "Must be one of:" ++ [["~n  ", atom_to_list(N)] || N <- Names],
                [Name]
            );
        error:{unknown_release_version, {Name, Version}, Versions} ->
            abort(
                "Release '~p' has no version ~s~n"
                "~n"
                "Must be one of:" ++ [["~n  ", V] || V <- Versions],
                [Name, Version]
            );
        error:{create_dir_failed, Dir, {error, Reason}} ->
            abort(
                "Error creating directory ~s: ~s",
                [Dir, file:format_error(Reason)]
            )
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%--- Internal ------------------------------------------------------------------

get_firmwares(RState, Refresh, RelName, RelVsn, ExtraRelArgs) ->
    SysFile
        = rebar3_grisp_util:firmware_file_path(RState, system, RelName, RelVsn),
    BootFile
        = rebar3_grisp_util:firmware_file_path(RState, boot, RelName, RelVsn),
    case {filelib:is_file(SysFile), filelib:is_file(BootFile)} of
       {true, true} when Refresh =:= false ->
            SysRelPath = grisp_tools_util:maybe_relative(SysFile, ?MAX_DDOT),
            BootRelPath = grisp_tools_util:maybe_relative(BootFile, ?MAX_DDOT),
            console("* Using existing system firmware: ~s", [SysRelPath]),
            console("* Using existing bootloader firmware: ~s", [BootRelPath]),
            {ok, SysFile, BootFile, RState};
        _ ->
            console("* Building firmwares...", []),
            case build_firmwares(RState, true, Refresh, RelName,
                                 RelVsn, ExtraRelArgs) of
                {ok, RState2} -> {ok, SysFile, BootFile, RState2};
                {error, _Reason} = Error -> Error
            end
    end.

get_firmware(RState, Refresh, RelName, RelVsn, ExtraRelArgs) ->
    SysFile
        = rebar3_grisp_util:firmware_file_path(RState, system, RelName, RelVsn),
    case filelib:is_file(SysFile) of
       true when Refresh =:= false ->
            SysRelPath = grisp_tools_util:maybe_relative(SysFile, ?MAX_DDOT),
            console("* Using existing system firmware: ~s", [SysRelPath]),
            {ok, SysFile, RState};
        _ ->
            console("* Building system firmware...", []),
            case build_firmwares(RState, false, Refresh, RelName,
                                 RelVsn, ExtraRelArgs) of
                {ok, RState2} -> {ok, SysFile, RState2};
                {error, _Reason} = Error -> Error
            end
    end.

build_firmwares(RState, WithBoot, Refresh, RelName, RelVsn, ExtraRelArgs) ->
    Args = [
        "as"
    ] ++ [
        lists:join(",", [atom_to_list(P)
                         || P <- rebar_state:current_profiles(RState)])
    ] ++ [
        "grisp",
        "firmware",
        "--relname", atom_to_list(RelName),
        "--relvsn", RelVsn,
        "--force",
        "--quiet"
    ] ++ case WithBoot of
        true -> ["--bootloader"];
        false -> []
    end ++ case Refresh =:= true of
        true -> ["--refresh"];
        false -> []
    end ++ case ExtraRelArgs of
        [_|_] -> ["--" | ExtraRelArgs];
        _ -> []
    end,
    case rebar3:run(Args) of
        {error, _Reason} = Error -> Error;
        {ok, _} -> {ok, RState}
    end.

grisp_tools_pack(RState, RelName, RelVsn, Args) ->
    SysFile = proplists:get_value(system, Args),
    BootFile = proplists:get_value(bootloader, Args),
    Force = proplists:get_value(force, Args, false),
    KeyFile = proplists:get_value(key, Args),
    BlockSize = proplists:get_value(block_size, Args),
    PackageFile = rebar3_grisp_util:update_file_path(RState, RelName, RelVsn),
    
    PackSpec = #{
        name => atom_to_binary(RelName),
        version => RelVsn,
        block_size => BlockSize,
        key_file => KeyFile,
        system => SysFile,
        bootloader => BootFile,
        package => PackageFile,
        force => Force,
        handlers => grisp_tools:handlers_init(#{
            event => {fun event_handler/2, RState},
            shell => {fun rebar3_grisp_handler:shell/3, #{}}
        })
    },
    State = grisp_tools:pack(PackSpec),
    #{event := RState2} = grisp_tools:handlers_finalize(State),
    info("Package created"),
    case proplists:get_value(quiet, Args) of
        false -> info("~s", [update_usage(RState2, RelName, RelVsn)]);
        true -> ok
    end,
    {ok, RState2}.

event_handler(Event, RState) ->
    event(Event),
    {ok, RState}.

event([pack, prepare]) ->
    console("* Preparing and validating...");
event([pack, prepare, _, {error, firmware_not_found, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("System firmware file not found: ~s", [RelPath]);
event([pack, prepare, _, {error, bootloader_not_found, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Bootloader firmware file not found: ~s", [RelPath]);
event([pack, prepare, _, {error, file_exists, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("File already exists (use --force to overwrite): ~s", [RelPath]);
event([pack, prepare, _, {error, file_access, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("File not accessible: ~s", [RelPath]);
event([pack, prepare, _, {error, not_a_file, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Not a regular file: ~s", [RelPath]);
event([pack, package, _, {expanding, Filename}]) ->
    console("* Expanding compressed file ~s", [Filename]);
event([pack, package, build_package]) ->
    console("* Creating software update package...");
event([pack, package, _, {done, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    console("    Software update package generated: ~s", [RelPath]);
event([pack, package, _, {error, Reason}]) ->
    abort_message("Failed to create package: ~s", Reason);
event(Event) ->
    debug("[rebar3_grisp] ~p", [Event]),
    case lists:last(Event) of
        {error, Reason, Info} when is_binary(Info) ->
            abort("Unexpected ~p error: ~s", [Reason, Info]);
        {error, Reason, Info} ->
            abort("Unexpected ~p error: ~p", [Reason, Info]);
        {error, Reason} ->
            abort("Unexpected ~p error", [Reason]);
        _ -> ok
    end.

abort_message(Prefix, Msg) when is_binary(Msg) ->
    abort("~s; ~s", [Prefix, Msg]);
abort_message(Prefix, Reason) ->
    abort("~s: ~p", [Prefix, Reason]).

local_address() ->
    case inet:getifaddrs() of
        {ok, Addrs} ->
            case [A || {_, P} <- Addrs,
                       A <- [proplists:get_value(addr, P)],
                       {_, _, _, _} <- [A],
                       A =/= {127, 0, 0, 1}] of
                [] -> <<"HOSTNAME">>;
                [Addr | _] -> inet_parse:ntoa(Addr)
            end;
        _ ->
            <<"HOSTNAME">>
    end.

update_usage(RState, RelName0, RelVsn) ->
    RelName = atom_to_binary(RelName0),
    PackageFile0 = rebar3_grisp_util:update_file_path(RState, RelName, RelVsn),
    PackageFile = grisp_tools_util:maybe_relative(PackageFile0, ?MAX_DDOT),
    iolist_to_binary([
        "Instructions to update GRiSP2 software on the board with grisp_updater:\n",
        "     NOTE: the software running on the board MUST run grisp_updater by adding the grisp_updater_grisp2 dependency.\n",
        "    - Unpack the software update package in some local directory:\n",
        "        $ mkdir -p releases/", RelName, "/", RelVsn, "\n",
        "        $ tar -C releases/", RelName, "/", RelVsn, " -xvf ", PackageFile, "\n",
        "    - Start a local HTTP server to serve the package:\n",
        "        $ http-server ./releases -p 8000\n",
        case os:type() of
            {unix, darwin} ->
                ["    - Open a serial console to the GRiSP board:\n",
                 "        $ screen /dev/tty.usbserial-0${GRISP_BOARD_SERIAL}1 115200\n"];
            {unix, linux} ->
                ["    - Open a serial console to the GRiSP board:\n",
                 "        $ screen /dev/ttyUSB1 115200\n"];
            _ ->
                ["    - Open a serial console to the GRiSP board.\n"]
        end,
        "    - On the GRiSP2 console, start the update process:\n",
        "        $ grisp_updater:update(<<\"http://", local_address(), ":8000/", RelName, "/", RelVsn, "\">>).\n",
        "    - Reset the GRiSP2 board using the onboard reset button.\n",
        "    - Validate the new software version on the GRiSP2 console:\n",
        "        $ grisp_updater:validate().\n"
    ]).
