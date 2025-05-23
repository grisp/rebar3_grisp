-module(rebar3_grisp_firmware).

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
            {name, firmware},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, install_deps}, {default, compile}]},
            {example, "rebar3 grisp firmware"},
            {opts, [
                {relname, $n, "relname", string, "Specify the name for the release that will be deployed"},
                {relvsn, $v, "relvsn", string, "Specify the version of the release"},
                {bundle, undefined, "bundle", string, "The release bundle to use in the firmware"},
                {refresh, $r, "refresh", {boolean, false}, "Force bundle deploy even if it already exists"},
                {force, $f, "force", {boolean, false}, "Replace existing files"},
                {compress, $z, "compress", {boolean, true}, "Compress the output files"},
                {system, $s, "system", {boolean, true}, "Generate a system firmware under _grisp/firmware"},
                {image, $i, "image", {boolean, false}, "Generate an eMMC image firmware under _grisp/firmware"},
                {bootloader, $b, "bootloader", {boolean, false}, "Generate a bootloader firmware under _grisp/firmware"},
                {truncate, $t, "truncate", {boolean, true}, "Truncate the generated image firmware to contain only the first partition"},
                {quiet, $q, "quiet", {boolean, false}, "Do not show the instructions on how to burn the firmwares"} 
            ]},
            {profiles, [grisp]},
            {short_desc, "Generate GRiSP firmware image files"},
            {desc,
                "Generate GRiSP complete firmware image or/and system firmware "
                "compatible with GRiSP A/B updates.\n"
                "\n"
                "If no bundle file is specified, it will be generated by "
                "calling 'rebar3 grisp deploy' with the optional release name "
                "and version. As for the deploy command, options passed after "
                "'--' are sent to the rebar 3 release task.\n"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    try
        {Args, ExtraArgs} = rebar_state:command_parsed_args(RState),
        case [proplists:get_value(N, Args) || N <- [bootloader, image, system]] of
            [false, false, false] -> erlang:error(no_firmware_to_be_extracted);
            _ -> ok
        end,
        RelNameArg = proplists:get_value(relname, Args, undefined),
        RelVsnArg = proplists:get_value(relvsn, Args, undefined),
        {RelName, RelVsn}
            = rebar3_grisp_util:select_release(RState, RelNameArg, RelVsnArg),
        case proplists:get_value(bundle, Args, undefined) of
            undefined ->
                Refresh = proplists:get_value(refresh, Args, false),
                case get_bundle(RState, Refresh, RelName, RelVsn, ExtraArgs) of
                    {error, _Reason} = Error -> Error;
                    {ok, BundleFile, RState2} ->
                        grisp_tools_firmware(RState2, RelName, RelVsn,
                                             [{bundle, BundleFile} | Args])
                end;
            BundleFile ->
                case filelib:is_file(BundleFile) of
                    false ->
                        abort("Bundle file not found: ~s", [BundleFile]);
                    true ->
                        RelPath = grisp_tools_util:maybe_relative(BundleFile, ?MAX_DDOT),
                        console("* Using provided bundle: ~s", [RelPath]),
                        grisp_tools_firmware(RState, RelName, RelVsn, Args)
                end
        end
    catch
        error:no_firmware_to_be_extracted ->
            abort(
                "No firmware selected.~n"
                "You must either select image ot boottloader firmware with options~n"
                "-i/--image or -b/--boot, or not disable system firmware.~n",
                []
            );
        error:{release_not_selected, [{Name, [Version|_]}|_]} ->
            abort(
                "Multiple releases defined!~n"
                "You must specify a name and optionally a version. Examples:~n"
                "~n"
                "    rebar3 grisp firmware --relname ~p~n"
                "    rebar3 grisp firmware --relname ~p --relvsn ~s~n",
                [Name, Name, Version]
            );
        error:no_release_configured ->
            App = rebar_app_info:name(hd(rebar_state:project_apps(RState))),
            abort(
                "No release configured"
                "~n"
                "You must specify at least one release in 'rebar.config' to be "
                "able to create a firmware.~nExample:~n"
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

toolchain_root(RebarState) ->
    case rebar3_grisp_util:toolchain_root(RebarState) of
        undefined -> undefined;
        {docker, _DockerImage} = Result -> Result;
        {directory, _Directory} = Result -> Result;
        {error, docker_not_found} -> abort("Docker is not available")
    end.

get_bundle(RState, Refresh, RelName, RelVsn, ExtraRelArgs) ->
    BundleFile = rebar3_grisp_util:bundle_file_path(RState, RelName, RelVsn),
    case filelib:is_file(BundleFile) of
        true when Refresh =:= false ->
            RelPath = grisp_tools_util:maybe_relative(BundleFile, ?MAX_DDOT),
            console("* Using existing bundle: ~s", [RelPath]),
            {ok, BundleFile, RState};
        _ ->
            console("* Deploying bundle...", []),
            case deploy_bundle(RState, Refresh, RelName,
                               RelVsn, ExtraRelArgs) of
                {ok, RState2} -> {ok, BundleFile, RState2};
                {error, _Reason} = Error -> Error
            end
    end.

deploy_bundle(RState, Refresh, RelName, RelVsn, ExtraRelArgs) ->
    Args = [
        "as"
    ] ++ [
        lists:join(",", [atom_to_list(P)
                         || P <- rebar_state:current_profiles(RState)])
    ] ++ [
        "grisp",
        "deploy",
        "--tar",
        "--relname", atom_to_list(RelName),
        "--relvsn", RelVsn,
        "--destination", ""
    ] ++ case Refresh =:= true of
        true -> ["--force"];
        false -> []
    end ++ case ExtraRelArgs of
        [_|_] -> ["--" | ExtraRelArgs];
        _ -> []
    end,
    case rebar3:run(Args) of
        {error, _Reason} = Error -> Error;
        {ok, _} -> {ok, RState}
    end.

grisp_tools_firmware(RState, RelName, RelVsn, Args) ->
    Config = rebar3_grisp_util:config(RState),
    Board = rebar3_grisp_util:platform(Config),
    BundleOpts = proplists:get_value(bundle, Args),
    ForceOpts = proplists:get_value(force, Args, false),
    GenSystemOpts = proplists:get_value(system, Args, true),
    GenImageOpts = proplists:get_value(image, Args, false),
    GenBootOpts = proplists:get_value(bootloader, Args, false),
    TruncateOpts = proplists:get_value(truncate, Args, true),
    CompressOpts = proplists:get_value(compress, Args, true),
    ToolchainRoot = toolchain_root(RState),
    case {ToolchainRoot, GenImageOpts, GenBootOpts} of
        {undefined, A, B} when A =:= true; B =:= true ->
            abort("Cannot generate image or bootloader firmware without a valid toolchain");
        _ -> ok
    end,
    SystemSpec = case GenSystemOpts of
        false -> undefined;
        true -> #{
            compress => CompressOpts,
            target => rebar3_grisp_util:firmware_file_path(RState, system,
                                                           RelName, RelVsn)
        }
    end,
    ImageSpec = case GenImageOpts of
        false -> undefined;
        true -> #{
            compress => CompressOpts,
            truncate => TruncateOpts,
            target => rebar3_grisp_util:firmware_file_path(RState, image,
                                                           RelName, RelVsn)
        }
    end,
    BootSpec = case GenBootOpts of
        false -> undefined;
        true -> #{
            compress => CompressOpts,
            target => rebar3_grisp_util:firmware_file_path(RState, boot,
                                                           RelName, RelVsn)
        }
    end,
    FirmwareSpec = #{
        platform => Board,
        force => ForceOpts,
        toolchain => ToolchainRoot,
        bundle => BundleOpts,
        system => SystemSpec,
        image => ImageSpec,
        boot => BootSpec,
        handlers => grisp_tools:handlers_init(#{
            event => {fun event_handler/2, RState},
            shell => {fun rebar3_grisp_handler:shell/3, #{}}
        })
    },
    State = grisp_tools:firmware(FirmwareSpec),
    #{event := RState2} = grisp_tools:handlers_finalize(State),
    info("Firmware(s) created"),
    case proplists:get_value(quiet, Args) of
        false -> info("~s", [firmware_usage(State)]);
        true -> ok
    end,
    {ok, RState2}.

event_handler(Event, RState) ->
    event(Event),
    {ok, RState}.

event([firmware, prepare]) ->
    console("* Preparing and validating...");
event([firmware, prepare, _, {bootloader, Name}]) ->
    console("    Bootloader selected: ~s", [Name]);
event([firmware, prepare, _, {error, unsupported_platform, Platform}]) ->
    abort("Platform ~s not supported", [Platform]);
event([firmware, prepare, _, {error, bundle_not_found, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Bundle file not found: ~s", [RelPath]);
event([firmware, prepare, _, {error, toolchain_not_found, {directory, Path}}]) ->
    abort("Toolchain root directory not found: ~s", [Path]);
event([firmware, prepare, _, {error, toolchain_not_found, {docker, ImageName}}]) ->
    abort("Toolchain docker image not found: ~s", [ImageName]);
event([firmware, prepare, _, {error, toolchain_required}]) ->
    abort("A toolchain is required to generate image and/or bootloader firmwares", []);
event([firmware, prepare, _, {error, docker_error, Reason}]) ->
    abort("Docker error: ~p", [Reason]);
event([firmware, prepare, _, {error, directory_not_found, Path}]) ->
    abort("Expected directory not found: ~p", [Path]);
event([firmware, prepare, _, {error, file_exists, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("File already exists (use --force to overwrite): ~s", [RelPath]);
event([firmware, prepare, _, {error, file_access, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("File not accessible: ~s", [RelPath]);
event([firmware, prepare, _, {error, not_a_file, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    abort("Not a regular file: ~s", [RelPath]);
event([firmware, build_firmware, create_image]) ->
    console("* Creating disk image...");
event([firmware, build_firmware, create_image, {error, Reason}]) ->
    abort_message("Failed to create firmware image file", Reason);
event([firmware, build_firmware, copy_bootloader]) ->
    console("* Writing bootloader...");
event([firmware, build_firmware, copy_bootloader, {error, Reason}]) ->
    abort_message("Failed to write bootloader", Reason);
event([firmware, build_firmware, create_partitions]) ->
    console("* Creating disk partition table...");
event([firmware, build_firmware, create_partitions, {error, Reason}]) ->
    abort_message("Failed to create partition table", Reason);
event([firmware, build_firmware, format_system]) ->
    console("* Formatting system partitions...");
event([firmware, build_firmware, format_system, {error, Reason}]) ->
    abort_message("Failed to format system partition", Reason);
event([firmware, build_firmware, deploy_bundle]) ->
    console("* Deploying release bundle...");
event([firmware, build_firmware, deploy_bundle, {error, Reason}]) ->
    abort_message("Failed to expand release bundle", Reason);
event([firmware, build_firmware, extract_system]) ->
    console("* Extracting system firmware...");
event([firmware, build_firmware, extract_system, {extracted, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    console("    System firmware exported: ~s", [RelPath]);
event([firmware, build_firmware, extract_system, {error, Reason}]) ->
    abort_message("Failed to extract system firmware", Reason);
event([firmware, build_firmware, extract_image]) ->
    console("* Extracting image firmware...");
event([firmware, build_firmware, extract_image, {extracted, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    console("    eMMC image firmware exported: ~s", [RelPath]);
event([firmware, build_firmware, extract_image, {error, Reason}]) ->
    abort_message("Failed to extract eMMC image firmware", Reason);
event([firmware, build_firmware, extract_boot]) ->
    console("* Extracting bootloader firmware...");
event([firmware, build_firmware, extract_boot, {extracted, Path}]) ->
    RelPath = grisp_tools_util:maybe_relative(Path, ?MAX_DDOT),
    console("    Bootloader firmware exported: ~s", [RelPath]);
event([firmware, build_firmware, extract_boot, {error, Reason}]) ->
    abort_message("Failed to extract bootloader firmware", Reason);
event([firmware, build_firmware, close_image]) ->
    console("* Cleaning up...");
event([firmware, build_firmware, close_image, {error, Reason}]) ->
    abort_message("Failed to cleanup", Reason);
event([firmware, build_firmware, _, {exec, Args}]) ->
    LogLine = iolist_to_binary(lists:join(" ", Args)),
    debug("command: ~s", [LogLine]);
event([firmware, build_firmware, _, {exit, {status, Status}}]) ->
    debug("[exit ~w]", [Status]);
event([firmware, build_firmware, _, {exit, {signal, Sig}}]) ->
    debug("[signal ~w]", [Sig]);
event([firmware, build_firmware, _, {Stream, eof}])
  when Stream =:= stdin; Stream =:= stdout; Stream =:= stderr ->
    debug("[~s closed]", [Stream]);
event([firmware, build_firmware, _, {Stream, Data}])
  when Stream =:= stdin; Stream =:= stdout; Stream =:= stderr ->
    Str = unicode:characters_to_list(Data),
    debug("~s ~s", [stream_tag(Stream), string:strip(Str, right, $\n)]);
event([firmware, build_firmware, _, {Tag, _Term}])
  when Tag =:= result; Tag =:= error ->
    ok;
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

stream_tag(stdin) -> "<<";
stream_tag(stdout) -> "1>";
stream_tag(stderr) -> "2>".

abort_message(Prefix, Msg) when is_binary(Msg) ->
    abort("~s; ~s", [Prefix, Msg]);
abort_message(Prefix, Reason) ->
    abort("~s: ~p", [Prefix, Reason]).

firmware_usage(FirmwareSpec) ->
    FirmwareItems = maps:to_list(maps:with([system, image, boot], FirmwareSpec)),
    FirmwareDesc = #{
        system => {"system", "/dev/mmc1.0"},
        image => {"emmc", "/dev/mmc1"},
        boot => {"bootloader", "/dev/mmc1"}
    },
    Firmwares = [{K, R, L, D} || {K, #{target := T}} <- FirmwareItems,
                                 {L, D} <- [maps:get(K, FirmwareDesc)],
                                 R <- [grisp_tools_util:maybe_relative(T, ?MAX_DDOT)]],
    Prefix = "    ",
    Cautions = iolist_to_binary([
        case FirmwareSpec of
            #{image := #{truncate := true}} ->
                [[Prefix, "- When writing a truncated eMMC image firmware, only the first\n"],
                 [Prefix, "  system partition is written. If the active system is the\n"],
                 [Prefix, "  second one, the board will continue to boot the old software.\n"],
                 [Prefix, "  You will need to manually change the active system partition\n"],
                 [Prefix, "  in the bootloader console and restart the board:\n"],
                 [Prefix, "    $ let state.bootstate.active_system=0\n"],
                 [Prefix, "    $ state -s\n"]];
            _ -> []
        end,
        case FirmwareSpec of
            #{system := #{}} ->
                [[Prefix, "- When writing a system firmware, be sure to do it on the active system\n"],
                 [Prefix, "  partition (/dev/mmc1.0 or /dev/mmc1.1) or the board will continue to\n"],
                 [Prefix, "  boot the old software.\n"]];
            _ -> []
        end
    ]),
    iolist_to_binary([
        ["Instructions to update GRiSP2 firmware(s):\n"],
        case os:type() of
            {unix, darwin} ->
                [[Prefix, "- Copy the relevent firmware(s) to the SD card:\n"],
                 [[Prefix, "    $ cp \"", R, "\" /Volumes/GRISP # ", L, " firmware\n"]
                  || {_, R, L, _} <- Firmwares],
                 [Prefix, "- Unmount the SD card:\n"],
                 [Prefix, "    $ diskutil umount /Volumes/GRISP\n"],
                 [Prefix, "- Open a serial console to the GRiSP board:\n"],
                 [Prefix, "    $ screen /dev/tty.usbserial-0${GRISP_BOARD_SERIAL}1 115200\n"]];
            {unix, linux} ->
                [[Prefix, "- Copy the relevent firmware(s) to the SD card:\n"],
                 [[Prefix, "    $ cp \"", R, "\" /media/$USER/GRISP # ", L, " firmware\n"]
                  || {_, R, L, _} <- Firmwares],
                 [Prefix, "- Unmount the SD card:\n"],
                 [Prefix, "    $ umount /media/$USER/GRISP\n"],
                 [Prefix, "- Open a serial console to the GRiSP board:\n"],
                 [Prefix, "    $ screen /dev/ttyUSB1 115200\n"]];
            _ ->
                [[Prefix, "- Copy the relevent firmware(s) to the SD card.\n"],
                 [Prefix, "- Unmount the SD card.\n"],
                 [Prefix, "- Open a serial console to the GRiSP board.\n"]]
        end,
        [Prefix, "- Insert the SD card.\n"],
        [Prefix, "- Reset the GRiSP board using the onboard reset button.\n"],
        [Prefix, "- Enter into barebox console mode by pressing any key before 3 seconds.\n"],
        [Prefix, "- Write the relevent firmware(s):\n"],
        [[Prefix, "    $ uncompress \"/mnt/mmc/", filename:basename(R), "\" ", D, " # ", L, " firmware\n"]
         || {_, R, L, D} <- Firmwares],
        [Prefix, "- Remove the SD card.\n"],
        [Prefix, "- Reset the GRiSP board again.\n"],
        case Cautions of
            <<>> -> [];
            _ -> ["CAUTIONS:\n", Cautions]
        end
    ]).
