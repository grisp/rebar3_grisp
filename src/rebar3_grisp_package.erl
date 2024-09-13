-module(rebar3_grisp_package).

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

%--- Macros --------------------------------------------------------------------

-define(NAMESPACE, grisp).
-define(TASK, package).

%--- Callbacks -----------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {namespace, ?NAMESPACE},
            {name, ?TASK},
            {module, ?MODULE},
            {bare, true},
            {deps, []},
            {example, "rebar3 grisp " ++ atom_to_list(?TASK)},
            {opts, [
                {platform, $p, "platform", {string, "grisp2"}, "Platform to list packages for"},
                {columns, $c, "columns", string, "List columns to display"},
                {type, $t, "type", {string, "otp"}, "Package type"},
                {cached, $c, "cached", {boolean, false}, "List only cached packages"}
            ]},
            {profiles, [grisp]},
            {short_desc, "Pre-built package tasks"},
            {desc,
                "Pre-built packages tasks~n"
                "~n"
                "Commands:~n"
                "  list\t\tList packages~n"
            }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    {Args, _Rest} = ParsedArgs = rebar_state:command_parsed_args(RState),
    RState2 = case proplists:get_value(help, Args) of
        true ->
            task_help(proplists:get_value(task, Args), ParsedArgs, RState);
        undefined ->
            task_run(proplists:get_value(task, Args), ParsedArgs, RState)
    end,
    {ok, RState2}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%--- Internal ------------------------------------------------------------------

task_help("list", _Args, RState) ->
    console(
        "List packages~n"
        "~n"
        "Usage: rebar3 grisp packages list [--platform <platform>]~n"
        "~n"
        "  -p, --platform  Platform to list packages for~n"
        "  -x, --hash      List package hashes [default: false]~n"
    ),
    RState;
task_help("download", _Args, RState) ->
    console(
        "Download packages~n"
        "~n"
        "Foo"
    ),
    RState;
task_help(undefined, _Args, RState) ->
    Args = [atom_to_list(A) || A <- [?NAMESPACE, ?TASK]],
    {ok, RState2} = rebar_prv_help:do(rebar_state:command_args(RState, Args)),
    RState2.

task_run("list", {Args, _Rest}, RState) ->
    Config = rebar3_grisp_util:config(RState),
    Platform = to_atom(proplists:get_value(platform, Args, rebar3_grisp_util:platform(Config))),
    Type = to_atom(proplists:get_value(type, Args, otp)),
    Cached = to_atom(proplists:get_value(cached, Args, false)),
    case Type of
        otp ->
            info("GRiSP pre-built OTP versions for platform '~p'", [Platform]);
        toolchain ->
            info("GRiSP pre-built toolchain packages");
        Other ->
            abort("Unknown package type: ~p", [Other])
    end,
    try
        Files = grisp_tools:list_packages(#{
            type => Type,
            platform => Platform,
            source => case Cached of true -> cache; _ -> online end
        }),
        Columns = parse_columns(Type, proplists:get_value(columns, Args)),
        table(format(Files), Columns)
    catch
        error:{not_implemented, Type, Source} ->
            abort("Listing of ~p ~p packages not supported", [Source, Type])
    end,
    RState;
task_run("download", {Args, _Rest}, RState) ->
    console("~p", [Args]),
    RState;
task_run(Task, _Args, _State) ->
    abort("~p: unknown task: ~s", [?TASK, Task]).

parse_columns(otp, undefined) ->
    [version];
parse_columns(toolchain, undefined) ->
    [os, latest, os_version, url];
parse_columns(Type, Arg) ->
    case Arg of
        [] ->
            abort_columns(Type, "No columns specified");
        _ ->
            ok
    end,
    Columns = [list_to_atom(C) || C <- string:split(Arg, ",", all)],
    lists:foreach(fun(C) ->
        case lists:member(C, columns(Type)) of
            true -> ok;
            false -> abort_columns(Type, "Unknown column: ~p", [C])
        end
    end, Columns),
    Columns.

columns(otp) -> [version,hash|columns(default)];
columns(toolchain) -> [os,os_version,revision,latest|columns(default)];
columns(default) -> [name,size,etag,url,last_modified].

to_atom(Term) when is_atom(Term) -> Term;
to_atom(Term) when is_list(Term) -> list_to_atom(Term).

format(Files) ->
    lists:map(fun(#{last_modified := Modified, size := Size} = F) ->
        F#{
            last_modified => format_datetime(Modified),
            size => format_size(Size)
        }
    end, Files).

table([], _Columns) ->
    warn("No packages found");
table(Items, Columns) ->
    Items2 = lists:map(fun format_latest/1, Items),
    All = lists:usort(fun(A, B) ->
        compare(values(A, Columns), values(B, Columns))
    end, Items2),
    Table = grid:format(All, #{header => titlecase, columns => Columns}),
    io:format(Table).

format_size(Size) ->
    format_size(Size / 1.0, ["B", "KiB", "MiB", "GiB", "TiB"]).

format_size(Size, [Unit|Units]) when Size =< 1000; length(Units) == 0 ->
    Decimals = case trunc(Size) == Size of
        true -> 0;
        false -> 1
    end,
    iolist_to_binary([float_to_list(Size, [{decimals, Decimals}]), " ", Unit]);
format_size(Size, [_|Units]) ->
    format_size(Size / 1024, Units).

format_datetime(DateTime) ->
    iolist_to_binary(calendar:system_time_to_rfc3339(DateTime)).

format_latest(#{latest := false} = Item) -> Item#{latest => ""};
format_latest(#{latest := true} = Item) -> Item#{latest => "true"}.

abort_columns(Type, Msg) -> abort_columns(Type, Msg, []).
abort_columns(Type, Msg, Args) ->
    Cs = lists:join($\n, [["  ", atom_to_binary(C)] || C <- columns(Type)]),
    abort(Msg ++ "~nValid columns:~n~s", Args ++ [Cs]).

values(Item, Columns) -> [{C, maps:get(C, Item, undefined)} || C <- Columns].

compare([], []) ->
    true;
compare([ValueA|ValuesA], [ValueB|ValuesB]) ->
    lte(ValueA, ValueB) andalso compare(ValuesA, ValuesB).

lte({os_version, A}, {os_version, B}) ->
    ec_semver:lte(ec_semver:parse(A), ec_semver:parse(B));
lte({version, A}, {version, B}) ->
    ec_semver:lte(ec_semver:parse(A), ec_semver:parse(B));
lte({latest, _A}, {latest, _B}) ->
    true;
lte({_, A}, {_, B}) ->
    A =< B.
