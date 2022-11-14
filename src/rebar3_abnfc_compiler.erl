-module(rebar3_abnfc_compiler).

-export([compile/2]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(map(), proplists:proplist()) -> ok.
compile(AppOpts, CommandLineArgs) ->
    Opts = maps:merge(AppOpts, abnfc_args:reify(CommandLineArgs)),
    ABNFs = files(Opts),
    rebar_api:debug("abnf files found:~n~p~n", [ABNFs]),
    lists:foreach(fun(ABNF) -> compile1(ABNF, Opts) end, ABNFs).

%% ===================================================================

files(#{infile := File}) -> [File];
files(#{app_dir := AppDir}) ->
    %% Convert simple extension to proper regex
    SourceExtRe = "^[^._].*\\" ++ ".abnf" ++ [$$],
    Recursive = false,
    %% Find all possible source files
    rebar_utils:find_files(filename:join([AppDir, src]), SourceExtRe, Recursive).

compile1(ABNF, Opts) ->
    Target = get_target(ABNF, Opts),
    TargetTime = filelib:last_modified(Target),
    AbnfTime = filelib:last_modified(ABNF),
    rebar_api:debug("(~s, ~s):~n(~p, ~p)~n", [Target, ABNF, TargetTime, AbnfTime]),
    [cmp(ABNF, Opts) || TargetTime < AbnfTime].

get_target(_, #{mod := Mod, o := OutDir}) -> fn(OutDir, Mod);
get_target(ABNF, #{o := OutDir}) -> fn(OutDir, filename:basename(ABNF, ".abnf")).

fn(OutDir, Mod) ->
    lists:flatten(io_lib:format("~s/~s.beam", [OutDir, Mod])).

-spec cmp(string(), map()) -> ok.
cmp(Source, Opts) ->
    rebar_api:debug("compiling ~s: ~p", [Source, Opts]),
    case abnfc:file(Source, Opts) of
        {ok, _Mod, []} ->
            ok;
        {error, Reason, []} ->
            ReasonStr = abnfc_compile:format_error(Reason),
            rebar_utils:abort("failed to compile ~s: ~s~n", [Source, ReasonStr])
    end.
