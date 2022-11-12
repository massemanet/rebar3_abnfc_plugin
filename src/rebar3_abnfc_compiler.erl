-module(rebar3_abnfc_compiler).

-export([compile/2]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t(), rebar_state:t()) -> ok.
compile(AppInfo, CommandLineArgs) ->
    OutDir = filename:join(rebar_app_info:out_dir(AppInfo), ebin),
    AbnfcOpts = abnfc_opts(AppInfo),
    %% search for .abnf files
    ABNFs = files(CommandLineArgs, AppInfo),
    rebar_api:debug("abnf files found:~n~p~n", [ABNFs]),
    do_compile(ABNFs, dict:store(o, OutDir, AbnfcOpts)).

%% ===================================================================

files(CommandLineArgs, AppInfo) ->
    case CommandLineArgs of
        {[{task, File}], _} -> [File];
        _ -> discover(rebar_app_info:dir(AppInfo))
    end.

abnfc_opts(AppInfo) ->
    case dict:find(abnfc_opts, rebar_app_info:opts(AppInfo)) of
        error -> dict:new();
        AbnfcOpts -> AbnfcOpts
    end.

discover(AppDir) ->
    %% Convert simple extension to proper regex
    SourceExtRe = "^[^._].*\\" ++ ".abnf" ++ [$$],

    Recursive = false,
    %% Find all possible source files
    rebar_utils:find_files(filename:join([AppDir, src]), SourceExtRe, Recursive).

do_compile([], _ABNFs) -> ok;
do_compile([ABNF|ABNFs], AbnfcOpts) ->
    Target = get_target(ABNF, AbnfcOpts),
    TargetTime = filelib:last_modified(Target),
    AbnfTime = filelib:last_modified(ABNF),
    rebar_api:debug("(~s, ~s):~n(~p, ~p)~n", [Target, ABNF, TargetTime, AbnfTime]),
    case TargetTime < AbnfTime of
        true -> ok = cmp(ABNF, AbnfcOpts);
        false -> ok
      end,
    do_compile(ABNFs, AbnfcOpts).

get_target(ABNF, AbnfcOpts) ->
    flat("~s/~s.beam", [dict:fetch(o, AbnfcOpts), filename:basename(ABNF, ".abnf")]).

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

-spec cmp(string(), proplists:proplist()) -> ok.
cmp(Source, AbnfcOpts) ->
    Target = filename:dirname(Source),
    rebar_api:debug("compiling ~p to ~p", [Source, Target]),
    rebar_api:debug("compiler: ~p", [dict:to_list(AbnfcOpts)]),
    case abnfc:file(Source, Target, [verbose]) of
        ok ->
            ok;
        {ok, _} ->
            ok;
        {error, Reason, []} ->
            ReasonStr = abnfc_compile:format_error(Reason),
            rebar_utils:abort("failed to compile ~s: ~s~n", [Source, ReasonStr])
    end.
