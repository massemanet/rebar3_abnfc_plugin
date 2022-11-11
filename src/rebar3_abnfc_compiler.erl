-module(rebar3_abnfc_compiler).

-export([compile/2]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t(), rebar_state:t()) -> ok.
compile(AppInfo, _State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = filename:join(rebar_app_info:out_dir(AppInfo), ebin),
    AbnfcOpts = abnfc_opts(AppInfo),
    %% search for .abnf files
    ABNFs = discover(AppDir),
    rebar_api:debug("abnf files found (~s, ~s):~n~p~n", [AppDir, OutDir, ABNFs]),
    do_compile(ABNFs, dict:store(o, OutDir, AbnfcOpts)).

%% ===================================================================

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
    Beam = get_target(ABNF, AbnfcOpts),
    BeamTime = filelib:last_modified(Beam),
    AbnfTime = filelib:last_modified(ABNF),
    rebar_api:debug("(~s, ~s):~n(~p, ~p)~n", [Beam, ABNF, BeamTime, AbnfTime]),
    case BeamTime < AbnfTime of
        true -> ok = do_compile(ABNF, Beam, AbnfcOpts);
        false -> ok
      end,
    do_compile(ABNFs, AbnfcOpts).

get_target(ABNF, AbnfcOpts) ->
    flat("~s/~s.beam", [dict:fetch(o, AbnfcOpts), filename:basename(ABNF, ".abnf")]).

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

-spec do_compile(string(), string(), proplists:proplist()) -> ok.
do_compile(Source, Target, AbnfcOpts) ->
    rebar_api:debug("compiling ~p to ~p", [Source, Target]),
    rebar_api:debug("compiler: ~p", [dict:to_list(AbnfcOpts)]),
    
    case abnfc:file(Source, Target, []) of
        ok ->
            ok;
        {ok, []} ->
            ok;
        {error, Reason} ->
            ReasonStr = abnfc_compile:format_error(Reason),
            rebar_utils:abort("failed to compile ~s: ~s~n", [Source, ReasonStr])
    end.
