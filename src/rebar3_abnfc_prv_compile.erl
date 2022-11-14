-module(rebar3_abnfc_prv_compile).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [{default, app_discovery}]).
-define(SHORT_DESC, "Compile ABNF (.abnf) files using the abnfc compiler").
-define(DESC,"Configure abnfc options (abnfc_opts) in your rebar.config, e.g.\n"
             "  {abnfc_opts,[]}).").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, abnf},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {opts, 
             [{out, $o, "out_dir", string, "dir"},
              {module, $m, "mod", atom, "mod"},
              {verbose, $v, "dbg", boolean, "dbg"}]},
            {example, "rebar3 abnf compile"},
            {short_desc, ?SHORT_DESC},
            {desc, ?DESC}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    {Args, []} = rebar_state:command_parsed_args(State),
    AppInfos = app_infos(State),
    lists:foreach(fun(AppInfo) -> compile(AppInfo, Args) end, AppInfos),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================

app_infos(State) ->
    case rebar_state:current_app(State) of
        undefined -> rebar_state:project_apps(State);
        AppInfo -> [AppInfo]
    end.

compile(AppInfo, Args) ->
    rebar3_abnfc_compiler:compile(opts(AppInfo), Args).

opts(AppInfo) ->
    Opts0 = #{o => filename:join(rebar_app_info:out_dir(AppInfo), ebin),
              app_dir => rebar_app_info:dir(AppInfo)},
    maps:merge(Opts0, abnfc_opts(AppInfo)).

abnfc_opts(AppInfo) ->
    case dict:find(abnfc_opts, rebar_app_info:opts(AppInfo)) of
        error -> #{};
        {ok, AbnfcOpts} -> maps:from_list(dict:to_list(AbnfcOpts))
    end.
