-module(rebar3_abnfc_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    rebar3_abnfc_prv_compile:init(State0).
