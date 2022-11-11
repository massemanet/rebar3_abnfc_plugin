Rebar3 abnfc plugin
=====

A rebar3 plugin for automatically compiling .abnf files using the abnfc compiler.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
{erl_opts, [{i, "./_build/default/plugins/abnfc/include"}]}.

{plugins, [
    {rebar3_abnfc_plugin, "0.0.0"}
]}.
```

Configure abnfc options, e.g.

```erlang
{abnfc_opts, []}.
```
