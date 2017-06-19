rebar3_grisp
============

Rebar plugin for the GRiSP project.

Use
---

Add the plugin to your rebar config:

```erlang
{plugins, [rebar3_grisp]}.
```

Then just call your plugin directly in an existing application:

```
$ rebar3 grisp
===> Fetching grisp
===> Compiling grisp
<Plugin Output>
```

Create New Application
----------------------

Prerequisites:

* [Install Plugin Globally](#install-plugin-globally)

To create a new GRiSP project:

```
$ rebar3 new grispapp name=mygrispproject
```

Install Plugin Globally
-----------------------

To install the plugin globally. Add the plugin to your plugins list in
`~/.config/rebar3/rebar.config`:

```erlang
{plugins, [
    rebar3_hex,
    rebar3_grisp
]}.
```
