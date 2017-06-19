rebar3_grisp
============

Rebar plugin for the GRiSP project.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {grisp, ".*", {git, "git@host:user/grisp.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 grisp
    ===> Fetching grisp
    ===> Compiling grisp
    <Plugin Output>
