rebar3_grisp
============

Rebar plug-in for the GRiSP project.

Use
---

Add the plug-in to your rebar config:

```erlang
{plugins, [rebar3_grisp]}.
```

Then just call your plug-in directly in an existing application:

```
$ rebar3 grisp
===> Fetching grisp
===> Compiling grisp
<Plugin Output>
```

Create New Application
----------------------

Prerequisites:

* [Install Plug-In Globally](#install-plug-in-globally)

To create a new GRiSP project:

```
$ rebar3 new grispapp name=mygrispproject dest=/path/to/SD-card
```

The specific variables provided by this plug-in are:

* **`name`** is the name of the OTP application
* **`dest`** is the destination path for deployment. This should point to where
  your SD-card is mounted (e.g. on macOS it is `/Volumes/<NAME>` where `<NAME>`
  is the name of the SD-card partition)
* **`otp_release`** is the target Erlang/OTP version used on the GRiSP (defaults to
  `19`)

For a full list of customizable variables, run `rebar3 new help grispapp `.

Install Plug-In Globally
------------------------

To install the plug-in globally, add the plug-in to your plug-ins list in
`~/.config/rebar3/rebar.config`:

```erlang
{plugins, [
    rebar3_hex,
    rebar3_grisp
]}.
```
