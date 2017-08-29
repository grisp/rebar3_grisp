# rebar3_grisp

Rebar plug-in for the GRiSP project.

## Use

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

## Create New Application

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
* **`otp_release`** is the target Erlang/OTP version used on the GRiSP (defaults
  to `19`)

For a full list of customizable variables, run `rebar3 new help grispapp `.

## Deploy an Application

To deploy a GRiSP application, use the command `rebar3 grisp deploy`. The
command requires the release name and version to be provided. The deployment
destination can be set in `rebar.config` or be given as an additional argument.

Example:

```
rebar3 grisp deploy --relname my_release --relvsn 0.7.8
```

Run `rebar3 help grisp deploy` for information on all arguments.

### Configuration

`rebar.config`:

```erlang
{grisp, [
    {deploy, [
        % Path to put deployed release in
        {destination, "/path/to/destination"},

        % Shell script to run before deploying begins
        {pre_script, "rm -rf /path/to/destination/*"},

        % Shell script to run after deploying has finished
        {post_script, "unmount /path/to/destination"}
    ]}
]}.
```

## Install Plug-In Globally

To install the plug-in globally, add the plug-in to your plug-ins list in
`~/.config/rebar3/rebar.config`:

```erlang
{plugins, [
    rebar3_hex,
    rebar3_grisp
]}.
```
