# rebar3_grisp

Rebar plug-in for the GRiSP project.

## Installation

### Globally

To install the plug-in globally, add the plug-in to your plug-ins list in
`~/.config/rebar3/rebar.config`:

```erlang
{plugins, [
    rebar3_hex,
    rebar3_grisp
]}.
```

The first time you use Rebar the plug-in will be installed. To upgrade the plug-in to the latest version, you need to first update the Hex index and then the plug-in:

```console
$ rebar3 update
===> Updating package registry...
===> Writing registry to ~/.cache/rebar3/hex/default/registry
===> Generating package index...
===> Writing index to ~/.cache/rebar3/hex/default/packages.idx
$ rebar3 plugins upgrade rebar3_grisp
===> Fetching rebar3_grisp ({pkg,<<"rebar3_grisp">>,<<"1.1.0">>})
===> Downloaded package, caching at ~/.cache/rebar3/hex/default/packages/rebar3_grisp-1.1.0.tar
===> Compiling rebar3_grisp
```

### For an Existing Project

Add the plug-in to your rebar config:

```erlang
{plugins, [rebar3_grisp]}.
```

Then just call your plug-in directly in the root of the existing application:

```console
$ rebar3 grisp
===> Fetching grisp
===> Compiling grisp
<Plugin Output>
```

## Create New Application

Prerequisites:

* [Install Plug-In Globally](#globally)

To create a new GRiSP project:

```
rebar3 new grispapp name=mygrispproject dest=/path/to/SD-card
```

The specific variables provided by this plug-in are:

* **`name`** is the name of the OTP application
* **`dest`** is the destination path for deployment. This should point to where
  your SD-card is mounted (e.g. on macOS it is `/Volumes/<NAME>` where `<NAME>`
  is the name of the SD-card partition)
* **`otp_release`** is the target Erlang/OTP version used on the GRiSP board

For a full list of customizable variables, run `rebar3 new help grispapp`.

## Compile the project

```rebar3 compile```

Make sure you do that with the same Erlang version that you configured in the `rebar.config`. If you compiled rebar3 yourself with a more recent version of Erlang it will give errors, you will need to recompile rebar3 as well in that case.

For further information have a look at the [GRiSP Wiki](https://github.com/grisp/grisp/wiki)

## Deploy an Application

To deploy a GRiSP application, use the command `rebar3 grisp deploy`. The
command requires the release name and version to be provided. The deployment
destination can be set in `rebar.config` or be given as an additional argument.

Example:

```
rebar3 grisp deploy --relname mygrispproject --relvsn 0.7.8
```

or shorter:

```
rebar3 grisp deploy -n mygrispproject -v 0.1.0
```

Above command will try to download a crosscompiled OTP version from our CDN and unpack it. In many usecases this will be enough. If you want to add own port drivers or NIFs in C you will have to build your own toolchain and OTP, see below.

Run `rebar3 help grisp deploy` for information on all arguments.

### Configuration

`rebar.config`:

```erlang
{grisp, [
    {otp, [{version, "22.0"}]},
    {deploy, [
        % Path to put deployed release in
        {destination, "/path/to/destination"},

        % Shell script to run before deploying begins
        {pre_script, "rm -rf /path/to/destination/*"},

        % Shell script to run after deploying has finished
        {post_script, "umount /path/to/destination"}
    ]}
]}.
```

## Listing Packages

The plug-in can list pre-built GRiSP OTP packages and toolchains:

```console
$ rebar3 grisp package list
===> GRiSP pre-built OTP versions for platform 'grisp2'
Version
23.3.4.11
23.3.4.9
$ rebar3 grisp package list --type=toolchain
===> GRiSP pre-built toolchain packages
OS     Latest  OS Version         Url
Linux  true    5.11.0-1027-azure  https://grisp.s3.amazonaws.com/platforms/grisp2/toolchain/grisp2-rtems-toolchain_Linux_5.11.0-1027-azure_e2c29d3374d9046af01af570f6a85a6aa99546bb.tar.gz
Linux          5.11.0-1028-azure  https://grisp.s3.amazonaws.com/platforms/grisp2/toolchain/grisp2-rtems-toolchain_Linux_5.11.0-1028-azure_3122986b9cd7073f42f1387f3981c812a2909b68.tar.gz
macOS  true    10.15.7            https://grisp.s3.amazonaws.com/platforms/grisp2/toolchain/grisp2-rtems-toolchain_macOS_10.15.7_e2c29d3374d9046af01af570f6a85a6aa99546bb.tar.gz
macOS          11.6.3             https://grisp.s3.amazonaws.com/platforms/grisp2/toolchain/grisp2-rtems-toolchain_macOS_11.6.3_3122986b9cd7073f42f1387f3981c812a2909b68.tar.gz
```

## Build OTP for GRiSP

Add the path to the toolchain to the `rebar.config` under `grisp` → `build` → `toolchain` → `directory`:

```erlang
{grisp, [
    {otp, [{version, "22.0"}]},
    {build, [
        {toolchain, [
            {directory,"/PATH/TO/TOOLCHAIN-ROOT"}
        ]}
    ]},
    {deploy, [
        {destination, "/PATH/TO/DESTINATION"}
    ]}
]}.
```

Then execute `rebar3 grisp build`. This will take some time, because Erlang/OTP is cross-compiled for the GRiSP board.

You only need to do that again if you updated and rebuilt the `grisp-software` repository or if you changed or wrote new drivers in C. If you need to build OTP for a second time and just changed files you can speed it up by using `rebar3 grisp build --configure false`. Each time you add new C files you will need to run configure again, because this tool will apply a patch to a makefile for each C driver, NIF and system file.

You can create the tarballs we use for distribution on our CDN with `rebar3 grisp build --tar true`

The built Erlang distribution and its runtime system is located in the project
folder, under the path `_grisp/otp/<version>/install`.

## Development

To test the plug-in and develop for it, we recommend checking out a specific version into a local project. You can also create a new temporary GRiSP project using this plug-in. This can be useful to test deployments locally before copying them to an SD card:

```console
$ rebar3 new grispapp name=grisp_test dest=/tmp/GRISP_SD_CARD
```

Go into the project folder and prepare the checkout directory used by Rebar 3 for dependency overrides:

```console
$ cd grisp_test
$ mkdir -p _checkouts
```

### Testing `master`

You need to clone both _rebar3_grisp_ (this repo) and its dependency [_grisp_tools_](https://github.com/grisp/grisp_tools). If you want the latest `master` versions:

```console
$ git clone git clone https://github.com/grisp/rebar3_grisp.git _checkouts/rebar3_grisp
$ git clone git clone https://github.com/grisp/grisp_tools.git _checkouts/rebar3_grisp
```

### Testing a Specific Branch

Alternatively, clone a specific branch. Replace `$REBAR3_PLUGIN_BRANCH` with the branch name you want from _rebar3_grisp_ and `$GRISP_TOOLS_BRANCH` with the branch name you want from _grisp_tool_:

```console
$ git clone git clone --single-branch --branch $REBAR3_PLUGIN_BRANCH https://github.com/grisp/rebar3_grisp.git _checkouts/rebar3_grisp
$ git clone git clone --single-branch --branch $GRISP_TOOLS_BRANCH https://github.com/grisp/grisp_tools.git _checkouts/rebar3_grisp
```

In case you only need a specific branch of _rebar3_grisp_, you can default to using the `master` version of _grisp_tools_.
