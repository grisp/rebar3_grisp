# rebar3_grisp

Rebar plug-in for the GRiSP project. To obtain information about the plugin and
its tasks, use the following command:

```sh
rebar3 help grisp [<task>]
```

📖 **Table of content**
- [rebar3\_grisp](#rebar3_grisp)
  - [Installation](#installation)
    - [Globally](#globally)
    - [For an Existing Project](#for-an-existing-project)
  - [Create New Application](#create-new-application)
  - [Compile the project](#compile-the-project)
  - [Deploy an Application](#deploy-an-application)
    - [Configuration](#configuration)
  - [Listing Packages](#listing-packages)
  - [Build OTP for GRiSP](#build-otp-for-grisp)
  - [Bug reports](#bug-reports)
  - [Development](#development)
    - [Testing `master`](#testing-master)
    - [Testing a Specific Branch](#testing-a-specific-branch)

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
rebar3 grisp configure
```

This command will provide you with a CLI that will guide you in the creation of your GRiSP project.

You can also use the command in a non-interactive way:

```
rebar3 grisp configure -i false
```

Unless stated otherwise, the non-interactive option will use the default values to create the GRiSP project. You can overwrite the default values in the command:
```
rebar3 grisp configure -i false --name="my_grisp_app" -n true -w true --ssid="mywifi" --psk="wifipsk"
```
This command will create a new GRiSP project named "my_grisp_app" with a network (`-n true`) and wifi (`-w true`) configuration already setup. The configuration will use the ssid "mywifi" and the psk "wifipsk".



Note that some options require others. For example, if you want to setup the ssid of the wifi, then you also need to activate the network and wifi configuration (`-n true` and `-w true`).

The specific variables provided by this plug-in are:

* **`interactive`** activate the interactive mode
* **`name`** is the name of the OTP application
* **`dest`** is the destination path for deployment. This should point to where
  your SD-card is mounted (e.g. on macOS it is `/Volumes/<NAME>` where `<NAME>`
  is the name of the SD-card partition)
* **`otp_version`** is the target Erlang/OTP version used on the GRiSP board
* **`network`** specifies if the project contains network configuration files
* **`wifi`** specifies if the project contains wifi configuration files. (requires `network`)
* **`ssid`** is the ssid of the wifi network you want your board to connect to. (requires `network` and `wifi`) 
* **`psk`** is the psk of the wifi network you want your board to connect to. (requires `network` and `wifi`) 
* **`grisp_io`** specifies if you want your board to connect and use GRiSP.io. (requires `network`) 
* **`grisp_io_linking`** specifies if you want your board to link itself to GRiSP.io. (requires `network` and `grisp_io`)
* **`token`** is your personnal GRiSP.io token. (requires `network`, `grip_io` and `grisp_io_linking`) 
* **`epmd`** specifies if you want your board to have epmd. (requires `network`) 
* **`cookie`** is the magic cookie that your board should use. (requires `network` and `epmd`) 

Some variables are modfiable only through the command line. These variables are:
* **`desc`** is the short description of the GRiSP application
* **`copyright_year`** is the copyright year
* **`author_name`** is the name of the author of the project
* **`author_email`** is the email of the author of the project 

For a full list of customizable variables as well as their short form, run `rebar3 help grisp configure`.

In the interactive mode you can also specify a few variables. During the CLI interaction, the questions related to these variable will be skipped. For example:

```
rebar3 grisp configure --name="mygrispapp"
```
Here the CLI won't ask you for the name of your GRiSP project because it's already provided.

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

To generate a tarball with all the deployed files, add the option `-t/--tar`,
all the files will be bundled in a a tarball under `_grisp/deploy`:

```
rebar3 grisp deploy --tar
```


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


## Generate GRiSP 2 Firmwares

The `firmware` command generates binary files that can be written on GRiSP 2
eMMC. There is three types of firmware that can be generated:

 - **System Firmware**:
   The system firmware is the content of a system partition on the eMMC.
   When using A/B software update, the system firmware can be written either
   on the first or the second system partition. By default, the command will
   generate a system firmware under `_grisp/firmware` but it can be disabled with
   the option `-b false` or `--system false`.
 - **eMMC Image Firmware**:
   The eMMC image firmware is a full image containing the bootloader, the
   partition table and the system partitions. It is meant to be written on the
   GRiSP 2 board to reset it completely with the new software. If the image is
   truncated (it is by default), the image only contains the first system
   partition. It means that when writing the firmware to the eMMC, the second
   system partition will be untouched. To generate an eMMC image firmware under
   `_grisp/firmware`, add the option `-i` or `--image`, to disable truncating
   so the image contains both system partitions, uses the option `-t false` or
   `--truncate false`.
 - **Bootloader Firmware**:
   The bootloader firmware contains only the bootloader and the partition table.
   To generate it under `_grisp/firmware`, add the option `-b` or `--bootloader`.

e.g.

Generate a system firmware for the default release:

    rebar3 grisp firmware

Generate a system firmware for a specific release:

    rebar3 grisp firmware --relname myapp --relvsn 1.2.3

Generate all firmwares, forcing existing files to be overwritten and forcing the
generation of the software bundle even if one already exists in `_grisp/deploy`:

    rebar3 grisp firmware --bootloader --image --force --refresh


### Firmware Update

Description of the variables in the commands that will follow:
 - **`${RELNAME}`**: The relx release names used when generating the firmware.
 - **`${RELVSN}`**: The relx release version used when generating the firmware.
 - **`${USER}`**: The username of the account running the command.
 - **`${GRISP_BOARD_SERIAL}`**: The serial number of the GRiSP 2 board.

To write a system firmware to a GRiSP 2 board:

 - Copy the firmware to the SD card:

    **`macOS`** `$ cp _grisp/firmware/grisp2.${RELNAME}.${RELVSN}.sys.gz /Volumes/GRISP`

    **`Linux`** `$ cp _grisp/firmware/grisp2.${RELNAME}.${RELVSN}.sys.gz /media/${USER}/GRISP`

 - Unmount the SD card:

    **`macOS`** `$ diskutil umount /Volumes/GRISP`

    **`Linux`** `$ umount /media/${USER}/GRISP`

 - Open a serial console to the GRiSP board:

    **`macOS`** `$ screen /dev/tty.usbserial-0${GRISP_BOARD_SERIAL}1 115200`

    **`Linux`** `$ screen /dev/ttyUSB1 115200`

 - Insert the SD card in the GRiSP 2 board.
 - Reset the board using the onboard reset button.
 - Enter into barebox console mode by pressing any key before 3 seconds.
 - Consult the current active system partition:

    **`Barebox`** `$ echo $state.bootstate.active_system`

 - Write the firmware. If the current active system is `0`, use device
   `/dev/mmc1.0`, if it is `1` use device `/dev/mmc1.1`:

    **`Barebox`** `$ uncompress /mnt/mmc/grisp2.${RELNAME}.${RELVSN}.sys.gz /dev/mmc1.0`

 - Remove the SD card.
 - Reset the GRiSP board again.

To reset a GRiSP 2 board eMMC, either with a truncated or full image firmware:

 - Copy the firmware to the SD card:

    **`macOS`** `$ cp _grisp/firmware/grisp2.${RELNAME}.${RELVSN}.emmc.gz /Volumes/GRISP`

    **`Linux`** `$ cp _grisp/firmware/grisp2.${RELNAME}.${RELVSN}.emmc.gz /media/${USER}/GRISP`

 - Unmount the SD card:

    **`macOS`** `$ diskutil umount /Volumes/GRISP`

    **`Linux`** `$ umount /media/${USER}/GRISP`

 - Open a serial console to the GRiSP board:

    **`macOS`** `$ screen /dev/tty.usbserial-0${GRISP_BOARD_SERIAL}1 115200`

    **`Linux`** `$ screen /dev/ttyUSB1 115200`

 - Insert the SD card in the GRiSP 2 board.
 - Reset the board using the onboard reset button.
 - Enter into barebox console mode by pressing any key before 3 seconds.
 - Set the current active system partition to the first one:

    **`Barebox`** `$ let state.bootstate.active_system=0`

    **`Barebox`** `$ state -s`

 - Write the firmware:

    **`Barebox`** `$ uncompress /mnt/mmc/grisp2.${RELNAME}.${RELVSN}.emmc.gz /dev/mmc1`

 - Remove the SD card.
 - Reset the GRiSP board again.

To reset only the bootloader of the board:

 - Copy the firmware to the SD card:

    **`macOS`** `$ cp _grisp/firmware/grisp2.${RELNAME}.${RELVSN}.boot.gz /Volumes/GRISP`

    **`Linux`** `$ cp _grisp/firmware/grisp2.${RELNAME}.${RELVSN}.boot.gz /media/${USER}/GRISP`

 - Unmount the SD card:

    **`macOS`** `$ diskutil umount /Volumes/GRISP`

    **`Linux`** `$ umount /media/${USER}/GRISP`

 - Open a serial console to the GRiSP board:

    **`macOS`** `$ screen /dev/tty.usbserial-0${GRISP_BOARD_SERIAL}1 115200`

    **`Linux`** `$ screen /dev/ttyUSB1 115200`

 - Insert the SD card in the GRiSP 2 board.
 - Reset the board using the onboard reset button.
 - Enter into barebox console mode by pressing any key before 3 seconds.
 - Write the firmware:

    **`Barebox`** `$ uncompress /mnt/mmc/grisp2.${RELNAME}.${RELVSN}.boot.gz /dev/mmc1`

 - Remove the SD card.
 - Reset the GRiSP board again.


### Cautions

#### With truncated image firmwares

When writing a truncated eMMC image firmware, only the first system partition is
written. If the active system is the second one, the board will continue to boot
the old software. You will need to manually change the active system partition
in the bootloader console and restart the board.

To consule the current active system partition in the bootloader console:

    $ echo $state.bootstate.active_system

To change the current active system partition to the first one:

    $ let state.bootstate.active_system=0
    $ state -s


#### With writing system firmware on inactive system partition

When writing a system firmware, be sure to do it on the active system
partition or the board will continue to boot the old software.
The device for the first system is `/dev/mmc1.0` and the one for the second
system is `/dev/mmc1.1`. See [the caution about truncated images firmware](#with-truncated-image-firmwares)
for details on how to consult and change the current active system partition.


## Build Software Update Package

To create a GRiSP software update package, use the 'pack' command:

    $ rebar3 grisp pack

It creates a software update package under `_grisp/update`.

To include the bootloader in the generate update package, add the option
`-b/--with-bootloader`:

    $ rebar3 grisp pack -b

Note that a toolchain is required for building the bootloader firmware, see the
`deploy` command for more information on how to configure the toolchain.

To force the recreation of the bundle and firmware(s) use the option
`-r/--refresh`:

    $ rebar3 grisp pack -r

To generate a signed package, use the `-k/--key` option:

    $ rebar3 grisp pack --key private_key.pem


### Updating a GRiSP Board

To be able to update a GRiSP board using a software update package, the software
running on the board must have the `grisp_updater_grisp2` dependency in
`rebar.config`:

    {deps, [grisp_updater_grisp2]}.

`grisp_updater` needs to be configured in `sys.config` (or equivalent):

    {grisp_updater, [
        {signature_check, true},
        {signature_certificates, {priv, my_app, "certificates/updates"}},
        {system, {grisp_updater_grisp2, #{}}},
        {sources, [
            {grisp_updater_tarball, #{}},
            {grisp_updater_http, #{
                backend => {grisp_updater_grisp2, #{}}
            }}
        ]}
    ]},

If `signature_check` is set to `true` the software package must be signed using
the `-k/--key` option, and the public key must be available in the directory
configured by `signature_certificates`.

When these conditions are met, you can follow these step to perform a A/B
software update of a GRiSP board:

 - Unpack the software update package in some local directory:

    **`any`** `$ mkdir -p releases/${RELNAME}/${RELVSN}`
    **`any`** `$ tar -C releases/${RELNAME}/${RELVSN} -xvf _grisp/update/grisp2.${REL_NAME}".${RELVSN}.${PROFILE}.tar -xvf`

 - Start a local HTTP server to serve the package:

    **`any`** `$ http-server ./releases -p 8000`

 - Open a serial console to the GRiSP board:

    **`macOS`** `$ screen /dev/tty.usbserial-0${GRISP_BOARD_SERIAL}1 115200`

    **`Linux`** `$ screen /dev/ttyUSB1 115200`

- On the GRiSP2 console, start the update process:

    **`GRiSP`** `$ grisp_updater:update(<<"http://${HOST_IP}:8000/${RELNAME}/${RELVSN}">>).`

- Reset the GRiSP2 board using the onboard reset button.

- Validate the new software version on the GRiSP2 console:

    **`GRiSP`** `$ grisp_updater:validate().`

Note that the update process will only show progress information if the log
level is at least `INFO`.


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

The fastest way is to use our docker image `grisp/grisp2-rtems-toolchain`:

Add the toolchain image name to the `rebar.config` under `grisp` → `build` → `toolchain` → `docker`

Or if you have a local installation you can use that:

Add the path to the toolchain to the `rebar.config` under `grisp` → `build` → `toolchain` → `directory`

```erlang
{grisp, [
    {otp, [{version, "25"}]},
    {build, [
        {toolchain, [
            {directory, "/PATH/TO/TOOLCHAIN-ROOT"}
            % Or use docker
            {docker, "grisp/grisp2-rtems-toolchain"}
            % If both are specified, only 'directory' is used
        ]}
    ]},
    {deploy, [
        {destination, "/PATH/TO/DESTINATION"}
    ]}
]}.
```

Then execute `rebar3 grisp build`. This will take some time, because Erlang/OTP is cross-compiled for the GRiSP board.

You only need to do that again if you updated and rebuilt the `grisp2-rtems-toolchain` repository or if you changed or wrote new drivers in C. If you need to build OTP for a second time and just changed files you can speed it up by using `rebar3 grisp build --configure false`. Each time you add new C files you will need to run configure again, because this tool will apply a patch to a makefile for each C driver, NIF and system file.

You can create the tarballs we use for distribution on our CDN with `rebar3 grisp build --tar true`

The built Erlang distribution and its runtime system is located in the project
folder, under the path `_grisp/otp/<version>/install`.

## Bug reports

You can run `rebar3 grisp report` to gather info about the project configuration. The user can view and edit the generated text files. It's possible to pack them later adding `--tar` to the same command. Providing such report file can speedup debugging and support from the dev team.

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
