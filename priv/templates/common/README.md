{{{name}}}
=====

{{{desc}}}

Build
-----

    $ rebar3 compile
{{#rust}}

The Rust NIF example is `{{{name}}}_rust_nif:hello/0`. Target builds require a
Rust-enabled GRiSP2 toolchain and the compatible Cargo plugin configured in
`rebar.config`.

If you use the Docker toolchain and already have an older image locally, update
it before building:

    $ docker pull grisp/grisp2-rtems-toolchain:latest
{{/rust}}
{{#c_nif}}

The C NIF example is `{{{name}}}_c_nif:hello/0`; its source is in the GRiSP
build overlay. It is compiled into the target runtime by `grisp build`.
{{/c_nif}}
{{#native}}

Native NIFs require a custom runtime. Select the toolchain in `rebar.config`
and build before deploying:

    $ rebar3 grisp build
{{/native}}

Deploy
------

    $ rebar3 grisp deploy -n <name> -v <version>
{{#grisp_io}}

GRiSP.io
--------

### Link a device

If you did not configure a linking token, copy one from
[GRiSP.io Devices](https://app.grisp.io/grisp-manager/devices) and run this
once in the device shell:

    grisp_connect:link_device(<<"TOKEN">>).

### Publish an update

For projects with NIFs, build the custom OTP first:

    rebar3 grisp build

Build a local update package:

    rebar3 grisp pack

Authenticate before using the GRiSP.io commands for the first time. Your
credentials are saved for later commands; then upload the package:

    rebar3 grisp-io auth
    rebar3 grisp-io upload

Upload builds the package if it does not exist locally. Use `--refresh` to
rebuild an existing package or `--force` to replace an uploaded package.

### Deploy an update

Use the GRiSP.io web interface to start an update from the device's
**Device Details** page. It shows progress and lets you reboot and validate.
You can also start an update from the command line:

    rebar3 grisp-io deploy --device SERIAL

If the update requests a reboot, reboot from the web interface or command line.
Validate after the device reconnects:

    rebar3 grisp-io reboot --device SERIAL
    rebar3 grisp-io validate --device SERIAL

> **Warning:** Updates install to internal eMMC, not the SD card. If the board
> booted from an SD card, eject it before rebooting after the update. Otherwise
> the board will start the old system from the SD card. If validation reports
> `Error: device needs to be rebooted`, reboot and reconnect the device, then
> validate again. An update that is not validated can roll back on the next
> reboot.

### Manage packages

Manage uploaded packages in the web interface, or use these commands. `PACKAGE`
is the filename shown by `list`, not the local package file name:

    rebar3 grisp-io list
    rebar3 grisp-io delete PACKAGE
{{/grisp_io}}
