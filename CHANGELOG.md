# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.11.0] - 2026-02-21

### Changed

-  `rebar3 grisp deploy --tar` will not copy the release to the configured destination and will not execute `pre` and `post` scripts, usually defined in the `rebar.config` file.

- the option `--destination` is ignored when using option `--tar`

- Erlang/OTP-26 and older versions are now deprecated. A warning message will be print to inform the user.

### Fixed

- `rebar3 grisp firmware` command failure with latest rebar versions

## [2.10.2] - 2026-01-15

### Fixed

- Fix bug in which an outdated Rebar state was passed to the release handler, this caused failures when using semver in checkouts dependencies

### Changed

- Use semver for versioning the plugin
- Improved CI/CD workflow to test building and deployment on plugin changes [#106](https://github.com/grisp/rebar3_grisp/pull/106)

## [2.10.1] - 2025-07-22

### Fixed

- Restore the rebar3 script configuration to ensure erlexec 2.0.7 for OTP versions below 27 [#105](https://github.com/grisp/rebar3_grisp/pull/105)

## [2.10.0] - 2025-07-08

### Changed

- When using `rebar3 grisp configure` the version of `grisp_connect` is now unlocked [#99](https://github.com/grisp/rebar3_grisp/pull/99)

### Added

- Support for GRiSP Nano [#100](https://github.com/grisp/rebar3_grisp/pull/100)
- New option `update_prebuild` [#100](https://github.com/grisp/rebar3_grisp/pull/100)
- Support GRISP2 projects that build with the [BLAS](https://github.com/erlef/blas) library, GRiSP shell hooks are now injected in the build [#104](https://github.com/grisp/rebar3_grisp/pull/104)

## [2.9.0] - 2025-04-24

### Added

- Add Manifest validation Events [#97](https://github.com/grisp/rebar3_grisp/pull/97)
- Pass the profile list to the deploy command to be included in the manifest [#96](https://github.com/grisp/rebar3_grisp/pull/96)

### Changed

- Update templates of rebar3 grisp configure for grisp.io. [#90](https://github.com/grisp/rebar3_grisp/pull/90)

### Fixed

- Fix CI workflow. [#98](https://github.com/grisp/rebar3_grisp/pull/98)

## [2.8.0] - 2024-10-11

### Fixed

- Fix the firmware and pack commands when a dependency is using semver.

### Changed

- Renamed the grisp firmware command argument --force-bundle/-F and the grisp
  pack command argument --force-bundle/-F to --refresh/-r. [#91](https://github.com/grisp/rebar3_grisp/pull/92)

### Removed

- The utility function rebar3_grisp_util:rebar_command/4 has been removed as it
  runs app_discovery provider again and that causes issues with dependencies
  using semver. Instead rebar3:run/1 is used. [#91](https://github.com/grisp/rebar3_grisp/pull/92)

## [2.7.0] - 2024-09-06

### Added

- New `-t/--tar` option to the deploy command to save a grisp release tarball in
  the `_grisp/deploy` directory.
- New firmware command to generate GRiSP 2 binary firmwares: [#83](https://github.com/grisp/rebar3_grisp/pull/83)
- New pack command to generate GRiSP 2 software update package: [#85](https://github.com/grisp/rebar3_grisp/pull/85)

### Changed

- The deploy destination in rebar.config is now optional if the -t/--tar option
  is specified to the deploy command. To disable a copy deployment when
  configured in rebar.config, an empty destination can be specified on the
  command line: `--destination=""`.

## [2.6.1] - 2024-06-21
### Changed
- Adapation to the new format of the prompt provided by `grisp_tools_configure:settings/0`: [#81](https://github.com/grisp/rebar3_grisp/pull/81)
- User input are now asked using events for `configure` task: [#81](https://github.com/grisp/rebar3_grisp/pull/81)

## [2.6.0] - 2024-05-07

### Added

- New `rebar3 grisp configure` command to create a custom GRiSP application. It replaces the command `rebar3 new grispapp` as a new way to create a custom GRiSP application: [#78](https://github.com/grisp/rebar3_grisp/pull/78)

### Removed

- Removed the *grispapp* template: [#78](https://github.com/grisp/rebar3_grisp/pull/78)

### Fixed

- CI update to only test OTP 24 and 25: [#79](https://github.com/grisp/rebar3_grisp/pull/79)

## [2.5.0] - 2023-04-21

### Added

- New `rebar3 grisp report` command: groups and packs usefull debug information
about the current grisp project
- New rebar.config option `{docker, "grisp/grisp2-rtems-toolchain"}` is available for the grisp toolchain. Allows to use a docker image instead of a local installation.

### Changed

- Set default Erlang version to `"25.2.3"` in grispapp template

## [2.4.0] - 2022-07-18

### Added

- A `--cached` flag to `rebar3 grisp package list` to list only cached
  packages
- Support for OTP 24

### Fixed

- Crash when listing packages
- Download skipped if using OTP package cache

## [2.3.0] - 2022-06-07

### Added

- Exact OTP version selection, e.g. '=23.3'
- Toolchain validation before building

### Changed

- The OTP Git repository is more thoroughly checked for consistency

### Fixed

- Deploy and build tasks now works offline
- Extracted packages are cached globally instead of per project
- Custom builds now correctly uses OTP versions from Git
- `rebar3 package list` now sorts versions correctly

## [2.2.2] - 2022-03-01

### Fixed

- Fix processing of already downloaded OTP packages (through grisp_tools)

## [2.2.1] - 2022-03-01

### Added

- Automatically pick the highest version when only the release name is
  specified during deployment (#64)
- `rebar3 grisp package list` can now list both OTP and toolchain packages. In
  addition, it can display only certain columns.
- OTP Git cloning is now shallow (using `--dethp 1`) which more than halves the
  download time of OTP sources.

### Fixed

- Deploys no longer crash on invalid release names or versions (#68)

## [2.2.0] - 2022-02-16

### Added

- `rebar3 grisp package list` can now take a `--hash` flag that prints all hash versions of a package

### Changed

- Deploying without a custom OTP build will correctly pick a version from
  existing pre-built packages or show a proper error

## [2.1.0] - 2022-02-14

### Added

- `rebar3 grisp package list` task that lists pre-built OTP package versions

### Changed

- `deploy` task no longer requires specifying release name and version if there
  is only one release

### Fixed

- Template rendering no longer crashes when environment variables contain
  unicode (#42)

## [2.0.0] - 2022-02-01

**BREAKING CHANGE:** Require `GRISP_TC_ROOT` for all Erlang version to point
to the tool chain root folder and not the subfolder `rtems/5`. If you have
configured the setting `grisp.build.toolchain.directory` you need to remove
the `rtems/5` postfix if present.

### Changed

- Default platform is now `grisp2`

### Added

- Patch for OTP 23.3.4

### Fixed

- Don't build unused apps
- Enhanced build logging
- Use correct `system_libs` path for rebar3 3.14+

## [1.3.0] - 2020-03-09

### Changed

- Use Erlang 22 by default [\#60](https://github.com/grisp/rebar3_grisp/issues/60)

### Fixed

- Error in build phase [\#55](https://github.com/grisp/rebar3_grisp/issues/55)

## [1.2.6] - 2019-09-27

### Changed

- Update grisp\_tools dependency to 0.2.6 [\#58](https://github.com/grisp/rebar3_grisp/pull/58) ([nextl00p](https://github.com/nextl00p))

## [1.2.5] - 2019-09-27

### Changed

- Remove deprecated maintainers section [\#57](https://github.com/grisp/rebar3_grisp/pull/57) ([nextl00p](https://github.com/nextl00p))

## [1.2.4] - 2019-09-27

### Added

- Add support for OTP 22 [\#47](https://github.com/grisp/rebar3_grisp/pull/47) ([sylane](https://github.com/sylane))
- Add support for NIFs [\#46](https://github.com/grisp/rebar3_grisp/pull/46) ([nextl00p](https://github.com/nextl00p))

### Changed

- Refactor deploy logic into grisp\_tools library [\#39](https://github.com/grisp/rebar3_grisp/issues/39)

### Fixed

- Handle duplicate C source files from different apps [\#1](https://github.com/grisp/rebar3_grisp/issues/1)
- Files in grisp/grisp\_base/\*/\*.c are ignored in user applications [\#45](https://github.com/grisp/rebar3_grisp/issues/45)
- Fix OTP 21.0-rc1 patch [\#56](https://github.com/grisp/rebar3_grisp/pull/56) ([nextl00p](https://github.com/nextl00p))
- Prebuild OTP filename depends on Board now [\#52](https://github.com/grisp/rebar3_grisp/pull/52) ([nextl00p](https://github.com/nextl00p))
- Revert "Prebuild OTP filename depends on Board now" [\#54](https://github.com/grisp/rebar3_grisp/pull/54) ([nextl00p](https://github.com/nextl00p))
- Fix OTP 22 patches after adding NIF support [\#53](https://github.com/grisp/rebar3_grisp/pull/53) ([sylane](https://github.com/sylane))
- Fix patch files to support NIFs for all supported OTP versions [\#50](https://github.com/grisp/rebar3_grisp/pull/50) ([nextl00p](https://github.com/nextl00p))
- Detect GRiSP files in current project as well, add debug output, fix \#45 [\#48](https://github.com/grisp/rebar3_grisp/pull/48) ([nextl00p](https://github.com/nextl00p))
- Fix failing deploy [\#44](https://github.com/grisp/rebar3_grisp/pull/44) ([nextl00p](https://github.com/nextl00p))
- Already Patched OTP is not detected anymore [\#49](https://github.com/grisp/rebar3_grisp/issues/49)
- Deploy fails [\#43](https://github.com/grisp/rebar3_grisp/issues/43)

## [1.2.3] - 2018-08-07

### Added

- Print a proper error for missing template keys [\#37](https://github.com/grisp/rebar3_grisp/issues/37)

### Fixed

- Default GRiSP sources overwrite custom sources [\#38](https://github.com/grisp/rebar3_grisp/issues/38)

## [1.2.2] - 2018-08-01

### Fixed

- Templates do not work when deploying [\#36](https://github.com/grisp/rebar3_grisp/issues/36)

## [1.2.1] - 2018-08-01

### Fixed

- Templates do not work when building [\#35](https://github.com/grisp/rebar3_grisp/issues/35)

## [1.2.0] - 2018-07-31

### Added

- Add version task that prints version number [\#33](https://github.com/grisp/rebar3_grisp/pull/33) ([eproxus](https://github.com/eproxus))
- Include environment variables in templates [\#32](https://github.com/grisp/rebar3_grisp/pull/32) ([eproxus](https://github.com/eproxus))
- Re-introduce GRISP\_TOOLCHAIN environment variable [\#34](https://github.com/grisp/rebar3_grisp/pull/34) ([eproxus](https://github.com/eproxus))

## [1.1.5] - 2018-06-25

### Fixed

- Weird directory with name " created during deploy [\#30](https://github.com/grisp/rebar3_grisp/issues/30)

## [1.1.4] - 2018-06-21

### Changed

* Use OTP 21.0 by default for new apps ([c4274c1a](https://github.com/grisp/rebar3_grisp/commit/c4274c1a328b83ba9b3a4b16cf5cd77ce60b4110 "Use 21.0 by default for new apps"))

[Full Changelog][1.1.4]

## [1.1.3] - 2018-06-21

### Added

* Support OTP 21.0 ([9975e51d](https://github.com/grisp/rebar3_grisp/commit/9975e51d17c8f230f9edbd7bc5d843a453a3d9c0 "Add patch for OTP 21.0"))

## [1.1.2] - 2018-06-06

### Added

- deploy: added support for whitespaces in source and deployment directories [\#28](https://github.com/grisp/rebar3_grisp/pull/28) ([lwehmeier](https://github.com/lwehmeier))
- Add support for OTP 21.0-rc1 [\#25](https://github.com/grisp/rebar3_grisp/pull/25) ([sylane](https://github.com/sylane))

### Changed

- Remove references to old versions [\#27](https://github.com/grisp/rebar3_grisp/pull/27) ([nextl00p](https://github.com/nextl00p))

### Fixed

- consistency of the instructions for release name [\#26](https://github.com/grisp/rebar3_grisp/pull/26) ([CrowdHailer](https://github.com/CrowdHailer))

## [1.1.1] - 2018-05-25

### Fixed

- Freshly generated grispapp fails to deploy the first time its run [\#23](https://github.com/grisp/rebar3_grisp/issues/23)

## [1.1.0] - 2018-05-24

### Added

- Prebuilt toolchain [\#20](https://github.com/grisp/rebar3_grisp/pull/20) ([nextl00p](https://github.com/nextl00p))
- Add version to OTP xcomp files [\#18](https://github.com/grisp/rebar3_grisp/pull/18) ([sylane](https://github.com/sylane))
- Make deploy destination available via command-line flag [\#5](https://github.com/grisp/rebar3_grisp/issues/5)
- Start Erlang runtime when source dependencies are included [\#21](https://github.com/grisp/rebar3_grisp/issues/21)

### Changed

- Refactor build configuration [\#16](https://github.com/grisp/rebar3_grisp/pull/16) ([sylane](https://github.com/sylane))

### Fixed

- Confusing error message, when no toolchain root is set [\#4](https://github.com/grisp/rebar3_grisp/issues/4)
- Allow deploy without grisp runtime and fix version warning [\#17](https://github.com/grisp/rebar3_grisp/pull/17) ([sylane](https://github.com/sylane))
- Wrong warning about Erlang version mismatch [\#14](https://github.com/grisp/rebar3_grisp/issues/14)

## [1.0.1] - 2017-12-20

### Changed

* Update to Erlang 20.2 by default for new projects ([d8b48ad3](https://github.com/grisp/rebar3_grisp/commit/d8b48ad354f04ff67bda928c955399be29580ce3))

## [1.0.0] - 2017-12-19

### Added

- Document rebar3 grisp build [\#3](https://github.com/grisp/rebar3_grisp/issues/3)
- Make OTP version branch customizable [\#12](https://github.com/grisp/rebar3_grisp/pull/12) ([eproxus](https://github.com/eproxus))
- Make SMP the default and add support for OTP 20.2 [\#13](https://github.com/grisp/rebar3_grisp/pull/13) ([sylane](https://github.com/sylane))

### Fixed

- With default profile section ERTS does not get included [\#8](https://github.com/grisp/rebar3_grisp/issues/8)

## [0.1.1] - 2017-12-05

### Added

* GRiSP application template
* Deploy command
* Ensure install directory before installing
* Option to turn off running configure
* Pre- and post-script options to deploy
* Copy all files from project file section


### Changed

* Install OTP locally after build

Modify release configuration dynamically

### Fixed

* Avoid patching OTP if already patched
* Fix bug finding grisp application

## [0.1.0] - 2017-10-20

### Added

* Initial release.

[unreleased]: https://github.com/grisp/rebar3_grisp/compare/2.11.0...HEAD
[2.11.0]: https://github.com/grisp/rebar3_grisp/compare/2.10.2...2.11.0
[2.10.2]: https://github.com/grisp/rebar3_grisp/compare/2.10.1...2.10.2
[2.10.1]: https://github.com/grisp/rebar3_grisp/compare/2.10.0...2.10.1
[2.10.0]: https://github.com/grisp/rebar3_grisp/compare/2.9.0...2.10.0
[2.9.0]: https://github.com/grisp/rebar3_grisp/compare/2.8.0...2.9.0
[2.8.0]: https://github.com/grisp/rebar3_grisp/compare/2.7.0...2.8.0
[2.7.0]: https://github.com/grisp/rebar3_grisp/compare/2.6.1...2.7.0
[2.6.1]: https://github.com/grisp/rebar3_grisp/compare/2.6.0...2.6.1
[2.6.0]: https://github.com/grisp/rebar3_grisp/compare/2.5.0...2.6.0
[2.5.0]: https://github.com/grisp/rebar3_grisp/compare/2.4.0...2.5.0
[2.4.0]: https://github.com/grisp/rebar3_grisp/compare/2.3.0...2.4.0
[2.3.0]: https://github.com/grisp/rebar3_grisp/compare/2.2.2...2.3.0
[2.2.2]: https://github.com/grisp/rebar3_grisp/compare/2.2.1...2.2.2
[2.2.1]: https://github.com/grisp/rebar3_grisp/compare/2.2.0...2.2.1
[2.2.0]: https://github.com/grisp/rebar3_grisp/compare/2.1.0...2.2.0
[2.1.0]: https://github.com/grisp/rebar3_grisp/compare/2.0.0...2.1.0
[2.0.0]: https://github.com/grisp/rebar3_grisp/compare/1.3.0...2.0.0
[1.3.0]: https://github.com/grisp/rebar3_grisp/compare/1.2.6...1.3.0
[1.2.6]: https://github.com/grisp/rebar3_grisp/compare/1.2.5...1.2.6
[1.2.5]: https://github.com/grisp/rebar3_grisp/compare/1.2.4...1.2.5
[1.2.4]: https://github.com/grisp/rebar3_grisp/compare/1.2.3...1.2.4
[1.2.3]: https://github.com/grisp/rebar3_grisp/compare/1.2.2...1.2.3
[1.2.2]: https://github.com/grisp/rebar3_grisp/compare/1.2.1...1.2.2
[1.2.1]: https://github.com/grisp/rebar3_grisp/compare/1.2.0...1.2.1
[1.2.0]: https://github.com/grisp/rebar3_grisp/compare/1.1.5...1.2.0
[1.1.5]: https://github.com/grisp/rebar3_grisp/compare/1.1.4...1.1.5
[1.1.4]: https://github.com/grisp/rebar3_grisp/compare/1.1.3...1.1.4
[1.1.3]: https://github.com/grisp/rebar3_grisp/compare/1.1.2...1.1.3
[1.1.2]: https://github.com/grisp/rebar3_grisp/compare/1.1.1...1.1.2
[1.1.1]: https://github.com/grisp/rebar3_grisp/compare/1.1.0...1.1.1
[1.1.0]: https://github.com/grisp/rebar3_grisp/compare/1.0.1...1.1.0
[1.0.1]: https://github.com/grisp/rebar3_grisp/compare/1.0.0...1.0.1
[1.0.0]: https://github.com/grisp/rebar3_grisp/compare/0.1.1...1.0.0
[0.1.1]: https://github.com/grisp/rebar3_grisp/compare/0.1.0...0.1.1
[0.1.0]: https://github.com/grisp/rebar3_grisp/compare/5a76a33028c2ee90ee0ebae52bb4dda94c991594...0.1.0
