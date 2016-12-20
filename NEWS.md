# sevenbridges 1.5.5 (TBD)

## Improvements

- Support three authentication methods: direct authentication, via environment variables, or via user configuration file. Compliant with the new API client authentication specification.
- Updated platform types: users can now choose from `cgc`, `aws-us`, `aws-eu`, `gcp`, and `cavatica`.
- File ID support on task input (directly) ([#27](https://github.com/sbg/sevenbridges-r/issues/27))
- Removed credential information parsing when package is loaded.
- Improved robustness of `convert_app` when a CWL JSON has more fields than defined ([#44](https://github.com/sbg/sevenbridges-r/issues/44))
- Migrate Docker images to Seven Bridges Docker Hub account ([#43](https://github.com/sbg/sevenbridges-r/issues/43))
- New argument `keep_test` for `task_add()` added: when users push a Tool object with no `sbg:job` information, can still use `keep_test = TRUE` to keep previous test information. ([#31](https://github.com/sbg/sevenbridges-r/issues/31))
- Support manifest file upload with fitler and subsetting for uploader ([#46](https://github.com/sbg/sevenbridges-r/issues/46))
- New vignette style ([#38](https://github.com/sbg/sevenbridges-r/issues/38))

# sevenbridges 1.5.4

## Bug Fixes

- Big fix for batch mode: ([b6a91f2](https://github.com/sbg/sevenbridges-r/commit/b6a91f2fbbb59cbe6dc40b63f6b7057064e25c19))
- Bug fix in IDE Dockerfile ([838856b](https://github.com/sbg/sevenbridges-r/commit/838856bd1dfb4ea8bc143f246a152deda3c76d92))

## Improvements

- Decoupled Files class and (CWL) File class; updated Files class to support the latest API
- Updated error status code to the latest version

# sevenbridges 1.5.3

## Bug Fixes

- Fixed build error under Windows caused by unexpected UTF-8 characters ([b03ed45](https://github.com/sbg/sevenbridges-r/commit/b03ed45d5c9495196df311b58a2e275b3f2ea44a))
- Removed BiocStyle to avoid naming scheme conflicts
- Added essential dependencies, such as `miniUI`, `shiny`, and `rstudioapi` to be ready for RStudio addins
- Fixed batch mode input checking. Thanks: Fabian Zimmer

## Improvements

- New website for function references and vignettes: https://sbg.github.io/sevenbridges-r/
- Added continuous integration (with new badge) for Windows
- Structural and style improvements for `README.md`
- General R code style improvements

# sevenbridges 1.1.16

## New Features

- Full support for API V2, user-friendly call from R
- CWL Draft 2+ generator in R, create JSON/YAML tool directly
- 5 Vignettes added for comprehensive tutorials and reference
- Three examples inlcuded under inst/docker for cwl app examples
- Auth configuration file to maintain multiple platforms and user account
- Works for multiple Seven Bridges supported platforms
- More features like task hook function to ease the automation

# sevenbridges 1.0.0

## New Features

- Initial version
- All the APIs of the SBG platform are supported
- First vignette added
