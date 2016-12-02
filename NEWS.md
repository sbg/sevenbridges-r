# sevenbridges 1.5.4

## Improvements

- Redesigned authentication process; compliant with the new specification for configuration file format
- Experimental support for Folders API (advanced access)
- Updated error status code to the latest version
- Decoupled Files class and (CWL) File class; updated Files class to support the latest API

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
