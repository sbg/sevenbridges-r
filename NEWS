# sevenbridges 1.19.2

## Improvements

- Transfer package maintainership.

# sevenbridges 1.19.1

## Bug Fixes

- Fixed an API issue on invalid JSON when running tasks.
- Fixed a subtle column class mismatch issue when binding data frames since R 4.0.0.

## Improvements

- Use `data.table::rbindlist()` when possible to increase the data frame binding performance.

# sevenbridges 1.17.1

## Bug Fixes

- Fix build issues in R 4.0.0, in which the the default value for `stringsAsFactors` is changed to `FALSE`. Updated the affected `data.frame()` calls which rely on the default automatic vector expansion behavior with explicit `stringsAsFactors = TRUE`.

# sevenbridges 1.15.2

## Improvements

- Added a new argument `authorization` in `Auth` and `api()` to allow specifying the `token` as the access token from Seven Bridges single sign-on (SSO).

# sevenbridges 1.15.1

## Improvements

- Added new fields `created_by`, `created_on`, and `modified_on` to the `Project` class following the recent API improvements. This enables better project filtering when querying projects. See the [vignette](https://sbg.github.io/sevenbridges-r/articles/api.html#filter-by-project-creation-date-modification-date-and-creator) for details.

# sevenbridges 1.13.5

## Improvements

- Added new platform options for the recently introduced environments. Now we can choose from `"aws-us"`, `"aws-eu"`, `"ali-cn"`, `"cgc"`, `"cavatica"`, and `"f4c"` in `Auth()` calls.

# sevenbridges 1.13.4

## New Features

- Added support for Markers API (Advance Access feature). See the [Markers API section in the vignette](https://sbg.github.io/sevenbridges-r/articles/api.html#markers) for details.
- Added support for Actions API. See the [Actions API section in the vignette](https://sbg.github.io/sevenbridges-r/articles/api.html#actions) for details.

## Improvements

- Added a new field `description` to the `Files` class following the recent API improvements.
- Updated the API [vignette](https://sbg.github.io/sevenbridges-r/articles/api.html#run-tasks-using-spot-instances) to reflect the platform default setting update for spot instances (spot is now enabled by default).

# sevenbridges 1.13.3

## New Features

- Added support for Enterprise API. See the [Enterprise API section in the vignette](https://sbg.github.io/sevenbridges-r/articles/api.html#enterprise) for details.

## Improvements

- Removed unnecessary package dependencies to optimize the time needed for package installation and loading.
- New look for the [documentation website](https://sbg.github.io/sevenbridges-r/) with improved text readability.

# sevenbridges 1.13.2

## New Features

- Added support for Folder API. See the [Folders API section in the vignette](https://sbg.github.io/sevenbridges-r/articles/api.html#folders) for details.

## Improvements

- Added floating TOC and changed the vignette theme for the HTML vignettes available on Bioconductor, to improve the browsing experience for long vignettes.

## Bug Fixes

- Fixed Docker image build issue and updated Bunny version.

# sevenbridges 1.13.1

## New Features

- Added support for setting [execution hints per task run](https://sbg.github.io/sevenbridges-r/articles/api.html#execution-hints-per-task-run) when drafting new tasks.

# sevenbridges 1.11.5

## Bug Fixes

- Fixed issues related to hints when creating CWL tools ([#65](https://github.com/sbg/sevenbridges-r/issues/65)) in R 3.5.x.

# sevenbridges 1.11.4

## Bug Fixes

- Fixed several compatibility issues for CWL app conversion caused by newly introduced fields.

# sevenbridges 1.11.3

## Improvements

- Added better support for the spot instance feature, by introducing the new argument `use_interruptible` for `project_new()` and `use_interruptible_instances` for `task_add()`. This will allow users to enable/disable the spot instance feature on both the project level and individual task level. See the new section ["Run tasks using spot instances"](https://sbg.github.io/sevenbridges-r/articles/api.html#run-tasks-using-spot-instances) in the API vignette for details.

# sevenbridges 1.11.2

## Improvements

- Further remove all words related to the previous installation method per Bioconductor's request.

# sevenbridges 1.11.1

## Improvements

- Use the new Bioconductor package manager `BiocManager` for installation instructions.

# sevenbridges 1.9.1

## Bug Fixes

- Removed functions that prevents Shiny apps to load when the package is loaded ([#63](https://github.com/sbg/sevenbridges-r/issues/63)).

# sevenbridges 1.7.5

## Improvements

- Update `rabix-cli` (Bunny) to 1.0.2 for the Docker container.

# sevenbridges 1.7.4

## Bug Fixes

- Fixed the `baseCommand` conversion issue ([#59](https://github.com/sbg/sevenbridges-r/issues/59)).

# sevenbridges 1.7.3

## Improvements

- Use system font stack instead of Google Fonts in vignettes to avoid pandoc SSL issue.

# sevenbridges 1.7.2

## Bug Fixes

- Fixed task creation issues by introducting the new field `use_interruptible_instances` which supports the latest [spot instance feature](https://www.sevenbridges.com/spot-instances-cost-reduction/) ([ea14d5c](https://github.com/sbg/sevenbridges-r/commit/ea14d5c6333b999e754ebb760fc93f89ecdf6019)).

# sevenbridges 1.7.1

## New Features

- Added a new argument (global option) `advance_access` for enabling the access to Advance Access features.

# sevenbridges 1.5.9

## New Features

### Seven Bridges Command Line Uploader Interface

With this update, users are able to download the Seven Bridges command line uploader (Java-based) for the corresponding platform, and control the command line uploader within R directly. This offers another option for uploading (large) files in addition to API file uploading. Related changes are:

- New function `get_uploader()` for downloading Seven Bridges command line uploader for specific platforms. The old function `misc_get_uploader()` is deprecated.
- New functions `cli_upload()`, `cli_list_projects()`, `cli_list_tags()` as the R interface for Seven Bridges command line uploader. The old function `misc_upload_cli()` is deprecated.

For detailed usage of these functions, please check [this section](https://sbg.github.io/sevenbridges-r/articles/api.html#upload-files-via-command-line-uploader) in the vignette.

## Bug Fixes

- Fixed task output files download issue ([#52](https://github.com/sbg/sevenbridges-r/issues/52)).
- Fixed IDE Docker image build issues; removed the libssl-dev dependency; updated Dockerfile ([#50](https://github.com/sbg/sevenbridges-r/issues/50)).

## Improvements

- New function `get_token()` for getting the authentication token for different Seven Bridges platforms. The old function `misc_get_token()` is deprecated.
- `misc_make_metadata()` is deprecated, use `Metadata()` for metadata constructor instead.
- API status code has been updated to the latest version.
- Added docker pull stats badge ([#49](https://github.com/sbg/sevenbridges-r/issues/49)).

# sevenbridges 1.5.8

## Bug Fixes

- Fixed task creation issues introduced by the recently added field `created_time` in the API ([#51](https://github.com/sbg/sevenbridges-r/issues/51)).

- Fixed the user configuration file path (from `~/.sevenbridges/credential` to `~/.sevenbridges/credentials`), following the authentication specification. Thanks: Fabian Zimmer

# sevenbridges 1.5.6

## Improvements

### API Client

- Added `fields` as query default in API calls, the same with `limit`, `offset`; now requests on file details will use `fields = "_all"` directly so that only one request is issued. The same applies to updating a upload logic for folder/multiple files ([54488bc](https://github.com/sbg/sevenbridges-r/commit/54488bcaa77e999b198999e81af8c6c471908d9f)). Thanks: Raunaq Malhotra.

- Added functions `input_matrix()` and `output_matrix()` ([2ec7c84](https://github.com/sbg/sevenbridges-r/commit/2ec7c84ede491e50639502ca0d34bae04c5cde8f)) to extract input/output matrix from CWL JSON files directly, without converting CWL JSON to `Tool` or `Flow` objects. This is a faster implementation compared to the old method, and more stable to custom fields.

### CWL

- Added more default SBG fields to fix conversion errors; support array type enum ([79274b8](https://github.com/sbg/sevenbridges-r/commit/79274b8d75f838934d736c30f11da417416030c5), [8ff4c68](https://github.com/sbg/sevenbridges-r/commit/8ff4c6832e7aff63fbdeb0904d0d7f937334eb0c)).

## Bug Fixes

### CWL

- Fixed `filename` and `fileContent` conversion to `Expression` ([4c0a686](https://github.com/sbg/sevenbridges-r/commit/4c0a6867b597d0c6e882ed63216c7c3882257404)).

# sevenbridges 1.5.5

## Improvements

### API Client (Authentication)

- Support three authentication methods: direct authentication, via environment variables, or via user configuration file. Compliant with the new API client authentication specification. Please check the latest vignette `vignette("api", package = "sevenbridges")` for the current authentication methods.
- Updated platform types: now users can choose from `"cgc"`, `"aws-us"`, `"aws-eu"`, `"gcp"`, and `"cavatica"` in `Auth()` to avoid using API base URLs explicitly.
- Removed credential information parsing when package is loaded.

### API Client (Task and Upload)

- File ID support on task input (directly) ([#27](https://github.com/sbg/sevenbridges-r/issues/27))
- New argument `keep_test` for `task_add()` added: when users push a Tool object with no `sbg:job` information, can still use `keep_test = TRUE` to keep previous test information. ([#31](https://github.com/sbg/sevenbridges-r/issues/31))
- Support manifest file upload with fitler and subsetting for uploader ([#46](https://github.com/sbg/sevenbridges-r/issues/46))

### Docker Image

- Migrated Docker images to Seven Bridges Docker Hub account ([#43](https://github.com/sbg/sevenbridges-r/issues/43)), now users could use `docker pull sevenbridges/sevenbridges-r`

### CWL Support

- Improved robustness of `convert_app()` when a CWL JSON has more fields than defined ([#44](https://github.com/sbg/sevenbridges-r/issues/44))

### Miscellaneous

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
- Three examples inlcuded under inst/docker for CWL app examples
- Auth configuration file to maintain multiple platforms and user account
- Works for multiple Seven Bridges supported platforms
- More features like task hook function to ease the automation

# sevenbridges 1.0.0

## New Features

- Initial version
- All the APIs of the SBG platform are supported
- First vignette added
