# sevenbridges-r  <a href="https://www.sevenbridges.com"><img src="logo.png" align="right" alt="logo" height="128" width="128" /></a>

[![Build Status](https://travis-ci.org/sbg/sevenbridges-r.svg?branch=master)](https://travis-ci.org/sbg/sevenbridges-r)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/sbg/sevenbridges-r?branch=master&svg=true)](https://ci.appveyor.com/project/nanxstats/sevenbridges-r)
[![BioC](https://www.bioconductor.org/shields/years-in-bioc/sevenbridges.svg)](https://bioconductor.org/packages/release/bioc/html/sevenbridges.html#since)
[![Downloads](https://www.bioconductor.org/shields/downloads/sevenbridges.svg)](https://bioconductor.org/packages/stats/bioc/sevenbridges/)
[![Docker Pulls](https://img.shields.io/docker/pulls/sevenbridges/sevenbridges-r.svg)](https://hub.docker.com/r/sevenbridges/sevenbridges-r/)

[BioC (Release)](https://www.bioconductor.org/packages/release/bioc/html/sevenbridges.html) · [BioC (Development)](https://www.bioconductor.org/packages/devel/bioc/html/sevenbridges.html) · [GitHub (Latest)](https://github.com/sbg/sevenbridges-r)

## Overview

sevenbridges-r is an [R](https://www.r-project.org/)/[Bioconductor](https://www.bioconductor.org/) package that provides an interface for the [Seven Bridges Platform](https://www.sevenbridges.com/) (US, EU, China), [Cancer Genomics Cloud](https://www.cancergenomicscloud.org/), [Cavatica](http://www.cavatica.org/), and [FAIR4CURES](https://f4c.sbgenomics.com) public APIs.

The [Seven Bridges Platform](https://www.sevenbridges.com/) is a cloud-based environment for conducting bioinformatics analysis. It is a central hub for teams to store, analyze, and jointly interpret their bioinformatic data. The Platform co-locates analysis pipelines alongside the largest genomic datasets to optimize processing, allocating storage, and compute resources on demand.

The [Cancer Genomics Cloud (CGC)](https://www.cancergenomicscloud.org/), powered by [Seven Bridges](https://www.sevenbridges.com/), is also a cloud-based computation environment. It was built as one of three pilot systems funded by the [National Cancer Institute](https://www.cancer.gov) to explore the paradigm of colocalizing massive genomics datasets, like The [Cancer Genomics Atlas (TCGA)](https://cancergenome.nih.gov), alongside secure and scalable computational resources to analyze them. The CGC makes more than a petabyte of multi-dimensional data available immediately to authorized researchers. You can add your data to analyze alongside TCGA using predefined analytical workflows or your own tools.

[Cavatica](http://www.cavatica.org/), powered by [Seven Bridges](https://www.sevenbridges.com), is a data analysis and sharing platform designed to accelerate discovery in a scalable, cloud-based compute environment where data, results, and workflows are shared among the world's research community. Cavatica is built in collaboration with the Children Hospital of Philadelphia and it is focused on pediatric data.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
  - [Check R version](#check-r-version)
  - [Bioconductor - Release Branch](#bioconductor---release-branch)
  - [Bioconductor - Development Branch](#bioconductor---development-branch)
  - [Latest Development Version](#latest-development-version)
- [Features](#features)
  - [Flexible Authentication Methods](#flexible-authentication-methods)
  - [Complete API R Client](#complete-api-r-client)
  - [Task Monitoring](#task-monitoring)
  - [Batch Tasks Support](#batch-tasks-support)
  - [Cross Environment Support](#cross-environment-support)
  - [Common Workflow Language Tool Interface](#common-workflow-language-tool-interface)
  - [Utilities for Tool and Flow](#utilities-for-tool-and-flow)
- [Tutorials](#tutorials)
- [IDE Docker Image](#ide-docker-image)
- [FAQ](#faq)
- [Events](#events)
- [Contribute](#contribute)
- [Copyright](#copyright)

## Installation

### Check R version

First, check the version of R you are using with the following command (in R):

```r
R.version.string
```

If you are not running the latest release version of R, install or upgrade with [these instructions](https://www.bioconductor.org/install/#install-R). If you are using RStudio, restart RStudio after installing R. RStudio will detect the new installation.

### Bioconductor - Release Branch

This is recommended for most users as it is the most stable version.

You can install the package from the `release` branch on Bioconductor using `BiocManager`:

```r
install.packages("BiocManager")
BiocManager::install("sevenbridges")
```

### Bioconductor - Development Branch

If you are developing tools under the `devel` branch or use the development version of R and Bioconductor, install the package from the Bioconductor `devel` branch. You probably also want to install R-devel first by following the directions in ["Using the 'Devel' Version of Bioconductor"](https://bioconductor.org/developers/how-to/useDevel/).

To install the `sevenbridges` package from the `devel` branch, use

```r
install.packages("BiocManager")
BiocManager::install("sevenbridges", version = "devel")
```

### Latest Development Version

To try the latest features, please install the package directly from GitHub. We push to the Bioconductor branch (`release` and `devel`) regularly.

Installing the `sevenbridges` package from GitHub requires you have the `devtools` package. If you do not have `devtools`, install it from CRAN first.

```r
install.packages("devtools")
```

You may get an error for missing system dependecies such as curl and ssl. You probably need to do the following first in order to install `devtools` and to build vignettes since you need `pandoc` under Ubuntu.

```bash
apt-get update
apt-get install libcurl4-gnutls-dev libssl-dev pandoc pandoc-citeproc
```

After `devtools` is installed, install the latest version of `sevenbridges` from GitHub:

```r
install.packages("BiocManager")

devtools::install_github(
  "sbg/sevenbridges-r",
  repos = BiocManager::repositories(),
  build_vignettes = TRUE, dependencies = TRUE
)
```

If you have trouble with `pandoc` and do not want to install it,  set `build_vignettes = FALSE` to avoid building the vignettes.

## Features

The `sevenbridges` package includes the following features:

### Flexible Authentication Methods

Multiple authentication methods support.

- Direct authentication:

```r
# Direct authentication
a <- Auth(token = "your_token", platform = "cgc")

# or use base url
a <- Auth(token = "your_token", url = "https://cgc-api.sbgenomics.com/v2")
```

- Authentication via system environment variables:

```r
sbg_set_env(token = "your_token", url = "https://cgc-api.sbgenomics.com/v2")
a <- Auth(from = "env")
```

- Authentication via a user configuration file, collect and manage your credentials for multiple accounts across various Seven Bridges environments:

```r
a <- Auth(from = "file", profile_name = "aws-us-username")
```

Please check `vignette("api", package = "sevenbridges")` for technical details about all available authentication methods.

### Complete API R Client

A complete API R client with a user-friendly, object-oriented API with printing and support operations for API requests relating to users, billing, projects, files, apps, and tasks. Short examples are also included, as shown below:

```r
# Get a project by pattern-matching its name
p <- a$project("demo")

# Get a project by its id
p <- a$project(id = "username/demo")

# Delete files from a project
p$file("sample.tz")$delete()

# Upload fies from a folder to a project and include file metadata
p$upload("folder_path", metadata = list(platform = "Illumina"))
```

### Task Monitoring

A task monitoring hook which allows you to add a hook function to specific task statuses as you monitor a task. For example, you can opt to receive an email when the task is completed or specify to download all files produced by the task, as shown below:

```r
setTaskHook("completed", function() {
  tsk$download("~/Downloads")
})
tsk$monitor()
```

### Batch Tasks Support

Batch tasks by metadata and by item.

```r
# Batch by item
(tsk <- p$task_add(
  name = "RNA DE report new batch 2",
  description = "RNA DE analysis report",
  app = rna.app$id,
  batch = batch(input = "bamfiles"),
  inputs = list(
    bamfiles = bamfiles.in,
    design = design.in,
    gtffile = gtf.in
  )
))

# Batch by metadata. Note that input files must
# have relevant metadata fields specified.
(tsk <- p$task_add(
  name = "RNA DE report new batch 3",
  description = "RNA DE analysis report",
  app = rna.app$id,
  batch = batch(
    input = "fastq",
    c("metadata.sample_id", "metadata.library_id")
  ),
  inputs = list(
    bamfiles = bamfiles.in,
    design = design.in,
    gtffile = gtf.in
  )
))
```

### Cross Environment Support

Cross-platform support for Seven Bridges environments, such as [Cancer Genomics Cloud](https://www.cancergenomicscloud.org/) or [Seven Bridges Platform](https://www.sbgenomics.com/) on either Amazon Web Services or Google Cloud Platform.

### Common Workflow Language Tool Interface

A [Common Workflow Language (CWL)](http://www.commonwl.org/) Tool interface to directly describe your tool in R, export it to JSON or YAML, or add it to your online project. This package defines a complete set of CWL object, so you can describe tools as follows:

```r
fd <- fileDef(name = "runif.R", content = readr::read_file(fl))

rbx <- Tool(
  id = "runif",
  label = "runif",
  hints = requirements(
    docker(pull = "rocker/r-base"),
    cpu(1), mem(2000)
  ),
  requirements = requirements(fd),
  baseCommand = "Rscript runif.R",
  stdout = "output.txt",
  inputs = list(
    input(id = "number", type = "integer", position = 1),
    input(id = "min", type = "float", position = 2),
    input(id = "max", type = "float", position = 3)
  ),
  outputs = output(id = "random", glob = "output.txt")
)

# Print CWL JSON
rbx$toJSON(pretty = TRUE)

# Print CWL YAML
rbx$toYAML()
```

### Utilities for Tool and Flow

Utilities for `Tool` and `Flow`, for example

```r
library("sevenbridges")

# convert a SBG CWL JSON file
t1 <- system.file("extdata/app", "tool_star.json", package = "sevenbridges")

# convert json file into a Tool object
t1 <- convert_app(t1)

# shows all input matrix
t1$input_matrix()
```

## Tutorials

We maintain 3 different sets of documentation: the sevenbridges-r GitHub repository (latest and most up-to-date), Bioconductor release channel, and Bioconductor development channel. Below, only the GitHub version is linked to provide the latest documentation. For the other versions, please visit [Bioconductor Release version](https://www.bioconductor.org/packages/release/bioc/html/sevenbridges.html) or [Bioconductor Development version](https://www.bioconductor.org/packages/devel/bioc/html/sevenbridges.html). The tutorials below are re-generated regularly as we update the package on GitHub.

| Tutorial Title                                    | HTML       | Rmd Source   |
|:--------------------------------------------------|:-----------|:-------------|
| Complete Reference for the API R Client | [HTML](https://sbg.github.io/sevenbridges-r/articles/api.html) | [Source](https://raw.githubusercontent.com/sbg/sevenbridges-r/master/vignettes/api.Rmd) |
| Use R on the Cancer Genomics Cloud | [HTML](https://sbg.github.io/sevenbridges-r/articles/bioc-workflow.html) | [Source](https://raw.githubusercontent.com/sbg/sevenbridges-r/master/vignettes/bioc-workflow.Rmd) |
| Create a Docker Container and use Command Line Interface for R | [HTML](https://sbg.github.io/sevenbridges-r/articles/docker.html) | [Source](https://raw.githubusercontent.com/sbg/sevenbridges-r/master/vignettes/docker.Rmd) |
| Describe and execute Common Workflow Language (CWL) Tools and Workflows in R | [HTML](https://sbg.github.io/sevenbridges-r/articles/apps.html) | [Source](https://raw.githubusercontent.com/sbg/sevenbridges-r/master/vignettes/apps.Rmd) |
| <a name="ide"/>IDE container: Rstudio and Shiny server and more | [HTML](https://sbg.github.io/sevenbridges-r/articles/rstudio.html) | [Source](https://raw.githubusercontent.com/sbg/sevenbridges-r/master/vignettes/rstudio.Rmd) |
| Browse data on the Cancer Genomics Cloud via the Data Explorer, a SPARQL query, <br />or the Datasets API | [HTML](https://sbg.github.io/sevenbridges-r/articles/cgc-sparql.html) | [Source](https://raw.githubusercontent.com/sbg/sevenbridges-r/master/vignettes/cgc-sparql.Rmd) |

## IDE Docker Image

In the tutorial for [IDE container](#tutorials) above, we built a Docker container locally from which we can launch RStudio and Shiny. To launch RStudio and Shiny Server with the Seven Bridges IDE Docker container, do the following:

```shell
docker run  -d -p 8787:8787 -p 3838:3838 --name rstudio_shiny_server sevenbridges/sevenbridges-r
```

To mount a file system, you need to use `--privileged` with fuse.

```shell
docker run  --privileged -d -p 8787:8787 -p 3838:3838 --name rstudio_shiny_server sevenbridges/sevenbridges-r
```

Check out the IP from Docker Machine if you are on a Mac OS.

```bash
docker-machine ip default
```

In your browser, you can see where the RStudio server is located from the path `http://<url>:8787/`. For example, if 192.168.99.100 is returned, visit `http://192.168.99.100:8787/` for Rstudio.

For the Shiny server, each app__ is hosted at `http://<url>:3838/users/<username of rstudio>/<app_dir>` for the Shiny server. For example, an app called `01_hello` owned by user `rstudio` (a default user) has the path `http://<url>:3838/users/rstudio/01_hello/`. To develop your Shiny apps as an Rstudio user, you can login your RStudio server and create a folder in your home folder called `~/ShinyApps`. There, you can develop shiny apps in that folder. For example, you can create an app called `02_text` at `~/ShinyApps/02_text/`.

Log into your RStudio at `http://<url>:8787`. Then, try to copy an app to your home folder, as follows:

```r
dir.create("~/ShinyApps")
file.copy(
  "/usr/local/lib/R/site-library/shiny/examples/01_hello/",
  "~/ShinyApps/",
  recursive = TRUE
)
```

If you are logged in as user `rstudio`, visit  `http://192.168.99.100:3838/rstudio/01_hello`. You should be able to see the "hello" example.

_Note_: Generic Shiny apps can also be hosted at `http://<url>:3838/` or, for a particular app, at  `http://<url>:3838/<app_dir>`. Inside the Docker container, it's hosted under `/srv/shiny-server/`.

## FAQ

The best place to ask questions about the `sevenbridges` package is the [mailing list](https://groups.google.com/forum/#!forum/sevenbridges-r).

- __Q__: Does this package support Seven Bridges' API v1 which was not CWL compatible?<br />
  __A__: No. This package only supports API v2 +. For API v1, please check out the [sbgr](https://github.com/nanxstats/sbgr) package. Note that API v1 and associated legacy project types will be deprecated eventually.

- __Q__: Which version of the Common Workflow Language (CWL) is supported?<br />
  __A__: We support draft 2 and are making progress on supporting draft 3.

- __Q__: Is there a Python binding for the API?<br />
  __A__: Yes, the official Python client is [here](https://github.com/sbg/sevenbridges-python). Recipes and tutorials using the Python bindings are [here](https://github.com/sbg/okAPI).

- __Q__: Why do I get warning messages when I use the API R client?<br />
  __A__: The warning only exists in Rstudio and is potentially a bug in Rstudio. To ignore, it use `options(warn = -1)`

- __Q__: I still have problems despite dismissing the messages.<br />
  __A__: Please try to use the latest package on GitHub or [update installed Bioconductor packages](https://bioconductor.org/install/#update-bioconductor-packages). This usually includes the most recent bug fixes.

## Events

| Time              | Event                     | Location                      |
|:------------------|:--------------------------|:------------------------------|
| Jan 12, 2017 | [Genomics in the Cloud - Boston Bioconductor Meetup](https://www.meetup.com/Boston-R-Bioconductor-for-genomics/events/235580582/) (talk) [[slides](https://nanx.me/talks/bioc-meetup-cgc-170112.pdf)] | Dana-Farber Cancer Institute, Boston, MA |
| Sep 12 - 14, 2016 | [Probabilistic Modeling in Genomics](https://www.stats.ox.ac.uk/events/probgen16) (poster) | University of Oxford, Oxford, UK |
| May 27 - 29, 2016 | [The 9th China-R Conference](http://china-r.org/bj2016/index.html) (talk) | Renmin University of China, Beijng, China |
| Jun 27 - 30, 2016 | [The R User Conference 2016](http://user2016.org/) (talk) | Stanford University, Stanford, CA |
| Jun 24 - 26, 2016 | [BioC 2016: Where Software and Biology Connect](https://bioconductor.org/help/course-materials/2016/BioC2016/) (workshop) | Stanford University, Stanford, CA |
| Apr 1 - 3, 2016   | [NCI Cancer Genomics Cloud Hackathon](https://www.cancergenomicscloud.org/hacking-cancer/) (tutorial)<br />[[HTML](https://sbg.github.io/sevenbridges-r/articles/bioc-workflow.html)] [[R Markdown Source](https://raw.githubusercontent.com/sbg/sevenbridges-r/master/vignettes/bioc-workflow.Rmd)] | Seven Bridges Genomics, Inc., Boston, MA |

## Contribute

Please file bug reports/feature requests on the [issue page](https://github.com/sbg/sevenbridges-r/issues), or create pull requests [here](https://github.com/sbg/sevenbridges-r/pulls).

Contributors should read the [Seven Bridges Notice to Contributors](https://drive.google.com/file/d/0B9ms5nmyMyvSY3ZmeDRqV1p4SHc/view?usp=sharing) and sign the [Seven Bridges Contributor Agreement](https://secure.na1.echosign.com/public/esignWidget?wid=CBFCIBAA3AAABLblqZhAqt_9rHEqy2MggS0uWRmKHUN2HYi8DWNjkgg5N68iKAhRFTy7k2AOEpRHMMorxc_0*) before submitting a pull request.

## Copyright

© 2018 Seven Bridges Genomics, Inc. All rights reserved.

This project is licensed under the terms of the [Apache License 2.0](LICENSE).
