# R Package for Seven Bridges Platform

<!-- [![Build Status](https://travis-ci.org/road2stat/sevenbridges.png?branch=master)](https://travis-ci.org/tengfei/sevenbridges) -->

Please checkout the [bioconductor page](http://bioconductor.org/packages/3.3/bioc/html/sevenbridges.html) for more information

## Features

- Complete API R client with user friendly API like a$project("demo")$file("sample.tz")$delete()

- CWL Tool interface, you can directly describe your tool in R, export to JSON/YAML, or add it to your online project. This package defines a complete set
of CWL object.

- Task monitoring hook allow you to add hook function to task status when you monitor a task, for example, when task is finished sent you email or download all files.

- Cross platform support, if it's cwl compatible platform from Seven
  Bridges, like Cancer genomics cloud or Seven bridges on google and
  AWS, you can use it.

- Authentification management for multiple platforms/users via config file.


## Tutorials

Tutorials are listed on  [bioconductor page](http://bioconductor.org/packages/3.3/bioc/html/sevenbridges.html)


## Installation

The package is currently not available in the Bioconductor `release` branch yet until next Bioc release cycle, please switch to the `devel` branch by following code to install.

```
source("http://bioconductor.org/biocLite.R")
useDevel(devel = TRUE)
biocLite("sevenbridges")
```

Alternatively, you can install the latest development version of the package from GitHub too:

```
# install.packages("devtools") if devtools was not installed
source("http://bioconductor.org/biocLite.R")
##  useDevel() ## ignore the erro if you cannot use devel
## biocLite("BiocUpgrade") ## don't have to run
library(devtools)
install_github("tengfei/sevenbridges", build_vignettes=TRUE, 
  repos=BiocInstaller::biocinstallRepos(),
  dependencies=TRUE)
```

To load the package in R, simply call

```
library("sevenbridges")
```

## Launch Rstudio Server and Shiny Server with sevenbridges utils

```
docker run  -d -p 8787:8787 -p 80:3838 tengfei/sevenbridges:local &
```

check out the ip

```
docker-machine ip default
```
`http://<url>:8787/` for Rstudio
`http://<url>:8787/<username of rstudio>/` for Shiny server

For example, if 192.168.99.100 is what returned, visit `http://192.168.99.100:8787/` for Rstudio.

Note: for users of that Rstudio, just create `ShinyApps` folder under
your home folder, then put your apps under that folder, so you can
visit `http://<url>:8787/<username of rstudio>/<app name>` for your
shiny apps. For example ``http://192.168.99.100:8787/tengfei/hello`

<hr>

© Seven Bridges Genomics 2012 - 2016. Licensed under the MIT license.
