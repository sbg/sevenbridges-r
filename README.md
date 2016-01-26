# R Client for Seven Bridges Genomics API

<!-- [![Build Status](https://travis-ci.org/road2stat/sevenbridges.png?branch=master)](https://travis-ci.org/tengfei/sevenbridges) -->

The `sevenbridges` package provides an R client for accessing the [Seven Bridges Genomics API](http://developer.sbgenomics.com/) and describe tools in CWL.

## Installation

The package is currently not availble in the Bioconductor `release` branch yet until next Bioc release cycle, please switch to the `devel` branch by following code to install.

```
source("http://bioconductor.org/biocLite.R")
useDevel(devel = TRUE)
biocLite("sevenbridges")
```

Alternatively, you can install the latest development version of the package from GitHub too:

```
# install.packages("devtools") if devtools was not installed
library("devtools")
install_github("tengfei/sevenbridges")
```

To load the package in R, simply call

```
library("sevenbridges")
```

<hr>

© Seven Bridges Genomics 2012 - 2016. Licensed under the MIT license.
