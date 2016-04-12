# NCI Cancer Genomics Cloud Hackathon Tutorials

Workshop tutorial: [[html](http://www.tengfei.name/sevenbridges/cgc-hackathon-2016-04/bioc-workflow.html)] [[R markdown](https://raw.githubusercontent.com/tengfei/sevenbridges/master/vignettes/bioc-workflow.Rmd)] [[R script](http://www.tengfei.name/sevenbridges/cgc-hackathon-2016-04/bioc-workflow.R)]

# R Package for Seven Bridges Platform

You can also checkout the [bioconductor page](http://bioconductor.org/packages/3.3/bioc/html/sevenbridges.html) for more information

## Features

- Complete API R client with user friendly API like 

`a$project("demo")$file("sample.tz")$delete()` 

`a$project(id = "tengfei/quickstart")$upload("folder_path", metadata = list(platform = "Illumina"))`


- CWL Tool interface, you can directly describe your tool in R, export to JSON/YAML, or add it to your online project. This package defines a complete set
of CWL object.

- Task monitoring hook allow you to add hook function to task status when you monitor a task, for example, when task is finished sent you email or download all files.

- Cross platform support, if it's cwl compatible platform from Seven
  Bridges, like Cancer genomics cloud or Seven bridges on google and
  AWS, you can use it.

- Authentification management for multiple platforms/users via config file.


## Tutorials

Tutorials are listed on  [bioconductor page](http://bioconductor.org/packages/3.3/bioc/html/sevenbridges.html)

```
browseVignettes("sevenbridges")
```


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

If you have trouble with pandoc, either instal it or set `build_vignettes = FALSE`, 

To load the package in R, simply call

```
library("sevenbridges")
```

## Launch Rstudio Server and Shiny Server with sevenbridges IDE docker container

```
docker run  -d -p 8787:8787 -p 3838:3838 tengfei/sevenbridges
```

check out the ip from docker machine if you are on mac os.

```
docker-machine ip default
```

In your browser, 

`http://<url>:8787/` for Rstudio

`http://<url>:3838/<username of rstudio>/app_dir` for Shiny server

For example, if 192.168.99.100 is what returned, visit `http://192.168.99.100:8787/` for Rstudio.

Note: for users of that Rstudio, just create `ShinyApps` folder under
your home folder, then put your apps under that folder, so you can
visit `http://<url>:3838/<username of rstudio>/<app name>` for your
shiny apps. For example 

In your Rstudio server launched from container, please run

```
file.copy("/usr/local/lib/R/site-library/shiny/examples/01_hello/", "~/ShinyApps/", recursive = TRUE)
```

If you are login as username 'rstudio', then visit  `http://192.168.99.100:3838/rstudio/01_hello` you should be
able to see the hello example.


<hr>

© Seven Bridges Genomics 2012 - 2016. Licensed under the MIT license.
