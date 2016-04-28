[![Build Status](https://travis-ci.org/sbg/sevenbridges-r.svg?branch=master)](https://travis-ci.org/sbg/sevenbridges-r)
![platform](http://www.bioconductor.org/shields/availability/devel/sevenbridges.svg)
![bioc](http://www.bioconductor.org/shields/years-in-bioc/sevenbridges.svg)


# sevenbridges: R Package for Seven Bridges Platform from API client to CWL generator 

Bioconductor-Stable |  [Bioconductor-Devel](http://bioconductor.org/packages/3.3/bioc/html/sevenbridges.html) 

### Events

- May 27 - 29, 2016 [The 9th China-R Conference 2016](http://china-r.org/bj2016/index.html) @ Remin University, Beijng, China. (talk)
- June 24 - 26, 2016 [BioC 2016: Where Software and Biology Connect](http://bioconductor.org/help/course-materials/2016/BioC2016/) @ Stanford University, Stanford, CA (workshop)
- June 27 - 30, 2016 [The R User Conference 2016](http://user2016.org/) @ Stanford University, Stanford, CA (talk)
- (Past) [NCI Cancer Genomics Cloud Hackathon Tutorials](http://www.cancergenomicscloud.org/hacking-cancer/) @ Boston 2016
[[html](http://www.tengfei.name/sevenbridges/cgc-hackathon-2016-04/bioc-workflow.html)] 
[[R markdown](https://raw.githubusercontent.com/tengfei/sevenbridges/master/vignettes/bioc-workflow.Rmd)] 
[[R script](http://www.tengfei.name/sevenbridges/cgc-hackathon-2016-04/bioc-workflow.R)]

### Features

- Complete API R client with user friendly API in object-oriented fashion with user friendly printing, support operation on users, billing, project, file, app, task etc, short example.

```
## get project
p = a$project("demo")
## detele files from a project
p$file("sample.tz")$delete()
## upload fies from a folder to a project with metadata
p$upload("folder_path", metadata = list(platform = "Illumina"))`
```

- CWL Tool interface, you can directly describe your tool in R, export to JSON/YAML, or add it to your online project. This package defines a complete set
of CWL object.

- Task monitoring hook allow you to add hook function to task status when you monitor a task, for example, when task is finished sent you email or download all files.

- Cross platform support from Seven
  Bridges, such as [NCI Cancer genomics cloud](http://www.cancergenomicscloud.org/) or [Seven bridges](https://www.sbgenomics.com/) on google and
  AWS, you can use it.

- Authentification management for multiple platforms/users via config file.


### Installation

__[Recommended: Stable]__ After bioconductor 3.3 [release](http://bioconductor.org/developers/release-schedule/) on May 5th, 2016, you will be able to install like this. For most users, I will recommend this installation, it's most stable. 

```
source("http://bioconductor.org/biocLite.R")
biocLite("sevenbridges")
```

__[Recommended: Devel]__ Install from bioconductor `devel` branch by following code to install, if you are developing tools in devel branch
or if you are users who use devel version for Bioconductor.

```
source("http://bioconductor.org/biocLite.R")
useDevel(devel = TRUE)
biocLite("sevenbridges")
```

__[Latest]__ Alternatively, you can always install the latest development version of the package from GitHub, it's always the most latest 
version, fresh new but not fully tested,  with all new features and hot fixes, before we push to any bioconductor branch (release/devel) 

```
# install.packages("devtools") if devtools was not installed
source("http://bioconductor.org/biocLite.R")
library(devtools)
install_github("sbg/sevenbridges-r", build_vignettes=TRUE, 
  repos=BiocInstaller::biocinstallRepos(),
  dependencies=TRUE)
```

If you have trouble with pandoc, either install it or set `build_vignettes = FALSE` to avoid vignettes build, 

To load the package in R, simply call

```
library("sevenbridges")
```

### Launch Rstudio Server and Shiny Server with sevenbridges IDE docker container

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

### More tutorials

- Complete Guide for API R Client [[devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/api.html)]
- Master Tutorial: use R for Cancer Genomics Cloud [[devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/bioc-workflow.html)]
- Creating Your Docker Container and Command Line Interface [[devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/docker.html)]
- Describe CWL Tools/Workflows in R and Execution [[devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/apps.html)]
- Using Rstudio and Shiny [[devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/rstudio.html)]
- Reference [[devel](http://www.bioconductor.org/packages/3.3/bioc/manuals/sevenbridges/man/sevenbridges.pdf)]



### Issue report

All feedback are welcomed! Please file bug/issue/request on the [issue page](https://github.com/sbg/sevenbridges-r/issues) here on github, we wil 
try to respond asap.

### Q&A

- __Q__: Does this package support older API which is not cwl compatible?<br />
  __A__: No it only supports API v2 +, for older version, please check [sbgr](https://github.com/road2stat/sbgr) package, but 
please note that the old API or project type will be deprecated. 

- __Q__: Which version of CWL (common worklfow language) supported now? <br />
  __A__: draft 2, progress on draft 3.
  
- __Q__: Is there a python binding for the API? <br />
  __A__: Yes, official python client is [here](https://github.com/sbg/sevenbridges-python). And lots pyton recipes are now [here](https://github.com/sbg/okAPI)


<hr>

© Seven Bridges Genomics 2012 - 2016. Licensed under the MIT license.
