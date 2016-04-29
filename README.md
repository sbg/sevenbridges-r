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
[[html](http://www.tengfei.name/sevenbridges/vignettes/bioc-workflow.html)] 
[[R markdown](http://www.tengfei.name/sevenbridges/vignettes/bioc-workflow.Rmd)] 
[[R script](http://www.tengfei.name/sevenbridges/vignettes/bioc-workflow.R)]

### Features

- Complete API R client with user friendly API in object-oriented fashion with user friendly printing, support operation on users, billing, project, file, app, task etc, short example.

```r
## get project
p = a$project("demo")
## detele files from a project
p$file("sample.tz")$delete()
## upload fies from a folder to a project with metadata
p$upload("folder_path", metadata = list(platform = "Illumina"))
```

- Task monitoring hook allow you to add hook function to task status when you monitor a task, for example, when task is finished sent you email or download all files.

- Cross platform support from Seven
  Bridges, such as [NCI Cancer genomics cloud](http://www.cancergenomicscloud.org/) or [Seven bridges](https://www.sbgenomics.com/) on google and
  AWS, you can use it.

- Authentification management for multiple platforms/users via config file.

```r
## standard 
a = Auth(token = "fake_token", url = "api_url")
## OR from config file, multiple platform/user support
a = Auth(username = "tengfei", platform = "cgc")
```

- CWL Tool interface, you can directly describe your tool in R, export to JSON/YAML, or add it to your online project. This package defines a complete set
of CWL object. So you can describe tools like this

```r
library(readr)
fd <- fileDef(name = "runif.R",
              content = read_file(fl))
rbx <- Tool(id = "runif", 
            label = "runif",
            hints = requirements(docker(pull = "rocker/r-base"), 
                                 cpu(1), mem(2000)),
            requirements = requirements(fd),
            baseCommand = "Rscript runif.R",
            stdout = "output.txt",
            inputs = list(input(id = "number",
                                type = "integer",
                                position = 1),
                          input(id = "min",
                                type = "float",
                                position = 2),
                          input(id = "max",
                                type = "float",
                                position = 3)),
            outputs = output(id = "random", glob = "output.txt")) 
## output cwl JSON            
rbx$toJSON(pretty = TRUE) 
## output cwl YAML
rbx$toYAML()
```

- Describe simple linear workflow by connect Tool objects like this

```r
tool1 %>>% tool2 %>>% tool3
```


### Installation

__[Recommended: Stable]__ After bioconductor 3.3 [release](http://bioconductor.org/developers/release-schedule/) on May 5th, 2016, you will be able to install like this. For most users, I will recommend this installation, it's most stable. 

```r
source("http://bioconductor.org/biocLite.R")
biocLite("sevenbridges")
```

__[Recommended: Devel]__ Install from bioconductor `devel` branch if you are developing tools in devel branch or if you are users who use devel version for Bioconductor. You need to install R-devel first, please follow the instruction ["Using the Devel Version of Bioconductor"](http://bioconductor.org/developers/how-to/useDevel/). After upgrade of R. 

```r
source("http://bioconductor.org/biocLite.R")
useDevel(devel = TRUE)
biocLite("sevenbridges")
```

__[Latest]__ Alternatively, you can always install the latest development version of the package from GitHub, it's always the most latest 
version, fresh new but not fully tested,  with all new features and hot fixes, before we push to any bioconductor branch (release/devel) 

```r
# install.packages("devtools") if devtools was not installed
source("http://bioconductor.org/biocLite.R")
library(devtools)
install_github("sbg/sevenbridges-r", build_vignettes=TRUE, 
  repos=BiocInstaller::biocinstallRepos(),
  dependencies=TRUE)
```

If you have trouble with pandoc, either install it or set `build_vignettes = FALSE` to avoid vignettes build, 

To load the package in R, simply call

```r
library("sevenbridges")
```

### More tutorials 

- Complete Guide for API R Client
[[github](http://www.tengfei.name/sevenbridges/vignettes/api.html)]
[[bioc-devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/api.html)]
- Master Tutorial: use R for Cancer Genomics Cloud
[[github](http://www.tengfei.name/sevenbridges/vignettes/bioc-workflow.html)]
[[bioc-devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/bioc-workflow.html)]
- Creating Your Docker Container and Command Line Interface
[[github](http://www.tengfei.name/sevenbridges/vignettes/docker.html)]
[[bioc-devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/docker.html)]
- Describe CWL Tools/Workflows in R and Execution
[[github](http://www.tengfei.name/sevenbridges/vignettes/apps.html)]
[[bioc-devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/apps.html)]
- Using Rstudio and Shiny
[[github](http://www.tengfei.name/sevenbridges/vignettes/rstudio.html)]
[[bioc-devel](http://www.bioconductor.org/packages/3.3/bioc/vignettes/sevenbridges/inst/doc/rstudio.html)]
- Reference [[bioc-devel](http://www.bioconductor.org/packages/3.3/bioc/manuals/sevenbridges/man/sevenbridges.pdf)]

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





### Issue report

All feedback are welcomed! Please file bug/issue/request on the [issue page](https://github.com/sbg/sevenbridges-r/issues) here on github, we wil 
try to respond asap.

### Q&A

- __Q__: Does this package support older API which is not cwl compatible?<br />
  __A__: No it only supports API v2 +, for older version, please check [sbgr](https://github.com/road2stat/sbgr) package, but 
please note that the old API or project type will be deprecated. 

- __Q__: Which version of CWL (common worklfow language) supported now? <br />
  __A__: Draft 2, progress on draft 3.
  
- __Q__: Is there a python binding for the API? <br />
  __A__: Yes, official python client is [here](https://github.com/sbg/sevenbridges-python). And lots pyton recipes are now [here](https://github.com/sbg/okAPI)
  
- __Q__: Why I always get warning message when I use API R client?
- __A__: It only exists in Rstudio, potentially a bug in Rstudio. To ignore it use `options(warn = -1)`


<hr>

© Seven Bridges Genomics 2012 - 2016. Licensed under the MIT license.
