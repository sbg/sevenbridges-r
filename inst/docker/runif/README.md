# runif example

Add external script to the docker images and grant permission to execute it. In this way user can maintained your command line interface/script separately with docker image.

## Directory structure

```
.
├── Dockerfile
├── README.md
├── report
│   └── report.Rmd
└── src
    └── runif.R
```