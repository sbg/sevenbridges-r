#!/usr/bin/Rscript
"usage: report.R [options]

options:


--engine=<string>           packrat or liftr (docker in docker) or NA [default: packrat]
--shinyTemplate=<file>      Shinay app template as zipped(.zip) or tar(tar.gz) file.
--knitrTemplate=<file>      Rmarkdown file template will be rendered by knitr.
--data=<file>               Files to be included in data folder under app folder.
--www=<file>                Files to be included in www folder under app folder.
--src=<file>                Files to be included in src folder under app folder.
--appFiles=<file>           Files to be included in root at your app folder.
--setAccountInfo=<string>   shinyapps::setAccountInfo
--name=<string>             Name of account to save or remove, check shinyapps::setAccountInfo
--token=<string>            User token for the account, check shinyapps::setAccountInfo
--secret=<string>           User secret for the account, check shinyapps::setAccountInfo
--contentCategory=<string>  Optional; the kind of content being deployed (e.g. 'plot', 'document', or 'application').
--account=<string>          Account to deploy application to. This parameter is only required for the initial deployment of an application when there are multiple accounts configured on the system (see accounts).
--server=<string>           Server name. Required only if you use the same account name on multiple servers.
--quiet                     Request that no status information be printed to the console during the deployment.
" -> doc

.curPath <- normalizePath(".")
library(docopt)
## a hack for packrat
install.packages(c('shiny', 'rsconnect', 'packrat'),
                 repos='https://cran.rstudio.com/', quiet = TRUE)

opts <- docopt(doc)
deFiles <- function(x, split = ","){
    strsplit(x, split)[[1]]
}

engine <- opts$engine

## make working directory
app_name <- function(x, prefix = NULL, suffix = NULL,...){
    nm <- tools::file_path_sans_ext(basename(x), ...)
    paste0(prefix, nm, suffix)
}



copyFiles <- function(fl, opts){


    appName <- tools::file_path_sans_ext(basename(fl), compression = TRUE)
    ## dir.create(appName)
    .fullPath <- file.path(.curPath, appName)
    dir.create(.fullPath, FALSE)

    ## copy file over
    if(!is.null(opts$data)){
        message("copy to data folder")
        .data <- file.path(.fullPath, "data")
        dir.create(.data, FALSE)
        file.copy(deFiles(opts$data), .data, overwrite = TRUE, recursive = TRUE)
    }

    if(!is.null(opts$www)){
        message("copy to www folder")
        .www <- file.path(.fullPath, "www")
        dir.create(.www, FALSE)
        file.copy(deFiles(opts$www), .www,  overwrite = TRUE, recursive = TRUE)
    }

    if(!is.null(opts$src)){
        message("copy to src folder")
        .src <- file.path(.fullPath, "src")
        dir.create(.src, FALSE)
        file.copy(deFiles(opts$src), .src,  overwrite = TRUE, recursive = TRUE)
    }

    if(!is.null(opts$appFiles)){
        message("copy to root folder")
        file.copy(deFiles(opts$appFiles), file.path(.fullPath))
    }

    .fullPath
}



unpackShiny <- function(input, outdir = NULL){
    # ## extract

    appName <- tools::file_path_sans_ext(basename(input), compression = TRUE)
    extName <- tools::file_ext(basename(input))
    if(is.null(outdir)){

        outdir <- file.path(.curPath, appName)

    }
    dir.create(outdir, FALSE)
    ## .d <- tempfile()
    .d <- file.path(.curPath,basename(tempfile()))
    dir.create(.d, FALSE)

    message("Uncompress into ", .d)

    switch(extName,
           zip = {unzip(input, exdir = .d)},
           gz = {untar(input, exdir = .d)},
           tar = {untar(input, exdir = .d)},
           tar.gz = {untar(input, exdir = .d)},
           {
               setwd(.d)
               system(paste("unp", input))
               setwd(.curPath)
           })

    .ds <- list.dirs(.d, recursive = FALSE)

    if(liftr:::is_shinyapp(.d)){
        cmd <- paste("cp -R", file.path(dirname(.d), basename(.d), "*"),
                     outdir)
        message(cmd)
        system(cmd)
    }else if(length(.ds) == 1 &&
             liftr:::is_shinyapp(.ds[1])){
        cmd <- paste("cp -R", file.path(dirname(.ds[1]), basename(.ds[1]), "*"),
                     outdir)
        message(cmd)
        system(cmd)

    }else{
        warning("it's probably not shiny app")
        cmd <- paste("cp -R", file.path(dirname(.d), basename(.d), "*"),
                     outdir)
        message(cmd)
        system(cmd)
    }

}

## Set account info for Shiny apps
toDeploy <- TRUE
if(is.null(opts$setAccountInfo)){
    if(any(is.null(opts$name), is.null(opts$token), is.null(opts$secret))){
        toDeploy <- FALSE
    }else{
        rs = paste("rsconnect::setAccountInfo(name =", opts$name, ",",
                   "token = ", opts$token, ",",
                   "secret = ", opts$secret)
    }
}else{
    ## allow you to copy-paste from shinyapps.io or other services
    rs = opts$setAccountInfo
}






## Start with knitr template
if(!is.null(opts$knitrTemplate)){
    ## create rmarkdown
    fls <- deFiles(opts$knitrTemplate)

    sapply(fls, function(x){
        .fullPath <- copyFiles(x, opts)
        file.copy(x, file.path(.fullPath),  overwrite = TRUE, recursive = TRUE)
        x <- file.path(.fullPath, basename(x))
        message("Rendering ...", x)
        switch(engine,
               liftr = {
                   library(liftr)
                   o <- Onepunch(input = x)
                   o$onepunch(script = rs)
               },
               packrat = {
                   .dd <- tempfile()
                   dir.create(.dd, FALSE)
                   ## build in temp dir
                   file.copy(x, .dd,  overwrite = TRUE, recursive = TRUE)
                   suppressMessages({
                       packrat::init(.dd)
                       install.packages(c('shiny', 'rmarkdown', 'devtools'),
                                        repos='https://cran.rstudio.com/',
                                        quiet = TRUE)
                       devtools::install_github(c('rstudio/rsconnect', 'rstudio/shinyapps'),
                                                quiet = TRUE)
                   })
                   ## fixme
                   rmarkdown::render(x, output_dir = .curPath)
                   packrat::off()


               },
               {
                   rmarkdown::render(x, output_dir = .curPath)
               })
    })

}

# Start with shiny template
if(!is.null(opts$shinyTemplate)){
    ## make working directory

    fls <- deFiles(opts$shinyTemplate)
    sapply(fls, function(x){

        message("unpacking shiny app ", basename(x), " ...")
        unpackShiny(x)
        .fullPath <- copyFiles(x, opts)


        setwd(.curPath)
        tar.name <- paste0(basename(.fullPath), ".tar")
        message("tarname: ", tar.name)
        tar(tar.name, .fullPath)


        if(toDeploy){
            switch(engine,
                   liftr = {
                       system("service docker start")
                       library(liftr)

                       o <- Onepunch(input = .fullPath)
                       o$deploy(script = rs)
                   },
                   packrat = {
                       .dd <- file.path(.curPath,basename(tempfile()))
                       dir.create(.dd, FALSE)
                       unpackShiny(x, .dd)

                       packrat::init(.dd)

                       install.packages(c('shiny', 'rsconnect'),
                                        repos='https://cran.rstudio.com/',
                                        quiet = TRUE)


                       rsconnect::setAccountInfo(name =opts$name,
                                                 token = opts$token,
                                                 secret = opts$secret)
                       message("deploy shiny app: ", .fullPath)
                       rsconnect::deployApp(.fullPath)
                       packrat::off()

                   },
                   {
                       message("deploy shiny app: ", .fullPath)
                       rsconnect::deployApp(.fullPath)
                   })

        }
    })
    message("list.files:\n ",
            paste(list.files(.curPath), collapse = "\n"))

}
