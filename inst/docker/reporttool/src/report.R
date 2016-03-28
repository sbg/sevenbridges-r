#!/usr/local/bin/Rscript
"usage: report.R [options] 

options:

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
--account=<string>	    Account to deploy application to. This parameter is only required for the initial deployment of an application when there are multiple accounts configured on the system (see accounts).
--server=<string>	    Server name. Required only if you use the same account name on multiple servers.
--quiet	                    Request that no status information be printed to the console during the deployment.
" -> doc


library(docopt)
opts <- docopt(doc)
deFiles <- function(x, split = ","){
    strsplit(x, split)[[1]]    
}

dir.create(appName)
.fullPath <- normalizePath(appName)

## copy file over
if(!is.null(opts$data)){
    message("copy to data folder")
    .data <- file.path(.fullPath, "data")
    dir.create(.data)
    file.copy(deFiles(opts$data), .data, overwrite = TRUE, recursive = TRUE)        
}

if(!is.null(opts$www)){
    message("copy to www folder")        
    .www <- file.path(.fullPath, "www")
    dir.create(.www)
    file.copy(deFiles(opts$www), .www,  overwrite = TRUE, recursive = TRUE)        
}

if(!is.null(opts$src)){
    message("copy to src folder")                
    .src <- file.path(.fullPath, "src")
    dir.create(.src)
    file.copy(deFiles(opts$src), .src,  overwrite = TRUE, recursive = TRUE)        
}

if(!is.null(opts$appFiles)){
    message("copy to root folder")                        
    file.copy(deFiles(opts$appFiles), file.path(.fullPath))        
}

## Set account info for Shiny apps
toDeploy <- TRUE
if(is.null(opts$setAccountInfo)){
    if(any(is.null(opts$name), is.null(opts$token), is.null(opts$secret))){
        toDeploy <- FALSE
    }else{
        rs = paste("rsconnect::setAccountInfo(name =", opts$name,
                                  "token = ", opts$token,
                                  "secret = ", opts$secret)
    }
}else{
    ## allow you to copy-paste from shinyapps.io or other services
    rs = opts$setAccountInfo        
}

## make working directory
if(!is.null(opts$shinyTemplate)){
    ## working on shiny apps
    appName <- tools::file_path_sans_ext(basename(opts$shinyTemplate), compression = TRUE)
    extName <- tools::file_ext(basename(opts$shinyTemplate))

    ## dir.create(appName)
    ## .fullPath <- normalizePath(appName)

    ## extract
    message("Uncompress ....")
    switch(extName,
           zip = {unzip(opts$shinyTemplate, exdir = .fullPath)},
           gz = {untar(opts$shinyTemplate, exdir = .fullPath)},
           {stop("unsupported compressed shiny template format, try tar.gz or zip")})
    ## template is ready
    
    # ## application ready
    # if(toDeploy){
    #     message("deployApps ...")
    #     message("current path: ", getwd())
    #     message(".fullPath: ", .fullPath)
    #     list.files(.fullPath)
    #     rsconnect::deployApp(.fullPath, lint = FALSE)
    # }

    ## output compressed app
    tar(paste0(.fullPath, ".tar.gz"), files = list.files(.fullPath, recursive = TRUE, full.names = TRUE))

    library(liftr)
    o <- Onepunch(input = .fullPath)
    o$deploy(script = rs)
}


## create knitr template
if(!is.null(opts$knitrTemplate)){
    ## create rmarkdown
    message("copy knitr template to app root")
    fls <- deFiles(opts$knitrTemplate)
    file.copy(fls, file.path(.fullPath),  overwrite = TRUE, recursive = TRUE)
    fls <- file.path(.fullPath, fls)
    sapply(fls, function(x){
        message("Rendering ...", x)        
        rmarkdown::render(x, output_dir = ".")        
    })
}









