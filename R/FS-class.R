#' FS class
#'
#' FS class
#'
#' @param serverAddress placehoder
#' @param apiAddress placehoder
#' @param vsfsJar placehoder 
#' @param cacheDir placehoder
#' @param cacheSize placeholder
#' @param projectID placeholder
#' 
#' @importFrom uuid UUIDgenerate
#'
#' @export FS
FS <- setRefClass("FS", 
                    fields = list(mountPoint = "character",
                        mode = "character",
                        debug = "logical",
                        cacheDir = "character",
                        cacheSize = "character",
                        apiAddress = "character",
                        authToken = "character",
                        projectId = "list",
                        lastProjectId = "characterORNULL",
                        secure = "logical",
                        serverAddress = "character",
                        vsfsJar = "characterORNULL"),
                    methods = list(
                        initialize = function(serverAddress = 'fs.sbgenomics.com',
                            apiAddress ='https://api.sbgenomics.com',
                            vsfsJar = NULL,
                            cacheDir = '~/vsfs_cache',
                            cacheSize = '10GB', 
                            projectId = list(),
                            ...){

                            if(is.null(vsfsJar)){
                                ## vsfsJar <<- system.file("java", "sbg-vsfs.jar", package = "vsfsr")
                                stop("please provie jar library")
                            }else{
                                vsfsJar <<- vsfsJar
                            }
                            
                            serverAddress <<- serverAddress
                            apiAddress <<- apiAddress
                            
                            cacheDir <<- cacheDir
                            cacheSize <<- cacheSize      
                            ## db <<- db
                            ## icoll <<- icoll
                            
                            ## check projectId, has to be integer if any
                            if(length(projectId)){
                                if(!is.list(projectId)){
                                    if(length(projectId) > 1 && is.numeric(projectId)){
                                        .projectId <- as.list(projectId)
                                    }else{                  
                                        .projectId <- list(projectId)
                                    }
                                    
                                }else{
                                    .projectId <- projectId
                                }
                                .projectId <- lapply(.projectId, function(id){
                                    if(is.numeric(id) && !is.integer(id)){
                                        id <- as.integer(id)
                                        message("Converting numerical value to integer")
                                    }
                                    stopifnot(is.integer(id))
                                    id
                                    
                                })
                                projectId <<- .projectId
                                
                            }
                            
                            lastProjectId <<- unlist(tail(projectId, n = 1))
                            
                            
                            callSuper(...)
                        }
                    ))



FS$methods(
    checkMount = function(){
        'check if a path is mounted'
    },
    mount = function(mountPoint = NULL, projectId = NULL, ignore.stdout = TRUE, sudo = TRUE, ...){
        'mount a specific project if projectId is provided, otherwise mount all projects'
        projectId <<- c(.self$projectId, list(projectId))
        lastProjectId <<- unlist(tail(projectId, n = 1))
        if(is.null(mountPoint)){
            stop("mountPoint not provided")
        }else{
            dir.create(normalizePath(mountPoint))
            mountPoint <<- normalizePath(mountPoint)            
        }
        message("mount")
        
        ## create uid
        uid <- UUIDgenerate()
        
        ## create cache dir at local home? is there a problem?
        .cacheDir <- file.path(cacheDir, uid)
        
        dir.create(.cacheDir, recursive = TRUE)
        
        .cacheDir <- normalizePath(.cacheDir)
        ## stdout and stderr path
        stdout <- file.path(cacheDir, paste0(uid, ".out"))
        stderr <- file.path(cacheDir, paste0(uid, ".err"))
        
        ## run command
        if(!is.null(projectId)){
            cmd <- paste("java", "-Xmx128m", "-jar", vsfsJar, "-ssl",
                         "-as", apiAddress, 
                         "-s", serverAddress,
                         '-mountPoint', .self$mountPoint,
                         '-authToken', authToken,
                         '-cacheDir', .cacheDir,
                         '-cacheSize', cacheSize, ## check project id for list
                         '-projectId', projectId)
        }else{
            cmd <- paste("java", "-Xmx128m", "-jar", vsfsJar, "-ssl",
                         "-as", apiAddress, 
                         "-s", serverAddress,
                         '-mountPoint', .self$mountPoint,
                         '-authToken', authToken,
                         '-cacheDir', .cacheDir,
                         '-cacheSize', cacheSize) ## check project id for list
        }
        if(sudo){
            cmd <- paste("sudo", cmd)
        }
        ## system2(cmd, stdout = stdout, stderr = stderr)
        message(cmd)
        system(cmd, wait = FALSE, ignore.stdout = ignore.stdout, ...)

    },
    unmount = function(mountCmd = NULL, projectID = NULL, ...){
        'unmount a project if projectID is provided, otherwise unmount all'
        

        if(is.null(mountCmd)){
            if(Sys.info()['sysname'] == "Linux"){
                OS <- "linux"            
            } else if(Sys.info()['sysname'] != "Linux" &&
                      .Platform$OS.type == "unix"){
                OS <- "mac"
            } else {
                stop("please provide a working umount command interface")
            }
        }

        ## switching un-mount command line
        mountCmd <- switch(OS,
                           mac = "umount",
                           linux = "fusermount -u")
        if(!is.null(projectID)){
            ## check if this is correct
            mountPoint <<- file.path(mountPoint, "Projects", projectID)
        }
        cmd <- paste(mountCmd, mountPoint)
        message(cmd)
        system(cmd, ...)
    },
    projects = function(id = NULL){
        pids <- list.files(file.path(mountPoint, "Projects"))
        if(is.null(id))
            pids <- pids
        pids
    },
    files = function(id = NULL){
        'given project id, show all files in it'
        if(is.null(id)){
            if(is.null(lastProjectID))
                stop("nothing mounted yet")
            message("id not provided, show files in project", lastProjectId)
            list.files(file.path(mountPoint, "Projects", lastProjectId))  
        }else{
            list.files(file.path(mountPoint, "Projects", id))  
        }
    },
    path = function(id = NULL){
        'list path for all mounted projects, for easy copy/paste of file path; if project id
         is provoded, show project path and files path'
        
        if(is.null(id)){
            list.dirs(file.path(mountPoint, "Projects"), recursive = FALSE)
        }else{
            file.path(mountPoint, "Projects", id)
            ## list.files("file.path")
        }
    }
)

setClassUnion("FSORNULL", c("FS", "NULL"))
