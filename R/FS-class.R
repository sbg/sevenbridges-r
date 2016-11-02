#' FS class
#'
#' FS class
#'
#' @param server_address placehoder
#' @param api_address placehoder
#' @param vsfs_jar placehoder
#' @param cache_dir placehoder
#' @param cache_size placeholder
#' @param project_id placeholder
#'
#' @importFrom uuid UUIDgenerate
#'
#' @export FS
FS <- setRefClass("FS",
                    fields = list(mount_point = "characterORNULL",
                        mode = "characterORNULL",
                        debug = "logicalORNULL",
                        cache_dir = "characterORNULL",
                        cache_size = "characterORNULL",
                        api_address = "characterORNULL",
                        token = "characterORNULL",
                        project_id = "listORNULL",
                        lastproject_id = "characterORNULL",
                        secure = "logicalORNULL",
                        server_address = "characterORNULL",
                        vsfs_jar = "characterORNULL"),
                    methods = list(
                        initialize = function(server_address = 'fs.sbgenomics.com',
                            api_address ='https://api.sbgenomics.com',
                            vsfs_jar = NULL,
                            cache_dir = '~/vsfs_cache',
                            cache_size = '10GB',
                            project_id = list(),
                            ...){

                            if(is.null(vsfs_jar)){
                                ## vsfs_jar <<- system.file("java", "sbg-vsfs.jar", package = "vsfsr")
                                stop("please provie jar library")
                            }else{
                                vsfs_jar <<- vsfs_jar
                            }

                            server_address <<- server_address
                            api_address <<- api_address

                            cache_dir <<- cache_dir
                            cache_size <<- cache_size
                            ## db <<- db
                            ## icoll <<- icoll

                            ## check project_id, has to be integer if any
                            if(length(project_id)){
                                if(!is.list(project_id)){
                                    if(length(project_id) > 1 && is.numeric(project_id)){
                                        .project_id <- as.list(project_id)
                                    }else{
                                        .project_id <- list(project_id)
                                    }

                                }else{
                                    .project_id <- project_id
                                }
                                .project_id <- lapply(.project_id, function(id){
                                    if(is.numeric(id) && !is.integer(id)){
                                        id <- as.integer(id)
                                        message("Converting numerical value to integer")
                                    }
                                    stopifnot(is.integer(id))
                                    id

                                })
                                project_id <<- .project_id

                            }

                            lastproject_id <<- unlist(tail(project_id, n = 1))


                            callSuper(...)
                        }
                    ))



FS$methods(
    check_mount = function(){
        'check if a path is mounted'
    },
    mount = function(mount_point = NULL, project_id = NULL, ignore.stdout = TRUE, sudo = TRUE, ...){
        'mount a specific project if project_id is provided, otherwise mount all projects'
        .self$project_id <<- c(.self$project_id, list(project_id))
        lastproject_id <<- unlist(tail(project_id, n = 1))
        if(is.null(mount_point)){
            stop("mount_point not provided")
        }else{
            dir.create(normalizePath(mount_point))
            mount_point <<- normalizePath(mount_point)
        }
        message("mount")

        ## create uid
        uid <- UUIDgenerate()

        ## create cache dir at local home? is there a problem?
        .cache_dir <- file.path(cache_dir, uid)

        dir.create(.cache_dir, recursive = TRUE)

        .cache_dir <- normalizePath(.cache_dir)
        ## stdout and stderr path
        stdout <- file.path(cache_dir, paste0(uid, ".out"))
        stderr <- file.path(cache_dir, paste0(uid, ".err"))

        ## run command
        if(!is.null(project_id)){
            cmd <- paste("java", "-Xmx128m", "-jar", vsfs_jar, "-ssl",
                         "-as", api_address,
                         "-s", server_address,
                         '-mountPoint', .self$mount_point,
                         '-authToken', token,
                         '-cacheDir', .cache_dir,
                         '-cacheSize', cache_size, ## check project id for list
                         '-projectId', project_id)
        }else{
            cmd <- paste("java", "-Xmx128m", "-jar", vsfs_jar, "-ssl",
                         "-as", api_address,
                         "-s", server_address,
                         '-mountPoint', .self$mount_point,
                         '-authToken', token,
                         '-cacheDir', .cache_dir,
                         '-cacheSize', cache_size) ## check project id for list
        }
        if(sudo){
            cmd <- paste("sudo", cmd)
        }
        ## system2(cmd, stdout = stdout, stderr = stderr)
        message(cmd)
        system(cmd, wait = FALSE, ignore.stdout = ignore.stdout, ...)

    },
    unmount = function(mount_cmd = NULL, project_id = NULL, ...){
        'unmount a project if project_id is provided, otherwise unmount all'


        if(is.null(mount_cmd)){
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
        mount_cmd <- switch(OS,
                           mac = "umount",
                           linux = "fusermount -u")
        if(!is.null(project_id)){
            ## check if this is correct
            mount_point <<- file.path(mount_point, "Projects", project_id)
        }
        cmd <- paste(mount_cmd, mount_point)
        message(cmd)
        system(cmd, ...)
    },
    list_project_id = function(id = NULL){
        pids <- list.files(file.path(mount_point, "Projects"))
        if(is.null(id))
            pids <- pids
        pids
    },
    file = function(id = NULL){
        'given project id, show all files in it'
        if(is.null(id)){
            if(is.null(lastproject_id))
                stop("nothing mounted yet")
            message("id not provided, show files in project", lastproject_id)
            list.files(file.path(mount_point, "Projects", lastproject_id))
        }else{
            list.files(file.path(mount_point, "Projects", id))
        }
    },
    path = function(id = NULL){
        'list path for all mounted projects, for easy copy/paste of file path; if project id
         is provoded, show project path and files path'

        if(is.null(id)){
            list.dirs(file.path(mount_point, "Projects"), recursive = FALSE)
        }else{
            file.path(mount_point, "Projects", id)
            ## list.files("file.path")
        }
    }
)

setClassUnion("FSORNULL", c("FS", "NULL"))
