.ts <- c("id", "name", "description", "status", "app", "type",
         "created_by", "executed_by", "start_time", "end_time", 
         "execution_status", "price", "inputs", "outputs", "project")

Task <- setRefClass("Task", contains = "Item",
                    fields = list(id = "characterORNULL",
                        name = "characterORNULL",
                        description = "characterORNULL",
                        status = "characterORNULL",
                        app = "characterORNULL",
                        type = "characterORNULL",
                        created_by = "characterORNULL",
                        executed_by = "characterORNULL",
                        start_time = "characterORNULL",
                        end_time = "characterORNULL",
                        execution_status = "listORNULL",
                        price = "listORNULL",
                        inputs = "listORNULL",
                        outputs = "listORNULL",
                        project = "characterORNULL",
                        message = "characterORNULL",
                        steps_completed = "numericORNULL",
                        steps_total = "numericORNULL"),
                    methods = list(
                        initialize = function(execution_status = NULL, ...){
                            if(length(execution_status)){
                                message <<- execution_status$message
                                steps_completed <<- status$jobs_completed
                                steps_total <<- status$jobs_total
                            }
                            callSuper(execution_status = execution_status, ...)
                        },
                        update = function(name = NULL,
                            description = NULL,
                            inputs = NULL, ...){

                            body = list(name = name,
                                description = description,
                                inputs = inputs)

                            if(all(sapply(body, is.null))){
                                res = auth$api(path = paste0("tasks/", id), method = "GET", ...)
                            }else{
                                res = auth$api(path = paste0("tasks/", id), body = body, method = "PATCH", ...)
                            }



                            ## update object
                            for(nm in .ts){
                                .self$field(nm, res[[nm]])
                            }

                            .asTask(res)
                            
                        },
                        getInputs = function(...){
                            auth$api(path = paste0("tasks/", id, "/inputs"),
                                     method = "GET", ...)
                        },
                        delete = function(...){
                            auth$api(path = paste0("tasks/", id),
                                     method = "DELETE", ...)
                        }, 
                        abort = function(...){
                            ## turn this into a list
                            req <- auth$api(path = paste0("tasks/", id, "/actions/abort"),
                                     method = "POST", ...)

                            ## update object
                            for(nm in .ts){
                                .self$field(nm, req[[nm]])
                            }
                            .asTask(req)

                        },
                        monitor = function(time = 30, ...){
                            ## TODO set hook function
                            ## get hook
                            t0 <- Sys.time()
                            message("Monitoring ...")
                            while(TRUE){
                                ## get status
                                d <- tolower(update()$status)
                                .fun <- getTaskHook(d)
                                res <- .fun(...)
                                if(res){
                                    break
                                }
                                Sys.sleep(time)
                            }
                        },
                        download = function(destfile, ..., method = "curl"){
                            if(is.null(outputs)){
                                update()
                            }
                            fids <- sapply(outputs, function(x) x$path)
                            p <- auth$project(id = project, exact = TRUE)

                            for(fid in fids){
                                fl <- p$file(id = fid, exact = TRUE)
                                message("downloading: ", fl$name)
                                fl$download(destfile, ..., method = method)
                            }

                        },
                        run = function(...){
                            ## turn this into a list
                            req <- auth$api(path = paste0("tasks/", id, "/actions/run"),
                                     method = "POST", ...)

                            ## update object
                            for(nm in .ts){
                                .self$field(nm, req[[nm]])
                            }
                            .asTask(req)
                        },
                        show = function(){
                            .showFields(.self, "== Task ==", .ts)
                        }
                    ))


.asTask <- function(x){
    res <- do.call(Task, x)
    res$response <- response(x)
    res
}

TaskList <- setListClass("Task", contains = "Item0")

.asTaskList <- function(x){
    obj <- TaskList(lapply(x$items, .asTask))
    obj@href <- x$href
    obj@response <- response(x)
    obj
}


### Hook
TaskHook <- setRefClass("TaskHook", fields = list(
                                        queued = "function",
                                        draft = "function",
                                        running = "function",
                                        completed = "function",
                                        aborted = "function",
                                        failed = "function"),
                        methods = list(
                            initialize = function(queued = NULL,
                                draft = NULL,
                                running = NULL,
                                completed = NULL,
                                aborted = NULL, 
                                failed = NULL, ...){

                                if(is.null(completed)){
                                    completed <<- function(...){
                                        message("completed")
                                        return(TRUE)
                                    }
                                }

                                if(is.null(queued)){
                                    queued <<- function(...){
                                        return(FALSE)
                                    }
                                }

                                if(is.null(draft)){
                                    draft <<- function(....){
                                        ## should not happen in a running task
                                        return(FALSE)
                                    }
                                }

                                if(is.null(running)){
                                    running <<- function(...){
                                        return(FALSE)
                                    }
                                }

                                if(is.null(aborted)){
                                    aborted <<- function(...){
                                        message("aborted")
                                        return(TRUE)
                                    }
                                }

                                if(is.null(failed)){
                                    failed <<- function(...){
                                        message("failed")
                                        return(TRUE)
                                    }
                                }
                                
                            },
                            setHook = function(status = c("queued", "draft", "running",
                                                   "completed", "aborted", "failed"), fun){

                                stopifnot(is.function(fun))
                                status <- match.arg(status)
                                .self$field(status, fun)
                            },
                            getHook = function(status = c("queued", "draft", "running",
                                                   "completed", "aborted", "failed")){
                                status <- match.arg(status)
                                .self[[status]]
                            }
                        ))


#' set task function hook
#'
#' set task function hoook according to
#' 
#' @param status one of ("queued", "draft", "running", "completed", "aborted", "failed")
#' @param fun function 
#' 
#' @rdname TaskHook
#' @export setTaskHook
#' @examples
#' getTaskHook("completed")
#' setHook("completed", function(){
#'     message("completed yet ...")
#' })
setTaskHook = function(status = c("queued", "draft", "running",
                       "completed", "aborted", "failed"), fun){
    status <- match.arg(status)
    stopifnot(is.function(fun))
    options("sevenbridges")$sevenbridges$taskhook$setHook(status, fun)
}

#' @rdname TaskHook
#' @export getTaskHook
getTaskHook = function(status = c("queued", "draft", "running",
                       "completed", "aborted", "failed")){
    status <- match.arg(status)
    options("sevenbridges")$sevenbridges$taskhook$getHook(status)
}



#' @rdname delete-methods
#' @aliases delete,Task-method
setMethod("delete", "Task", function(obj){
    obj$delete()
})
