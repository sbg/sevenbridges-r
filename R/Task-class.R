.ts <- c("id", "name", "description", "status", "app", "type",
         "created_by", "executed_by", "start_time", "end_time",
         "execution_status", "price", "inputs", "outputs", "project",
         "batch", "batch_input", "batch_by",  "parent", "batch_group",
         "errors", "warnings")

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
                                  batch = "logicalORNULL",
                                  batch_input = "characterORNULL",
                                  batch_by = "listORNULL",
                                  parent = "characterORNULL",
                                  batch_group = "listORNULL",
                                  errors = "listORNULL",
                                  warnings = "listORNULL"),

                    methods = list(
                        # initialize = function(execution_status = NULL, ...){
                        #     if(!is.null(execution_status)){
                        #         .self$execution_status <<- do.call(EStatus, execution_status)
                        #     }
                        #     callSuper(...)
                        # },
                        update = function(name = NULL,
                                          description = NULL,
                                          inputs = NULL, ...){

                            if(is.null(name) && is.null(description) && !is.null(inputs)){

                                res = auth$api(path = paste0("tasks/", id, "/inputs"),
                                               body = inputs, method = "PATCH", ...)
                                return(update())
                            }


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
                        get_input = function(...){
                            getInputs(...)
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
                                if(!is.logical(res) || isTRUE(res)){
                                    break
                                }
                                Sys.sleep(time)
                            }
                        },
                        file = function(...){
                            auth$file(project = project, origin.task = id, ...)
                        },
                        download = function(destfile, ..., method = "curl"){
                            if(is.null(outputs)){
                                update()
                            }
                            fids <- sapply(outputs, function(x) x$path)
                            p <- auth$project(id = project)

                            for(fid in fids){
                                fl <- p$file(id = fid)
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
                    cat("\r", "completed")
                    return(TRUE)
                }
            }

            if(is.null(queued)){
                queued <<- function(...){
                    cat("\r", "queued")
                    return(FALSE)
                }
            }

            if(is.null(draft)){
                draft <<- function(....){
                    ## should not happen in a running task
                    message("draft")
                    return(FALSE)
                }
            }

            if(is.null(running)){
                running <<- function(...){
                    cat("\r", "running ...")
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
                    cat("\r", "failed")
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
#' @param fun function it must return a TRUE or FALSE in the end of function body, when it's
#' TRUE this function will also terminate monitor process, if FALSE, function called, but not going
#' to terminate task monitoring process.
#'
#' @rdname TaskHook
#' @return object from setHook and getHook.
#' @export setTaskHook
#' @examples
#' getTaskHook("completed")
#' setTaskHook("completed", function(){
#'     message("completed")
#'     return(TRUE)
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


setGeneric("asTaskInput", function(object) standardGeneric("asTaskInput"))
setMethod("asTaskInput", "Files", function(object){
    list(class = unbox("File"),
         path = unbox(object$id),
         name = unbox(object$name))
})


setMethod("asTaskInput", "FilesList", function(object){
    lapply(object, function(x){
        asTaskInput(x)
    })
})

setMethod("asTaskInput", "list", function(object){

    id.file <- sapply(object, is, "Files")
    id.lst <- sapply(object, is, "FilesList")
    if(sum(id.file)){
        res.f <- object[id.file]
    }else{
        res.f <- NULL
    }
    if(sum(id.lst)){

        res.l <- object[id.lst]
        res.l <- do.call(c, lapply(object[id.lst], function(x){
            # x here is FilesList
            lapply(x, function(x) x)
            # return a pure list
        }))
    }else{
        res.l <- NULL
    }
    res <- c(res.f, res.l)
    if(length(res)){
        return(asTaskInput(FilesList(res)))
    }else{
        stop("Not every list entries are Files or FilesList object")
    }

})

setMethod("asTaskInput", "ANY", function(object){
    object
})



#' batch function for task batch execution
#'
#' batch function for task batch execution
#'
#' @param input character, ID of the input on which you wish to batch on.
#' You would usually batch on the input containing a list of files.
#' If left out, default batching criteria defined in the app is used.
#' @param criteria a character vector, for example.
#' \code{c("metadata.sample_id", "metadata.library_id")}. The meaning of the
#' above batch_by dictionary is - group inputs (usually files) first on sample ID
#' and then on library ID. If NULL, using type "ITEM" by default.
#' @param type Criteria on which to batch on - can be in two formats."ITEM" and
#' "CRITERIA". If you wish to batch per item in the input (usually a file)
#' using "ITEM". If you wish a more complex criteria, specify the "CRITERIA"
#' on which you wish to group inputs on. Please check examples.
#' @return a list of 'batch_input' and 'batch_by' used for task batch
#' @export batch
#' @examples
#' batch(input = "fastq") # by ITEM
#' batch(input = "fastq", c("metadata.sample_id", "metadata.library_id"))
#' ## shorthand for this
#' batch(input = "fastq", c("metadata.sample_id", "metadata.library_id"), type = "CRITERIA")
batch = function(input = NULL,
                 criteria = NULL,
                 type = c("ITEM", "CRITERIA")){

    if(is.null(input)){
        stop("Please specify the input id")
    }
    type = match.arg(type)
    if(is.null(criteria)){
        if(type == "CRITERIA"){
            stop("Please provide cretieria, for example c(\"metadata.sample_id\")")
        }
    }else{
        if(type == "ITEM"){
            message("criteria provided, convert type from ITEM to CRITERIA")
        }
        type = "CRITERIA"
    }

    if(length(criteria) == 1){
        criteria = list(criteria)
    }
    switch(type,
           ITEM = {
               res = list(type = "ITEM")
           },
           CRITERIA = {
               if(is.null(criteria)){

               }else{
                   res = list(
                       type ="CRITERIA",
                       criteria = criteria
                   )
               }
           })
    c(list(batch_input = input), list(batch_by = res))
}
