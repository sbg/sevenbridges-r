#' Build workflow
#'
#' Build workflow
#'
#' @rdname Flow
#' @export SBGWorkflow
#' @exportClass SBGWorkflow
#' @aliases SBGWorkflow
SBGWorkflow <- setRefClass("SBGWorkflow", contains = c("Workflow", "SBG"),
                           fields = list(
                               "sbg:update" = "characterORNULL",
                        "sbg:canvas_zoom" = "numericORNULL",
                        "sbg:canvas_y" =  "numericORNULL",
                        "sbg:canvas_x" = "numericORNULL"
                    ),
                    methods = list(
                        initialize = function(id = NULL,
                            label = NULL,
                            canvas_zoom = 1,
                            canvas_y = NULL,
                            canvas_x = NULL,
                            steps = list(),
                            includeAll = TRUE,
                            update = NULL,
                            ...){

                            stopifnot(!is.null(id))
                            
                            stopifnot(!is.null(label))

                            args <- mget(names(formals()),
                                         sys.frame(sys.nframe()))
                            nms <- c("canvas_x", "canvas_y", "canvas_zoom", "update")
                            for(nm in nms){
                                   .self$field(paste0("sbg:", nm), args[[nm]])
                               }
                               steps <<- steps

                               ## first get connected output list
                               out.id <- unlist(sapply(steps, function(s){
                                   if(length(s$inputs)){
                                       sapply(s$inputs, function(i){
                                           res <- unlist(i$source)
                                           if(length(res)){
                                               return(res)
                                           }else{
                                               return(NULL)
                                           }
                                       })}else{
                                           return(NULL)
                                       }
                               }))

                               
                              
                               
                               ## get all output full id
                               out.all <- sapply(steps, function(s){
                                   getOutputId(s$run)
                               })
                               

                               ## rest output auto connet to inlcude port
                               out.include <- setdiff(unlist(out.all), out.id)
                          
                               if(length(out.include)){
                                   oi <- sapply(out.include, function(x){
                                       res <- unlist(strsplit(x, "[.]"))
                                       paste0("#", res[length(res)])
                                   })
                                   ol <- getOutputById(steps, oi)
        
                                   sol <- as(ol, "SBGWorkflowOutputParameterList")
                                   lst <- lapply(sol, function(ol){
                                       .id <- ol$id 
                                       nm <- names(oi[oi == .id])
                                       ol$source <- list(nm)
                                       ol
                                   })
                                   sol <- do.call("SBGWorkflowOutputParameterList",
                                                  lst)
                                   if(includeAll){
                                       outputs <<- sol
                                   }else{
                                       ## only last one?
                                       outputs <<- sol[length(sol)]
                                   }
                               }
                               callSuper(id = id, label = label, ...)
                        }
                    ))

#' @rdname Flow
#' @export Flow
#' @aliases Flow
Flow <- function(..., graph = TRUE,
                 x.width = 1000, 
                 y.width = 400,
                 x.start = 100,
                 y.start = 200,
                 canvas_zoom = 1,
                 canvas_x = 40,
                 canvas_y = 130){
    obj <- SBGWorkflow(...)
    if(graph){
        obj <- addGraph(obj, x.width = x.width,
                        y.width = y.width,
                        x.start = x.start,
                        y.start = y.start,
                        canvas_zoom = canvas_zoom,
                        canvas_x = canvas_x,
                        canvas_y = canvas_y)    
    }
    obj
}

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",Tool,Tool-method
setMethod("+", c("Tool", "Tool"), function(e1, e2){
    ## find the x1
    o <- getOutputType(e1)
    i <- getInputType(e2)
    idx <- match(o, i)
    
    if(!all(is.na(idx))){
        if(sum(!is.na(idx)) == 1){
            ## return a Workflow
            .out.id <- getOutputId(e1)[[which(!is.na(idx))]]
            .in.id <- getInputId(e2)[[idx[!is.na(idx)]]]
            ## insert int id
            steplst <- SBGStepList(SBGStep(id = getId(e1), 
                                           run = e1,
                                           outputs = WorkflowStepOutputList(
                                               WorkflowStepOutput(id = .out.id)
                                           )),
                                   SBGStep(id = getId(e2),
                                           run = e2,
                                           inputs = WorkflowStepInputList(
                                               WorkflowStepInput(id = .in.id,
                                                                 source = c(.out.id))
                                           )))
            ## make rest inports
            return(steplst)
        }else{
            stop("multiple matching found, no sure which one to connect")
        }
    }else{
        stop("no input match ouput types")
    }
    
    
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",WorkflowStepList,Tool-method
setMethod("+", c("WorkflowStepList", "Tool"), function(e1, e2){
    t.last <- e1[[length(e1)]]$run
    t.new <- (t.last + e2)[2]
    do.call(SBGStepList, c(as.list(e1), as.list(t.new)))
    
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",WorkflowStepList,WorkflowStepList-method
setMethod("+", c("WorkflowStepList", "WorkflowStepList"), function(e1, e2){
    do.call("WorkflowStepList", c(as.list(e1), as.list(e2)))
})


#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",App,App-method
setMethod("+", c("App", "App"), function(e1, e2){
    convertApp(e1) + convertApp(e2)
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",WorkflowStepList,App-method
setMethod("+", c("WorkflowStepList", "App"), function(e1, e2){
    t.last <- e1[[length(e1)]]$run
    t.new <- (t.last + convertApp(e2))[2]
    do.call(SBGStepList, c(as.list(e1), as.list(t.new)))
})




#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>%"
#' @aliases "%>%",Tool,Tool-method
setGeneric("%>%", function(e1, e2) standardGeneric("%>%"))

setMethod("%>%", c("Tool", "Tool"), function(e1, e2){
    .id <- paste(parseLabel(e1$label), parseLabel(e2$label), sep = "_")
    .label <- paste(parseLabel(e1$label), parseLabel(e2$label))
    lst <- e1 + e2
    Flow(id = .id, label = .label, steps = lst)
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>%",Workflow,Tool-method
setMethod("%>%", c("Workflow", "Tool"), function(e1, e2){
    e1$steps <- e1$steps + e2
    e1
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>%",Workflow,Workflow-method
setMethod("%>%", c("Workflow", "Workflow"), function(e1, e2){
    e1$steps <- e1$steps + e2$steps
    e1
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>%",App,App-method
setMethod("%>%", c("App", "App"), function(e1, e2){
    convertApp(e1) %>% convertApp(e2)
})


#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>%",Workflow,App-method
setMethod("%>%", c("Workflow", "App"), function(e1, e2){
    e1$steps <- e1$steps + convertApp(e2)
    e1
})

## assign coordinates
## add output port

## coersion
setAs("CommandOutputParameter", "WorkflowOutputParameter", 
      function(from){
          WorkflowOutputParameter(
              type = from$type, 
              label = from$label,
              description = from$description,
              streamable = from$streamable,
              default = from$default,
              id = from$id
          )
      })

setAs("CommandOutputParameter", "SBGWorkflowOutputParameter", 
       function(from){
          SBGWorkflowOutputParameter(
              type = from$type, 
              label = from$label,
              description = from$description,
              streamable = from$streamable,
              default = from$default,
              id = from$id
          )
})

setAs("OutputParameterList", "WorkflowOutputParameterList", 
      function(from){
          lst <- lapply(from, function(x) as(x, "WorkflowOutputParameter"))
          do.call("WorkflowOutputParameterList", lst)
      })

setAs("OutputParameterList", "SBGWorkflowOutputParameterList", 
      function(from){
          lst <- lapply(from, function(x) as(x, "SBGWorkflowOutputParameter"))
          do.call("SBGWorkflowOutputParameterList", lst)
      })


getOutputById <- function(sl, id){
    lst <- lapply(sl, function(s){
        ol <- s$run$outputs

        lapply(ol, function(o){
            if(o$id %in% id){
                return(o)
            }else{
                return(NULL)
            }
        })
    })

    lst <- do.call(c, lst)
    lst <- lst[!sapply(lst, is.null)]
    do.call("OutputParameterList", lst)
}


## asign step list coordinates
setGeneric("addGraph", function(obj, ...) standardGeneric("addGraph"))


setMethod("addGraph", "SBGStepList", function(obj, 
                                              x.width = 1000, 
                                              y.width = 400,
                                              x.start = 100,
                                              y.start = 200){
    N <- length(obj)
    x.step <- x.width/(N+1)
    ## x from 100
    ## y from 200
    for(i in 1:N){
        obj[[i]]$field("sbg:x", x.start + x.step * (i - 1))
        obj[[i]]$field("sbg:y", y.start)
    }
    obj
})

setMethod("addGraph", "SBGWorkflow", function(obj, 
                                              x.width = 1000, 
                                              y.width = 400,
                                              x.start = 100,
                                              y.start = 200,
                                              canvas_zoom = 1,
                                              canvas_x = 40,
                                              canvas_y = 130){
  
    obj$steps <- addGraph(obj$steps)
    slst <- obj$steps
    x.step <- x.width/(length(slst)+1)
    os <- obj$outputs
    N <- length(os)
    y.step <- y.width/(N + 1)
    for(i in 1:N){
        obj$outputs[[i]]$field("sbg:x", x.width - x.step)
        obj$outputs[[i]]$field("sbg:y", y.start + y.step * (i-1))
    }
    obj$field("sbg:canvas_zoom", canvas_zoom)
    obj$field("sbg:canvas_x", canvas_x)
    obj$field("sbg:canvas_y", canvas_y)
    obj
})


## transfer app


