.response_app <- c("href", "id", "name", "project", "revision")
## remove "raw" from default showing methods

#' App class
#'
#' App class
#'
#' @field id app id.
#' @field project project id
#' @field name app name
#' @field revision app revision
#' @field raw raw cwl list, if doesn't have any, call cwl() method.
#' 
#' @export App
#' @return App object.
#' @aliases App
App <- setRefClass("App", contains = "Item",
                   fields = list(id = "characterORNULL",
                       project = "characterORNULL",
                       name = "characterORNULL",
                       revision = "characterORNULL",
                       raw = "ANY"),
                   methods = list(
                       copyTo = function(project = NULL, name = NULL){
                           auth$copyApp(id, project = project, name = name)
                       },
                       cwl = function(revision = NULL, ...){
                           if(!is.null(revision)){
                               .id <- .update_revision(id, revision)
                           }else{
                               .id <- id
                           }
                           if(is.null(auth)){
                               stop("auth missing")
                           }
                           raw <<- auth$api(path = paste0("apps/", .id, "/raw"),
                                            methods = "GET", ...)
                           raw
                       },
                       input = function(){
                           if(is.null(raw)){
                               stop("missing raw cwl detail, run apps$cwl()")
                           }
                           getInputType(raw)
                       },
                       output = function(){
                           if(is.null(raw)){
                               stop("missing raw cwl detail, run apps$cwl()")
                           }
                           getOutputType(raw)
                       },
                       show = function(){
                           .showFields(.self, "== App ==", .response_app)
                       }
                   ))

.asApp <- function(x){
    if(!is.null(x$revision)){
        r <- as.character(x$revision)
    }else{
        r <- x$revision
    }
    App(id = x$id,
        name = x$name,
        project = x$project,
        revision = r,
        raw = x$raw,
        response = response(x))
}

AppList <- setListClass("App", contains = "Item0")

.asAppList <- function(x){
    obj <- AppList(lapply(x$items, .asApp))
    obj@href <- x$href
    obj@response <- response(x)
    obj
}



#' @rdname Tool-class
#' @export convertApp
#' @aliases convertApp
#' @param from an App object
#' @section convertApp:
#' \describe{
#' convert App into Tool or Flow object based on class. 
#' }
convertApp <- function(from){
    if(is.null(from$raw)){
        message("cannot find raw file, pull raw cwl from internet")
        from$cwl()
    }
    obj <- from$raw    
    .convertApp(obj)
}

.convertApp <- function(obj){
    cls <- obj$class
    switch(cls, 
           "CommandLineTool" = {
               .asTool(obj)
           },
           "Workflow" = {
               .asFlow(obj)
           })
}

.asTool <- function(obj){
    ## obj should be raw list
    args.inputs <- obj$inputs
    args.outputs <- obj$outputs
    args.requirements <- obj$requirements
    args.hints <- obj$hints
    args.stdin <- obj$stdin
    args.stdout <- obj$stdout
    
    .diy <- c("inputs", "outputs", "requirements", 
              "hints", "stdin", "stdout")
    
    ## inputs
    if(length(args.inputs)){
        res.in <- input(args.inputs)
    }else{
        res.in <- IPList()
    }
    
    
    ## outputs
    if(length(args.outputs)){
        res.out <- output(args.outputs)
    }else{
        res.out <- OPList()
    }
    
    
    ## hints
    if(length(args.hints)){
        res.hints <- requirements(args.hints)
    }else{
        res.hints <- requirements()
    }
    
    
    ## requirements
    if(length(args.requirements)){
        res.req <- requirements(args.requirements)
    }else{
        res.req <- requirements()
    }
    
    ## stdin
    if(length(args.stdin)){
        if(is.character(args.stdin)){
            res.stdin <- args.stdin
        }else{
            res.stdin <- do.call(Expression, args.stdin)
        }
    }else{
        res.stdin <- NULL
    }
    
    ## stdout
    
    if(length(args.stdout)){
        if(is.character(args.stdout)){
            res.stdout <- args.stdout
        }else{
            res.stdout <- do.call(Expression, args.stdout)
        }
    }else{
        res.stdout <- NULL
    }
    
    nms <- names(obj)
    .obj.nms <- setdiff(nms, .diy)
    res <- do.call("Tool", obj[.obj.nms])
    res$field("inputs", res.in)
    res$field("outputs", res.out)
    res$field("hints", res.hints)
    res$field("requirements", res.req)
    res$field("stdin", res.stdin)
    res$field("stdout", res.stdout)
    ## for the reason you convert from App, don't add #
    res$id <- gsub("^#", "", res$id)
    res
}

.asFlow <- function(obj){
    ##
    args.inputs <- obj$inputs
    args.outputs <- obj$outputs
    args.requirements <- obj$requirements
    args.hints <- obj$hints
    
    .diy <- c("inputs", "outputs", "requirements", "hints", "steps")
    
    ## inputs
    if(length(args.inputs)){
        res.in <- input(args.inputs)
    }else{
        res.in <- IPList()
    }
    
    ## outputs
    if(length(args.outputs)){
        lst <- lapply(args.outputs, function(o){
            .t <- o$type
            lst <- lapply(.t, function(x){
                if(("type" %in% names(x)) && x$type == "array"){
                    do.call(ItemArray, x)
                }else{
                    x
                }
            })
            .type <- do.call(DSCList, lst)
            lst <- c(o[!names(o) %in% c("sbg:x", 
                                        "sbg:y", 
                                        "sbg:includeInPorts", 
                                        "type",
                                        "source")], 
                     list("x" = o$"sbg:x", 
                          "y" = o$"sbg:y", 
                          "type" = .type,
                          "source" = list(as.character(o$source)),
                          "includeInPorts" = o$"sbg:includeInPorts"))
            do.call(SBGWorkflowOutputParameter, lst)
        })
        res.out <- do.call(SBGWorkflowOutputParameterList, lst)
    }else{
        res.out <- SBGWorkflowOutputParameterList()
    }
    
    ## hints
    if(length(args.hints)){
        res.hints <- requirements(args.hints)
    }else{
        res.hints <- requirements()
    }
    
    
    ## requirements
    if(length(args.requirements)){
        res.req <- requirements(args.requirements)
    }else{
        res.req <- requirements()
    }

    ## steps
    steplst <- obj$steps
    if(length(steplst)){
        lst <- lapply(steplst, function(x){
            .convertApp(x$run)
        })
        slst <- lst[[1]]
        for(i in 1:(length(lst) -1)){
            slst <- slst + lst[[i + 1]]
        }
    }else{
        slst <- SBGStepList()
    }
    nms <- names(obj)
    .obj.nms <- setdiff(nms, .diy)
    res <- do.call("Flow", c(obj[.obj.nms],
                             list(steps = slst,
                                  inputs = res.in,
                                  outputs = res.out,
                                  hints = res.hints,
                                  requirements = res.req)))
 
    res

}


#' @rdname App-class
#' @aliases appType
#' @export appType
#' @param x a App object
#' @section appType:
#' \describe{
#'  this function return class of a App object.
#' }
appType <- function(x){
    obj <- x$raw
    if(is.null(obj)){
        x$cwl()
        obj <- x$raw
    }
    obj$class
}
