## SBG extesion

.sbg.items <- c("sbg:homepage" , 
                "sbg:validationErrors" ,
                "sbg:sbgMaintained" ,
                "sbg:latestRevision" ,
                "sbg:job",
                "sbg:toolAuthor" ,
                "sbg:copyOf" ,
                "sbg:createdOn" ,
                "sbg:categories" ,
                "sbg:contributors" ,
                "sbg:links" ,
                "sbg:project" ,
                "sbg:createdBy" ,
                "sbg:toolkitVersion" ,
                "sbg:id"  , 
                "sbg:license" ,
                "sbg:revision" ,
                "sbg:cmdPreview" ,
                "sbg:modifiedOn" ,
                "sbg:modifiedBy" ,
                "sbg:revisionInfo" ,
                "sbg:toolkit" )

.sbg.fld <- gsub("sbg:", "", .sbg.items)

SBG <- setRefClass("SBG", contains  = "CWL", fields = list(
                              "sbg:homepage" = "characterORNULL", 
                              "sbg:validationErrors" = "listORNULL",
                              "sbg:sbgMaintained" = "logicalORNULL",
                              "sbg:latestRevision" = "integerORNULL",
                              "sbg:job" = "listORNULL",
                              "sbg:toolAuthor" = "characterORNULL",
                              "sbg:copyOf" = "characterORNULL",
                              "sbg:createdOn" = "integerORNULL",
                              "sbg:categories" = "characterORNULL",
                              "sbg:contributors" = "characterORNULL",
                              "sbg:links" = "characterORNULL",
                              "sbg:project" = "characterORNULL",
                              "sbg:createdBy" = "characterORNULL",
                              "sbg:toolkitVersion" = "characterORNULL",
                              "sbg:id"  = "characterORNULL", 
                              "sbg:license" = "characterORNULL",
                              "sbg:revision" = "characterORNULL",
                              "sbg:cmdPreview" = "characterORNULL",
                              "sbg:modifiedOn" = "integerORNULL",
                              "sbg:modifiedBy" = "characterORNULL", 
                              "sbg:revisionInfo" = "listORNULL",
                              "sbg:toolkit" = "characterORNULL"),
                   methods = list(initialize = function(homepage = NULL, 
                                      validationErrors = NULL,
                                      sbgMaintained = NULL,
                                      latestRevision = NULL,
                                      job = NULL, 
                                      toolAuthor = NULL,
                                      copyOf = NULL, 
                                      createdOn = NULL, 
                                      categories = NULL, 
                                      contributors = NULL, 
                                      links = NULL, 
                                      project = NULL, 
                                      createdBy = NULL, 
                                      toolkitVersion = NULL, 
                                      id  = NULL, 
                                      license = NULL, 
                                      revision = NULL, 
                                      cmdPreview = NULL, 
                                      modifiedOn = NULL, 
                                      modifiedBy = NULL, 
                                      revisionInfo = NULL, 
                                      toolkit = NULL, ...){

                       args <- mget(names(formals()),sys.frame(sys.nframe()))

                       nms <- names(args)

                       for(nm in nms){
                           .self$field(paste0("sbg:", nm), args[[nm]])                           
                       }

                       callSuper(...)

                   }))



#' Rabix specifc Requirements
#'
#' Extends ProcessRequirements. CPURequirement and MemRequirement to
#' setup CPU and Memory requiremnts.
#'
#' @field value [Integer] for CPU default is 1L, if 0L, use all
#' CPU. For mem, default is 1000L. Note: for CPU, 0L means
#' multi-tread, and non-zero value will be converted to 1L, which
#' means single thread.
#'
#' @rdname requirements
#'
#' @export CPURequirement cpu
#' @exportClass CPURequirement
#' @aliases CPURequirement CPURequirement-class cpu
#' @examples
#' cpu(1)
#' CPURequirement(value = 1L)
CPURequirement <-
    setRefClass("CPURequirement", contains = "ProcessRequirement",
                fields = list(
                    value = "integer"
                ),
                methods = list(
                    initialize = function(value = 1L,
                        class = "sbg:CPURequirement", ...){
                        class <<- class
                        stopifnot(is.numeric(value))
                        .v <- as.integer(value)
                        if(!.v %in% c(1L, 0L)){
                            warning("For now, CPU value must be 0L (multi-treads) or 1L (single-thread)")
                            if(.v > 0){
                                message("Convert CPU value ", .v, " to", 1L)
                                .v <- 1L
                            }
                        }
                        value <<- .v
                        callSuper(...)
                    }
                ))


cpu <- CPURequirement

#' @rdname requirements
#' @aliases docker
#' @param pull Docker Repository[:Tag] like rocker/r-base
#' @param imageId The image id that will be used for docker run, imageId Optionally set the id of image you get from SDK
#' @param load Specify a HTTP URL from which to download a Docker image using docker load
#' @param file Supply the contents of a Dockerfile which will be built using docker build.
#' @param output Set the designated output directory to a specific location inside the Docker container
#' @param ... extra aguments passed
#' @export docker
#' @examples
#' docker("rocker/r-base")
docker <- function(pull = "", imageId = "", load = "", 
                   file = "", output = "", ...){
    DockerRequirement(
        dockerImageId = imageId,
        dockerPull = pull,
        dockerLoad = load,
        dockerFile = file,
        dockerOutputDirectory = output, ...)    
}

#' requirements and hints
#'
#' requirements and hints
#'
#' @export requirements
#' @examples
#' requirements(docker("rocker/r-base"), cpu(1), mem(1024))
requirements <- function(...){
    listData <- .dotargsAsList(...)
    ## process
    listData <- lapply(listData, function(x){
        if(is.list(x)){
            if(all(sapply(x, is, "FileDef"))){
                return(CreateFileRequirement(fileDef = FileDefList(x)))
            }else{
                stop("not all FileDefList are FileDef object")
            }
        }else if(is(x, "FileDef")){
            return(CreateFileRequirement(fileDef = FileDefList(x)))
        }else{
            return(x)
        }
    })
    ## validation
    idx <- sapply(listData, function(x){
        is(x, "ProcessRequirement")
    })
    if(!all(idx)){
        print(listData[!idx])
        stop("Has to be ProcessRequirement class, use docker(), cpu(), mem(), fileDef(), function to help")
    }
    ProcessRequirementList(listData)
}

#' @param name file name
#' @param content file content, could be script
#' 
#' @rdname requirements
#' @aliases fileDef
#' @export fileDef
fileDef <- function(name = NULL, content = NULL){
    stopifnot(!is.null(name) && !is.null(content))
    FileDef(filename = name, fileContent = content)
}


#' @rdname requirements
#' @aliases MemRequirement MemRequirement-class mem
#' @export MemRequirement mem
#' @exportClass MemRequirement
#' @examples
#' mem(2000)
#' MemRequirement(value = 2000L)
MemRequirement <-
    setRefClass("MemRequirement", contains = "ProcessRequirement",
                fields = list(
                    value = "integer"
                ),
                methods = list(
                    initialize = function(value = 1000L,
                                          class = "sbg:MemRequirement", ...){
                        value <<- as.integer(value)
                        class <<- class
                        callSuper(...)
                    }
                ))

mem <- MemRequirement





## Tool

#' Rabix CommandLineTool Class
#'
#' Rabix subclass for CommandLineTool used by rabix.org or sbg
#' platform. \code{Tool} class extends \code{CommandLineTool}
#' with more fields.
#'
#' 
#' @field context [character] by default:
#' "https://github.com/common-workflow-language/common-workflow-language/blob/draft-1/specification/tool-description.md"
#' @field owner [list] a list of owner names. 
#' @field contributor [list] a list of contributor names.
#'
#' @export Tool
#' @exportClass Tool
Tool <-
    setRefClass("Tool",
                contains = c("CommandLineTool", "SBG"),
                fields = list(context = "character"),
                methods = list(
                    initialize = function(...,
                        id = NULL, 
                        label = NULL, 
                        inputs = NULL, 
                        outputs = NULL){

                        stopifnot(!is.null(id))
                        id <<- id
                        stopifnot(!is.null(label))
                        label <<- label

                        if(is(inputs, "InputParameterList") ||
                           (is.list(inputs) &&
                                all(sapply(inputs, is, "InputParameter")))){

                            if(is.list(inputs) &&
                               all(sapply(inputs, is, "InputParameter"))){
                                inputs <<- IPList(inputs)
                            }

                            if(is(inputs, "InputParameterList")){
                                inputs <<- inputs
                            }
                        }else if(is.data.frame(inputs)){
                            lst <- lapply(1:nrow(inputs), function(i){
                                para <- as.list(inputs[i, ])
                                lst <- lapply(para, function(x){
                                    
                                    if(is.factor(x)){
                                        return(as.character(x))
                                    }else{
                                        return(x)
                                    }
                                })
                                do.call(input, lst)
                            })
                            inputs <<- IPList(lst)
                        }else if(is(inputs, "InputParameter")){
                            inputs <<- IPList(inputs)
                        }else if(is.null(inputs)){
                            inputs <<- IPList()
                        }else{
                            stop("wrong inputs type")
                        }

                        ## outputs
                        if(is(outputs, "OutputParameterList") ||
                           (is.list(outputs) &&
                                all(sapply(outputs, is, "OutputParameter")))){

                            if(is.list(outputs) &&
                               all(sapply(outputs, is, "OutputParameter"))){
                                outputs <<- OPList(outputs)
                            }

                            if(is(outputs, "OutputParameterList")){
                                outputs <<- outputs
                            }

                        }else if(is.data.frame(outputs)){

                            lst <- lapply(1:nrow(outputs), function(i){
                                para <- as.list(outputs[i, ])
                                lst <- lapply(para, function(x){
                                    if(is.factor(x)){
                                        return(as.character(x))
                                    }else{
                                        return(x)
                                    }
                                })
                                do.call(output, lst)
                            })
                            outputs <<- OPList(lst)
                            
                        }else if(is(outputs, "OutputParameter")){
                            outputs <<- OPList(outputs)
                        }else if(is.null(outputs)){
                            outputs <<- OPList()
                        }else{
                            stop("wrong output")
                        }                       
                        
                        
                        callSuper(...)
                    }
                ))

