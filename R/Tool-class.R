

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

SBG <- setRefClass("SBG", fields = list(
                              "sbg:homepage" = "characterORNULL", 
                              "sbg:validationErrors" = "characterORNULL",
                              "sbg:sbgMaintained" = "logicalORNULL",
                              "sbg:latestRevision" = "integerORNULL",
                              "sbg:job" = "ANY",
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
                              "sbg:revisionInfo" = "ANY",
                              "sbg:toolkit" = "characterORNULL"),
                   methods = list(initialize = function(homepage = character(), 
                                      validationErrors = character(),
                                      sbgMaintained = FALSE,
                                      latestRevision = integer(),
                                      job = list(),
                                      toolAuthor = character(),
                                      copyOf = character(), 
                                      createdOn = integer(), 
                                      categories = character(), 
                                      contributors = character(), 
                                      links = character(), 
                                      project = character(), 
                                      createdBy = character(), 
                                      toolkitVersion = character(), 
                                      id  = character(), 
                                      license = character(), 
                                      revision = character(), 
                                      cmdPreview = character(), 
                                      modifiedOn = integer(), 
                                      modifiedBy = character(), 
                                      revisionInfo = character(), 
                                      toolkit = character()){

                       args <- mget(names(formals()),sys.frame(sys.nframe()))

                       nms <- names(args)

                       for(nm in nms){
                           .self$field(paste0("sbg:", nm), args[[nm]])                           
                       }

                   },
                       show = function(...){
                           .showFields(.self, values = .sbg.items, ...)
                       }
                                  ))


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
#' @export CPURequirement
#' @exportClass CPURequirement
#' @aliases CPURequirement CPURequirement-class
#' @examples
#' CPURequirement(value = 1L)
CPURequirement <-
    setRefClass("CPURequirement", contains = "ProcessRequirement",
                fields = list(
                    value = "integer"
                ),
                methods = list(
                    initialize = function(..., value = 1L,
                        class = "CPURequirement"){
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




#' @rdname requirements
#' @aliases MemRequirement MemRequirement-class
#' @export MemRequirement
#' @exportClass MemRequirement
#' @examples
#' MemRequirement(value = 2000L)
MemRequirement <-
    setRefClass("MemRequirement", contains = "ProcessRequirement",
                fields = list(
                    value = "integer"
                ),
                methods = list(
                    initialize = function(..., value = 1000L,
                                          class = "MemRequirement"){
                        value <<- as.integer(value)
                        class <<- class
                        callSuper(...)
                    }
                ))






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
#' @section other fields:
#' \describe{
#' \item{\code{cpu}}{cpu 0 or 1, for any value >1 will be converted to 1L, passed to CPURequirement}
#' \item{\code{mem}}{Positive integer. Passed to MemRequirement.}
#' \item{\code{dockerPull}}{[character] Get a Docker image using
#' docker pull}
#' 
#' \item{\code{dockerLoad}}{[character] Specify a HTTP URL from which
#' to download a Docker image using docker load.}
#' 
#' \item{\code{dockerFile}}{[character] Supply the contents of a
#' Dockerfile which will be build using docker build.}
#' 
#' \item{\code{dockerImageId}}{[character] The image id that will be
#' used for docker run. May be a human-readable image name or the
#' image identifier hash. May be skipped if dockerPull is specified,
#' in which case the dockerPull image id will be used.}
#' 
#' \item{\code{dockerOutputDirectory}}{ [character] Set the designated
#' output directory to a specific location inside the Docker
#' container.}}
#' 
#' @import methods
#' @importFrom docopt docopt
#' @export Tool
#' @exportClass Tool
Tool <-
    setRefClass("Tool",
                contains = c("CommandLineTool", "SBG", "App"),
                fields = list(context = "character",
                    owner = "list",
                    contributor = "list"),
                methods = list(
                    initialize = function(...,
                        inputs = NULL,
                        outputs = NULL,
                        cpu = 1L, mem = 1000L,                        
                        dockerImageId = "",
                        dockerPull = "",
                        dockerLoad = "",
                        dockerFile = "",
                        dockerOut = "",
                        requirements = NULL,
                        context = "https://github.com/common-workflow-language/common-workflow-language/blob/draft-1/specification/tool-description.md"){


                        

                        stopifnot(is.numeric(cpu))
                        .v <- as.integer(cpu)
                        if(!.v %in% c(1L, 0L)){
                            warning("For now, CPU value must be 0L (multi-treads) or 1L (single-thread)")
                            if(.v > 0){
                                message("Convert CPU value ", .v, " to", 1L)
                                .v <- 1L
                            }
                        }
                        

                        if(is.null(requirements)){
                            requirements <<-
                                ProcessRequirementList(
                                    list(DockerRequirement(
                                        dockerImageId = dockerImageId,
                                        dockerPull = dockerPull,
                                        dockerLoad = dockerLoad,
                                        dockerFile = dockerFile,
                                        dockerOutputDirectory = dockerOut),
                                         CPURequirement(value = .v),
                                         MemRequirement(value = as.integer(mem))))
                        }
                        context <<- context


                        ## inputs
                        stopifnot(is(inputs, "InputParameterList") ||
                                  (is.list(inputs) &&
                                       all(sapply(inputs, is, "InputParameter"))))
                        
                        if(is.list(inputs) &&
                           all(sapply(inputs, is, "InputParameter"))){
                            inputs <<- IPList(inputs)
                        }

                        if(is(inputs, "InputParameterList")){
                            inputs <<- inputs
                        }

                        ## outputs
                        stopifnot(is(outputs, "OutputParameterList") ||
                                  (is.list(outputs) &&
                                       all(sapply(outputs, is, "OutputParameter"))))
                        
                        if(is.list(outputs) &&
                           all(sapply(outputs, is, "OutputParameter"))){
                            outputs <<- OPList(outputs)
                        }

                        if(is(outputs, "OutputParameterList")){
                            
                        }
                        
                        
                        callSuper(...)
                    }
                ))

## override toJSON and to YAML
Tool$methods(toList = function(...){
    res <- callSuper(...)
    names(res)[which(names(res) == "context")] <- "@context"
    res
})
