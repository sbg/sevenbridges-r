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
                "sbg:toolkit",
                "sbg:projectId")

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
                              "sbg:categories" = "characterORlistORNULL",
                              "sbg:contributors" = "listORNULL",
                              "sbg:links" = "listORNULL",
                                             "sbg:project" = "characterORNULL",
                                                 "sbg:projectId" = "characterORNULL",
                              "sbg:createdBy" = "characterORNULL",
                              "sbg:toolkitVersion" = "characterORNULL",
                              "sbg:id"  = "characterORNULL", 
                              "sbg:license" = "characterORNULL",
                              "sbg:revision" = "integerORNULL",
                              "sbg:cmdPreview" = "characterORNULL",
                              "sbg:modifiedOn" = "integerORNULL",
                              "sbg:modifiedBy" = "characterORNULL", 
                              "sbg:revisionsInfo" = "listORNULL",
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
                                      revisionsInfo = NULL, 
                                      toolkit = NULL, ...){

                       args <- mget(names(formals()),sys.frame(sys.nframe()))

                       nms <- names(args)

                       for(nm in nms){
                           .self$field(paste0("sbg:", nm), args[[nm]])                           
                       }

                       callSuper(...)

                   }))



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
#' @return a Tool object.
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
                        ## .self$field("sbg:id", addIdNum(id))
                        stopifnot(!is.null(label))

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
                        
                        callSuper(id = id, label  = label, ...)

                    }
                ))

