##' Class Item
##'
##' Class Item
##'
##' To describe a set of objects, Project, Task, Pipeline, File etc.
##'
##' @field response save the raw response from a request.
##' @field auth_token propagate the auth_token from parent.
##' @field href api href
Item <- setRefClass("Item", fields = list(response = "ANY",
                                href = "characterORNULL",
                                auth = "AuthORNULL"))





#' Get raw response from an Item object
#'
#' Get raw response from an Item object
#'
#' @param x object that may have response.
#' @param value value to be replaced.
#'
#' @return a raw response from httr
#'
#' @export
#' @docType methods
#' @rdname response-methods
#' @examples
#' \dontrun{
#' response(x)
#' }
setGeneric("response", function(x) standardGeneric("response"))

#' @export
#' @docType methods
#' @rdname response-methods
setGeneric("response<-", function(x, value)
			standardGeneric("response<-"))


#' @rdname response-methods
#' @aliases response,ANY-method
setMethod("response", "ANY", function(x){
    attr(x, "response")
})

#' @rdname response-methods
#' @aliases response<-,ANY-method
setReplaceMethod("response", "ANY", function(x, value) {
    attr(x, "response") <- value
    x    
})

#' @rdname response-methods
#' @aliases response,Item-method
setMethod("response", "Item", function(x){
    x$response
})

#' @rdname response-methods
#' @aliases response<-,Item-method
setReplaceMethod("response", "Item", function(x, value) {
    x$response <- value
    x    
})


#' @rdname response-methods
#' @aliases response,SimpleList-method
setMethod("response", "SimpleList", function(x){
    x@response
})

#' @rdname response-methods
#' @aliases response<-,SimpleList-method
setReplaceMethod("response", "SimpleList", function(x, value) {
    x@response <- value
    x    
})

