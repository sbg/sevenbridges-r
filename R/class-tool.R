# SBG extesion
.sbg.items <- c(
    "sbg:homepage" ,
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
    "sbg:revisionNotes",
    "sbg:toolkit",
    "sbg:projectId",
    "sbg:image_url",
    "sbg:appVersion",
    "sbg:projectName",
    "sbg:publisher",
    "cwlVersion",
    "appUrl",
    "x")

.sbg.fld <- gsub("sbg:", "", .sbg.items)

SBG <- setRefClass(
    "SBG", contains  = "CWL",

    fields = list(
        "sbg:homepage"            = "characterORNULL",
        "sbg:validationErrors"    = "listORNULL",
        "sbg:sbgMaintained"       = "logicalORNULL",
        "sbg:latestRevision"      = "integerORNULL",
        "sbg:job"                 = "listORNULL",
        "sbg:toolAuthor"          = "characterORNULL",
        "sbg:copyOf"              = "characterORNULL",
        "sbg:createdOn"           = "integerORNULL",
        "sbg:categories"          = "characterORlistORNULL",
        "sbg:contributors"        = "listORNULL",
        "sbg:links"               = "listORNULL",
        "sbg:project"             = "characterORNULL",
        "sbg:projectId"           = "characterORNULL",
        "sbg:createdBy"           = "characterORNULL",
        "sbg:toolkitVersion"      = "characterORNULL",
        "sbg:id"                  = "characterORNULL",
        "sbg:license"             = "characterORNULL",
        "sbg:revision"            = "integerORNULL",
        "sbg:revisionNotes"       = "characterORNULL",
        "sbg:cmdPreview"          = "characterORNULL",
        "sbg:modifiedOn"          = "integerORNULL",
        "sbg:modifiedBy"          = "characterORNULL",
        "sbg:revisionsInfo"       = "listORNULL",
        "sbg:toolkit"             = "characterORNULL",
        "sbg:image_url"           = "characterORNULL",
        "sbg:updateRevisionNotes" = "characterORNULL",
        "sbg:updateModifiedBy"    = "characterORNULL",
        "sbg:update"              = "characterORNULL",
        "sbg:appVersion"          = "listORNULL",
        "sbg:projectName"         = "characterORNULL",
        "sbg:publisher"           = "characterORNULL",
        "cwlVersion"              = "characterORNULL"

    ),

    methods = list(

        initialize = function(
            homepage           = NULL,
            validationErrors   = NULL,
            sbgMaintained      = NULL,
            latestRevision     = NULL,
            job                = NULL,
            toolAuthor         = NULL,
            copyOf             = NULL,
            createdOn          = NULL,
            categories         = NULL,
            contributors       = NULL,
            links              = NULL,
            project            = NULL,
            createdBy          = NULL,
            toolkitVersion     = NULL,
            id                 = NULL,
            license            = NULL,
            revision           = NULL,
            cmdPreview         = NULL,
            modifiedOn         = NULL,
            modifiedBy         = NULL,
            revisionsInfo      = NULL,
            toolkit            = NULL,
            revisionNotes      = NULL,
            updateRevisionNote = NULL,
            updateModifiedBy   = NULL,
            update             = NULL,
            appVersion         = NULL,
            projectName        = NULL,
            publisher          = NULL,
            cwlVersion         = NULL,
            ...) {

            args <- mget(names(formals()),sys.frame(sys.nframe()))
            nms <- names(args)
            for(nm in nms) .self$field(paste0("sbg:", nm), args[[nm]])

            callSuper(...)

        }))

#' Class Tool
#'
#' code{Tool} class extends \code{CommandLineTool}
#' with more seven bridges flabored fields the \code{SBG} class.
#' \code{obj$toJSON()}, \code{obj$toJSON(pretty = TRUE)} or
#' \code{obj$toYAML()} will convert a
#' \code{Tool} object into a text JSON/YAML file.
#'
#' @field context [character] by default:
#' \url{http://www.commonwl.org/draft-2/}
#' @field owner [list] a list of owner names.
#' @field contributor [list] a list of contributor names.
#'
#' @export Tool
#' @return a Tool object.
#' @exportClass Tool
#' @examples
#' t1 = system.file("extdata/app", "tool_star.json", package = "sevenbridges")
#' # convert json file into a Tool object
#' t1 = convert_app(t1)
#' # get input type information
#' t1$input_type()
#' # get output type information
#' t1$output_type()
#' # return a input matrix with more informtion
#' t1$input_matrix()
#' # return only a few fields
#' t1$input_matrix(c("id", "type", "required"))
#' # return only required
#' t1$input_matrix(required = TRUE)
#' # return everything
#' t1$input_matrix(NULL)
#' # return a output matrix with more informtion
#' t1$output_matrix()
#' # return only a few fields
#' t1$output_matrix(c("id", "type"))
#' # return everything
#' t1$output_matrix(NULL)
#' # get input id
#' t1$input_id()
#' # get full input id with Tool name
#' t1$input_id(TRUE)
#' # get output id
#' t1$output_id()
#' # get full output id
#' t1$output_id(TRUE)
#' # get required input id
#' t1$get_required()
#' # set input required
#' t1$set_required(c("#reads", "winFlankNbins"))
#' t1$get_required()
#' t1$set_required("reads", FALSE)
#' t1$get_required()
#' t1$get_input(name = "ins")
#' t1$get_input(id = "#winFlankNbins")
#' t1$get_output(name = "gene")
#' t1$get_output(id = "#aligned_reads")
#' # get a tool from a flow
#' f1 = system.file("extdata/app", "flow_star.json", package = "sevenbridges")
#' # convert json file into a Tool object
#' f1 = convert_app(f1)
#' t2 = f1$get_tool("STAR$")
#' oid = t2$get_input_port()
#' oid
#' # set new ports
#' t2$input_id()
#' t2$set_input_port("#chimScoreSeparation")
#' t2$get_input_port()
#' t2$set_input_port("#chimScoreSeparation", FALSE)
#' t2$get_input_port()
#' # run the tool locally with example data
#' \dontrun{
#' t3 = system.file("extdata/app/dna2protein", "translate.cwl.json", package = "sevenbridges")
#' t3 = convert_app(t3)
#' fl = system.file("extdata/app/dna2protein/data", "input.txt", package = "sevenbridges")
#' set_test_env("dind", "tengfei/testenv", "~/mounts")
#' t3$input_type()
#' t3$run(list(input_file = Files(fl)))  # Not File}
Tool <- setRefClass(
    "Tool", contains = c("CommandLineTool", "SBG"),

    fields = list(
        context = "characterORNULL",
        x       = "numericORNULL",
        y       = "numericORNULL"),

    methods = list(

        initialize = function(
            ...,
            id      = NULL,
            label   = NULL,
            inputs  = NULL,
            outputs = NULL,
            x       = NULL,
            y       = NULL) {

            stopifnot(!is.null(id))

            if (is.null(label)) {
                .label <- id
            } else {
                .label <- label
            }

            if (is(inputs, "InputParameterList") ||
                (is.list(inputs) &&
                 all(sapply(inputs, is, "InputParameter")))) {

                if (is.list(inputs) &&
                    all(sapply(inputs, is, "InputParameter"))) {
                    inputs <<- IPList(inputs)
                }

                if (is(inputs, "InputParameterList")) {
                    inputs <<- inputs
                }

            } else if (is.data.frame(inputs)) {
                lst <- lapply(1:nrow(inputs), function(i) {
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
            } else if (is(inputs, "InputParameter")) {
                inputs <<- IPList(inputs)
            } else if (is.null(inputs)) {
                inputs <<- IPList()
            } else {
                stop("wrong inputs type")
            }
            # outputs
            if (is(outputs, "OutputParameterList") ||
                (is.list(outputs) &&
                 all(sapply(outputs, is, "OutputParameter")))) {

                if (is.list(outputs) &&
                    all(sapply(outputs, is, "OutputParameter"))) {
                    outputs <<- OPList(outputs)
                }

                if (is(outputs, "OutputParameterList")) {
                    outputs <<- outputs
                }

            } else if (is.data.frame(outputs)) {

                lst <- lapply(1:nrow(outputs), function(i) {
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

            } else if (is(outputs, "OutputParameter")) {
                outputs <<- OPList(outputs)
            } else if (is.null(outputs)) {
                outputs <<- OPList()
            } else {
                stop("wrong output")
            }
            x <<- x
            y <<- y
            res <- callSuper(id = id, label  = .label, ...)
            res$field("sbg:id", id)
            res
        },

        input_type = function() {
            'this return a vector of types, names of them are input id'
            getInputType(toList())
        },

        output_type = function() {
            'this return a vector of types, names of them are output id'
            getOutputType(toList())
        },

        input_matrix = function(
            new.order = c("id", "label", "type", "required", "prefix", "fileTypes"),
            required = NULL) {
            '
            This return a matrix of input parameters, by default,
            following the order id, label, type, required, prefix,
            fileTypes. new.order accept names of column you want to
            print, but it has to be a field of inputs. When its set
            to NULL, it prints all fields. When required = TRUE,
            only print required field.'
            res = suppressWarnings(as(inputs, "data.frame"))
            if (!is.null(required)) {
                stopifnot(is.logical(required))
                res = res[res$required == required, ]
                if (!nrow(res)) {
                    return(NULL)
                }
            }
            if (!is.null(new.order)) {
                new.order = intersect(new.order, names(res))
                res[, new.order]
            } else {
                res
            }
        },

        output_matrix = function(
            new.order = c("id", "label", "type", "fileTypes")) {
            '
            This return a matrix of output parameters, by default,
            following the order id, label, type, fileTypes.
            new.order accept names of column you want to print,
            but it has to be a field of outputs. When its set to
            NULL, it prints all fields. when required = TRUE,
            only print required field.'

            res = suppressWarnings(as(outputs, "data.frame"))
            if (!is.null(new.order)) {
                new.order = intersect(new.order, names(res))
                res[, new.order]
            } else {
                res
            }

        },

        input_id = function(full = FALSE, requiredOnly = FALSE) {
            '
            Get input id from a Tool, when full = TRUE,
            connect tool id with input id. e.g. If
            requiredOnly = TRUE, return required field only.'
            tool.name = get_id_from_label(label)
            if (requiredOnly) {
                idx = sapply(inputs, function(i) {
                    i$required
                })
                if (length(idx)) {
                    .inputs = inputs[idx]
                } else {
                    return(NULL)
                }
            } else {
                .inputs = inputs
            }
            res = sapply(.inputs, function(i) {
                if (full) {
                    res = paste0(tool.name, ".", de_sharp(i$id))
                    names(res) = .make_type(i$toList()$type)
                    res
                } else {
                    res = i$id
                    names(res) = tool.name
                    res
                }
            })
            res
        },

        output_id = function(full = FALSE) {
            '
            Get output id from a Tool, when full = TRUE,
            connect tool id with input id.'
            tool.name = get_id_from_label(label)
            res = sapply(outputs, function(o) {
                if (full) {
                    res = paste0(tool.name, ".", de_sharp(o$id))
                    names(res) = .make_type(o$toList()$type)
                    res
                } else {
                    res = o$id
                    names(res) = tool.name
                    res
                }
            })
            res
        },

        get_required = function() {
            'return required input fields types, names of them are input id'
            res = unname(unlist(sapply(inputs, function(i) {
                if (i$required) {
                    return(i$id)
                } else {
                    return(NULL)
                }
            })))
            if (length(res)) {
                res = de_sharp(res)
                it = input_type()
                enms = names(it)

                it[enms %in% res]
            } else {
                return(NULL)
            }
        },

        set_required = function(ids, required = TRUE) {
            '
            Set an input node required or not required. The first
            parameter takes single input id or more than one ids.
            the second parameters \\code{required} is the value you
            want to set to inputs. \\code{TRUE} means set to required.'
            iid <- input_id()
            ids <- addIdNum(ids)
            idx = ids %in% iid
            if (any(!idx)) {
                stop("mistyped id name: ", paste(ids[!idx], collapse = " "))
            }
            sapply(match(ids, iid), function(id) {
                inputs[[id]]$required <<- required
            })
        },

        get_input_port = function() {
            'the inputs node with \\code{sbg:includeInPorts} equals \\code{TRUE}'
            res = sapply(inputs, function(i) {

                if (is.null(i$'sbg:includeInPorts')) {
                    return(FALSE)
                } else {
                    return(i$'sbg:includeInPorts')
                }
            })
            idx = which(res)
            if (length(idx)) {
                input_id()[idx]
            } else {
                return(NULL)
            }
        },

        set_input_port = function(ids, include = TRUE) {
            'Set inputs ports field \\code{sbg:includeInPorts}
            to the value of include, default is \\code{TRUE}.'
            idx = match(ids, input_id())
            if (length(idx)) {
                for (i in idx) {
                    inputs[[i]]$'sbg:includeInPorts' <<- include
                }
            }
        },

        get_input = function(name = NULL, id = NULL) {
            'get input objects by names or id'
            if (is.null(name) && is.null(id)) {
                stop("please provide name or id")
            }

            if (!is.null(name)) {
                idx = which(grepl(name, sapply(inputs, function(i) i$label)))
            }
            if (!is.null(id)) {
                idx = which(id == sapply(inputs, function(i) i$id))
            }
            if (length(idx) == 0) {
                return(NULL)
            } else if (length(idx) == 1) {
                return(inputs[[idx]])
            } else if(length(idx) >1) {
                return(inputs[idx])
            }

        },

        get_output = function(name = NULL, id = NULL) {
            'get output objects by names or id'
            if (is.null(name) && is.null(id)) {
                stop("please provide name or id")
            }

            if (!is.null(name)) {
                idx = which(grepl(name, sapply(outputs, function(i) i$label)))
            }
            if (!is.null(id)) {
                idx = which(grepl(id, sapply(outputs, function(i) i$id)))
            }
            if (length(idx) == 0) {
                return(NULL)
            } else if(length(idx) == 1) {
                return(outputs[[idx]])
            } else if (length(idx) >1) {
                return(outputs[idx])
            }
        },

        copy_obj = function() {
            'this is a hack to make copy of reference cwl object'
            tmp = tempfile()
            write(toJSON(pretty = TRUE), tmp)
            res = convert_app(tmp)
            file.remove(tmp)
            res
        },
        run = function(run_inputs = list(), engine = c("bunny", "rabix", "cwlrun")) {
            'Run this tool with inputs locally.
            engine supported: bunny, rabix, cwlrun.
            Inputs accept list or JSON'
            engine = match.arg(engine)
            # convert
            run_inputs = lapply(run_inputs, asTaskInput)
            switch(engine,
                   bunny = {
                       test_tool_bunny(.self, run_inputs)
                   },
                   cwlrun = {
                       test_tool_cwlrun(.self, run_inputs)
                   },
                   rabix = {
                       test_tool_rabix(.self, run_inputs)
                   })
        }
    ))
