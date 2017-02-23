#' Class CWL
#'
#' Define CWL class and generic methods, no fields defeind.
#'
#' @aliases CWL-class
#'
#' @export CWL
#' @exportClass CWL
#' @rdname CWL
#' @examples
#' ## no fields, only to provide methods to be extended
#' x <- CWL()
CWL <- setRefClass(
    "CWL",

    methods = list(

        getFields = function(values) {
            '
            return fields as a list, used for following conversion,
            does not assume the value is primitive type.'
            # from Martin's code
            flds = names(getRefClass()$fields())
            if (!missing(values))
                flds = flds[flds %in% values]
            result = setNames(vector("list", length(flds)), flds)
            for (fld in flds)
                result[[fld]] = .self[[fld]]
            result
        },

        toList = function(...) {
            'Convert object to a list of simple data types'
            # simple assumption here
            # need to be override to make sure everything is list
            res <- .self$getFields(...)
            res <- lapply(res, function(x) {
                asList(x) ## until it's not s4 or cwl or SimpleLi
            })
            # mark unbox
            # recurseively jsonlite::unbox single string for JSON
            # need to test YAML
            res <- rapply(res, function(x) {
                if (!is(x, "scalar") &&
                    (is.character(x) ||
                     is.numeric(x) || is.logical(x))) {
                    if (length(x) == 1) {
                        if (!inherits(x, "DSCList") && !is(x, "box")) {
                            return(jsonlite::unbox(x))
                        } else {
                            return(x)
                        }
                    } else if (is.character(x) && length(x) == 0) {
                        # need to avoid type in cwl
                        return(jsonlite::unbox(""))
                    } else {
                        return(x)
                    }
                } else if (is(x, "SingleEnum")) {
                    return(as.character(x))
                } else {
                    return(x)
                }

            }, how = "replace")

            return(res)

        },

        toYAML = function(...) {
            'Covert object to YAML'
            l <- .self$toList()
            yaml::as.yaml(l, ...)
        },

        toJSON = function(...) {
            'Covert object to JSON'
            l <- .self$toList()
            jsonlite::toJSON(l, ...)
        },

        show = function(format = c("YAML", "JSON"), ...) {
            'pretty print YAML (default) or JSON format of an object'
            format <- match.arg(format)
            switch(format,
                   YAML = {
                       err <- try(writeLines(toYAML(...)),
                                  silent = TRUE)
                       if (inherits(err, "try-error")) {
                           showDefault(.self)
                       }
                   },
                   JSON = {
                       err <- try(print(prettify(
                           .self$toJSON(...))), silent = TRUE)
                       if (inherits(err, "try-error")) {
                           showDefault(.self)
                       }
                   })
        }

    ))

#' Convert a object slots/fields to a list, json, or yaml file
#'
#' Doesn't like \code{as.list}, only fields and slots are converted,
#' prepare a object to be conveted to YAML/JSON.
#'
#' @param object object, could be S4/R5 object.
#' For example, class CWL, SimpleList.
#' @param ... other parameters passed to \code{as.yaml} or \code{toJSON}.
#'
#' @export
#' @docType methods
#' @rdname as-methods
#'
#' @return a list object or json or yaml file.
#'
#' @examples
#' ## define a S4 object
#' A <- setClass("A", slots = list(a = "character", b = "numeric"))
#' ## define a reference object which extends 'CWL' class
#' B <- setRefClass("B", fields = list(x = "character", y = "A"), contains = "CWL")
#' ## new instances
#' a <- A(a = "hello", b = 123)
#' b <- B(x = "world", y = a)
#'
#' ## show
#' b
#' b$show("JSON")
#' b$show("YAML")
#'
#' ## You can convert slots/fields into a list
#' asList(a)
#' asList(b)
#' b$toList()
#'
#' ## asYAML
#' asYAML(a)
#' asYAML(b)
#' b$toYAML()
#'
#' ## asJSON
#' asJSON(a)
#' asJSON(b)
#' b$toJSON()
setGeneric("asList", function(object, ...) standardGeneric("asList"))

#' @rdname as-methods
#' @aliases asList,ANY-method
setMethod("asList", "ANY", function(object, ...) {
    if (is(object, "envRefClass")) {
        res <- .getFields(object)
        res <- lapply(res, function(x) {
            asList(x)
        })
    } else if (isS4(object)) {
        # get slots as list
        res <- getFields(object)
        res <- lapply(res, function(x) {
            asList(x)
        })
    } else {
        res <- object
    }
    return(res)
})

getFields <- function(x, values) {
    .nms <- slotNames(x)
    if (!missing(values))
        .nms <- .nms[.nms %in% values]
    res <- setNames(vector("list", length(.nms)), .nms)
    res
    for (nm in .nms)
        res[[nm]] <- slot(x, nm)
    res
}

#' @rdname as-methods
#' @aliases asList,CWL-method
setMethod("asList", "CWL", function(object, ...) {
    object$toList(...)
})

#' @rdname as-methods
#' @aliases asList,SingleEnum-method
setMethod("asList", "SingleEnum", function(object, ...) {
    ## sorry object, I have to make you a character ...
    as.character(object)
})

#' @rdname as-methods
#' @aliases asList,SimpleList-method
setMethod("asList", "SimpleList", function(object, ...) {
    if (length(object)) {
        res <- lapply(object, asList)
    } else {
        res <- list()
    }
    res
})

#' @docType methods
#' @export asYAML
#' @rdname as-methods
#' @aliases asYAML
setGeneric("asYAML", function(object, ...) standardGeneric("asYAML"))

#' @rdname as-methods
#' @aliases asYAML,ANY-method
setMethod("asYAML", "ANY", function(object, ...) {
    as.yaml(asList(object), ...)
})

#' @docType methods
#' @export asJSON
#' @aliases asJSON
#' @rdname as-methods
setGeneric("asJSON", function(object, ...) standardGeneric("asJSON"))

#' @rdname as-methods
#' @aliases asJSON,ANY-method
setMethod("asJSON", "ANY", function(object, ...){
    toJSON(asList(object), ...)
})



#' DSC list
#'
#' Contains DataypeSingleEnum, Schema, character
#'
#' @param \dots element or list of the element.
#' @export DSCList
#' @exportClass DSCList
#'
#' @return a DSCList
#' @aliases DSCList DSCList-class
#'
#' @examples
#' DSCList("test", DatatypeEnum(), Schema())
DSCList <- setListClass("DSC")

#' @rdname as-methods
#' @aliases asList,DSCList-method
setMethod("asList", "DSCList", function(object, ...){

    if (length(object) ==1 && is.character(object[[1]])) {
        if (length(object[[1]]) ==1) {
            return(list(object[[1]]))
        } else {
            return(object[[1]])
        }
    }
    if (length(object)) {
        res <- lapply(object, function(x) {
            if (is.character(x)) {
                if (length(x) == 1) {
                    # double check, do not unbox it.
                    r <- unbox(x)
                } else {
                    r <- x
                }
            } else if (is(x, "ItemArray")) {
                if (is(x$items, "enum")) {
                    r <- list(type = as.character(x$type),
                              items = x$items$toList())
                } else {
                    r <- list(items = as.character(x$items),
                              type = as.character(x$type))
                }

            } else if (is(x, "enum")) {
                r <- list(name = as.character(x$name),
                          symbols = as.character(x$symbols),
                          type = as.character(x$type))
            } else {
                r <- x
            }
            r
        })
    } else {
        res <- list()
    }
    res
})

#----------------------------------------------------------------------
# Schema
#----------------------------------------------------------------------

#' SchemaList
#'
#' @aliases SchemaList-class
#'
#' @param \dots element or list of the element.
#'
#' @export SchemaList
#' @exportClass SchemaList
#' @rdname Schema
#' @aliases ScehmaList
SchemaList <- setListClass("Schema")

#' Schema Class
#'
#' A schema defines a parameter type.
#'
#' @field type (ANY) The data type of this parameter.
#'
#' @field fields [SchemaList] When type is record, defines the fields of the
#' record.
#'
#' @field symbols [character] When type is enum, defines the set of valid symbols.
#'
#' @field items [ANY] When type is array, defines the type of the array
#' elements.
#'
#' @field values [ANY] When type is map, defines the value type for the
#' key/value pairs.
#'
#' @export Schema
#' @exportClass Schema
#' @rdname Schema
#' @aliases Schema
#' @examples
#' Schema(fields = SchemaList(SchemaDef(name = "schema")))
Schema <- setRefClass(
    "Schema", contains = "CWL",
    fields = list(
        type    = "DSCList",
        fields  = "SchemaList",
        symbols = "characterORNULL",
        items   = "ANY",
        values  = "ANY"
    ),
    methods = list(
        initialize = function(type = "", ...) {
            if (is(type, "DSCList")) {
                type <<- type
            } else {
                if (is.character(type)) {
                    .type <- deType(type)
                } else {
                    .type <- type
                }
                type <<- DSCList(.type)
            }
            callSuper(...)
        }
    ))

#' SchemaDef Class
#'
#' @export SchemaDef
#' @exportClass SchemaDef
#' @rdname Schema
#' @aliases SchemaDef
SchemaDef <- setRefClass(
    "SchemaDef", contains = "Schema",
    fields = list(name = "characterORNULL"))

#----------------------------------------------------------------------
# Datatype
#----------------------------------------------------------------------

.CWL.Primitive <- c(
    "null",     # no value
    "boolean",  # a binary value
    "int",      # 32-bit signed integer
    "long",     # 64-bit signed integer
    "float",    # single precision (32-bit)
    "double",   # double precision (64-bit)
    "bytes",    # sequence of uninterpreted 8-bit unsigned bytes
    "string"    # unicode character sequence
)

.CWL.Complex <- c(
    "record",  # An object with one or more fields defined by name and type
    "enum",    # A value from a finite set of symbolic values
    "array",   # An ordered sequence of values
    "map"      # An unordered collection of key/value pairs
)

#' Pre-defiend enums
#'
#' Please check \code{cwl:::.CWL.Pritimive}, \code{cwl:::.CWL.Complex}.
#'
#' @rdname Enum
#' @export PrimitiveEnum
#' @exportClass PrimitiveSingleEnum
#' @examples
#' PrimitiveEnum()
#' PrimitiveEnum("boolean")
#' ComplexEnum("record")
#' DatatypeEnum("map")
PrimitiveEnum <- setSingleEnum("Primitive" , levels = .CWL.Primitive)

#' @rdname Enum
#' @aliases ComplexEnum
#' @export ComplexEnum
#' @exportClass ComplexSingleEnum
ComplexEnum <- setSingleEnum("Complex" , levels = .CWL.Complex)

#' @rdname Enum
#' @aliases DatatypeEnum
#' @export DatatypeEnum
#' @exportClass DatatypeSingleEnum
DatatypeEnum <- setSingleEnum(
    "Datatype", levels = c(.CWL.Primitive, .CWL.Complex, "File"))

#' @rdname Enum
#' @aliases enum
#' @export enum
#' @exportClass enum
enum <- setRefClass(
    "enum", contains = "CWL",
    fields = list(
        name    = "character",
        symbols = "character",
        type    = "character"),
    methods = list(
        initialize = function(
            name = NULL,
            symbols = NULL,
            type = "enum") {
            stopifnot(!is.null(name))
            stopifnot(!is.null(symbols))
            if (is.list(symbols)) {
                .self$symbols <<- as.character(symbols)
            } else {
                .self$symbols <<- symbols
            }
            name <<- name
            type <<- type
        }
    ))

setClassUnion("DatatypeSingleEnumORenum", c("DatatypeSingleEnum", "enum"))

#' @rdname Enum
#' @aliases ItemArray
#' @export ItemArray
#' @exportClass ItemArray
ItemArray <- setRefClass(
    "ItemArray", contains = "CWL",
    fields = list(
        items = "DatatypeSingleEnumORenum",
        name  = "characterORNULL",
        type  = "character"),
    methods = list(
        initialize = function(
            items = "",
            name  = NULL,
            type  = "array") {

            type  <<- type
            name  <<- name

            if ("type" %in% names(items) && items$type == "enum") {
                items <<- do.call(enum, items)
            } else {
                items <<- DatatypeEnum(deType(items))
            }

        }
    ))

# TODO: singleEnum <> Enum
setClassUnion("DSC", c("DatatypeSingleEnum", "Schema", "character", "ItemArray", "enum"))

#----------------------------------------------------------------------
# Data Type
#----------------------------------------------------------------------

#' FileList Class
#'
#' @rdname File-class
#' @aliases FileList-class
#' @param \dots element or list of the element.
#'
#' @export FileList
#' @exportClass FileList
FileList <- setListClass("File")

#' File Class
#'
#' @field class (character) Must be File to indicate this object
#' describes a file.
#' @field path (character) The path to the file.
#' @field checksum [character] Optional hash code for validating file
#' integrity. Currently must be in the form "sha1$ + hexidecimal
#' string" using the SHA-1 algorithm.
#' @field size [numeric] Optional file size.
#' @field secondaryFile [FileList] A list of additional files that are
#' associated with the primary file and must be transferred alongside
#' the primary file. Examples include indexes of the primary file, or
#' external references which must be included when loading primary
#' document. A file object listed in secondaryFiles may itself include
#' secondaryFiles for which the same rules apply.
#'
#' @return File class generator
#'
#' @export File
#' @exportClass File
#'
#' @rdname File-class
#' @examples
#' library(jsonlite)
#' library(yaml)
#' f1 <- File()
#' f2 <- File(path = "./out.bam", checksum = "test",
#'            size = 3L, secondaryFile = FileList(File(path = "./out.bai")))
#' fl <- FileList(f1, f2)
#' asList(fl)
#' writeLines(asYAML(fl))
#' asJSON(fl)
#' f1
#' f2
#' fl
File <- setRefClass(
    "File", contains = "CWL",
    fields = list(
        class         = "characterORNULL",
        path          = "characterORNULL",
        checksum      = "characterORNULL",
        size          = "numericORNULL",  # integer has small limit
        secondaryFile = "FileList"
    ),
    methods = list(
        initialize = function(class = "File", ...) {
            class <<- class
            callSuper(...)
        }
    ))

#----------------------------------------------------------------------
# Expression
#----------------------------------------------------------------------

setClass("JsonPointer", contains = "VIRTUAL")
setClassUnion("JsonPointerORcharacter", c("JsonPointer", "character"))

#' Expression Class
#'
#' Define an expression that will be evaluated and used to modify the
#' behavior of a tool or workflow. See Expressions for more
#' information about expressions and ExpressionEngineRequirement for
#' information on how to define a expression engine.
#'
#' @field engine (JsonPointerORcharacter) Either cwl:JsonPointer or a
#' reference to an ExpressionEngineRequirement defining which engine
#' to use.
#' @field script (character) The code to be executed by the expression
#' engine.
#'
#' @export Expression
#' @exportClass Expression
#'
#' @examples
#' Expression(engine = "#cwl-js-engine", script = "$job.inputs['threads']")
Expression <- setRefClass(
    "Expression",
    contains = "CWL",
    fields = list(
        engine = "JsonPointerORcharacter",
        script = "characterORNULL",
        class  = "characterORNULL"
    ),
    methods = list(
        initialize = function(
            script = NULL,
            engine = "#cwl-js-engine",
            class  = "Expression") {

            script <<- script
            engine <<- engine
            class <<- class

        }
    ))

setClassUnion("ExpressionORNULL", c("Expression", "NULL"))
setClassUnion("characterORExpression", c("character", "Expression"))
setClassUnion("integerORExpression", c("integer", "Expression"))
setClassUnion("characterORExpressionORNULL", c("character", "Expression", "NULL"))
setClassUnion("characterORExpressionORlistORNULL", c("character", "Expression", "list", "NULL"))

#----------------------------------------------------------------------
# ProcessRequirement
#----------------------------------------------------------------------

#' ProcessRequirement Class
#'
#' @section ProcessRequirement:
#'
#' \describe{ A process requirement
#' modifies the semantics or runtime environment of a process. If an
#' implementation cannot satisfy all requirements, or a requirement is
#' listed which is not recognized by the implementation, it is a fatal
#' error and the implementation must not attempt to run the process,
#' unless overridden at user option.
#'
#' \item{\code{class}}{(character) The specific requirement type.}
#' }
#'
#' @rdname ProcessRequirement
#' @export ProcessRequirement
#' @exportClass ProcessRequirement
#'
#' @return a ProcessRequirement object or subclass object.
#'
#' @examples
#' dkr <- DockerRequirement(dockerImageId = "testid")
#' cfr <- CreateFileRequirement(fileDef =
#'                                  FileDefList(FileDef(filename = "hello.txt")))
#' sfr <- SubworkflowFeatureRequirement()
#' evr <- EnvVarRequirement(envDef = EnvironmentDefList(
#'     EnvironmentDef(envName = "path",
#'                    envValue = "testpath")
#' ))
#' safr <- ScatterFeatureRequirement()
#' eer <- ExpressionEngineRequirement(id = "hello")
#' ProcessRequirementList(dkr, cfr, sfr, evr, safr, eer)
ProcessRequirement <- setRefClass(
    "ProcessRequirement",
    contains = c("VIRTUAL", "CWL"),
    field = list(class = "character"))

#' DockerRequirement Class
#'
#' @section DockerRequirement Class:
#' \describe{
#'
#' Indicates that a workflow component should be run in a Docker
#' container, and specifies how to fetch or build the image.
#' If a CommandLineTool lists DockerRequirement under hints or
#' requirements, it may (or must) be run in the specified Docker
#' container. The platform must first acquire or install the correct
#' Docker image, as described by DockerRequirement. The platform must
#' execute the tool in the container using docker run with the
#' appropriate Docker image and the tool command line. The workflow
#' platform may provide input files and the designated output
#' directory through the use of volume bind mounts. The platform may
#' rewrite file paths in the input object to correspond to the Docker
#' bind mounted locations. When running a tool contained in Docker,
#' the workflow platform must not assume anything about the contents
#' of the Docker container,such as the presence or absence of specific
#' software, except to assume that the generated command line
#' represents a valid command within the runtime environment of the
#' container.
#'
#' \item{\code{dockerPull}}{(character) Get a Docker image using
#' docker pull}
#'
#' \item{\code{dockerLoad}}{(character) Specify a HTTP URL from which
#' to download a Docker image using docker load.}
#'
#' \item{\code{dockerFile}}{(character) Supply the contents of a
#' Dockerfile which will be build using docker build.}
#'
#' \item{\code{dockerImageId}}{(character) The image id that will be
#' used for docker run. May be a human-readable image name or the
#' image identifier hash. May be skipped if dockerPull is specified,
#' in which case the dockerPull image id will be used.}
#'
#' \item{\code{dockerOutputDirectory}}{(character) Set the designated
#' output directory to a specific location inside the Docker
#' container.}
#'
#' }
#'
#' @export DockerRequirement
#' @exportClass DockerRequirement
#'
#' @rdname ProcessRequirement
#' @aliases DockerRequirement
DockerRequirement <- setRefClass(
    "DockerRequirement", contains = "ProcessRequirement",
    fields = list(
        dockerPull            = "characterORNULL",
        dockerLoad            = "characterORNULL",
        dockerFile            = "characterORNULL",
        dockerImageId         = "characterORNULL",
        dockerOutputDirectory = "characterORNULL"
    ),
    method = list(
        initialize = function(
            class = "DockerRequirement",
            ...) {
            class <<- class
            callSuper(...)
        }
    ))

#' @section SubworkflowFeatureRequirement Class:
#'
#' Indicates that the workflow platform must support nested workflows
#' in the run field of (WorkflowStep)(#workflowstep).
#'
#' @export SubworkflowFeatureRequirement
#' @exportClass SubworkflowFeatureRequirement
#'
#' @rdname ProcessRequirement
#' @aliases SubworkflowFeatureRequirement
SubworkflowFeatureRequirement <- setRefClass(
    "SubworkflowFeatureRequirement", contains = "ProcessRequirement",
    method = list(
        initialize = function(
            class = "SubworkflowFeatureRequirement",
            ...) {
            class <<- class
            callSuper(...)
        }
    ))

#' @section FileDef Class:
#'
#' \describe{
#'
#' Define a file that must be placed by in the designated output
#' directory prior to executing the command line tool. May be the
#' result of executing an expression, such as building a configuration
#' file from a template.
#'
#' \item{\code{filename}}{(characterORExpression) The name of the file
#' to create in the output directory.}
#'
#' \item{\code{fileContent}}{(characterORExpression) If the value is a
#' string literal or an expression which evalutes to a string, a new
#' file must be created with the string as the file contents. If the
#' value is an expression that evaluates to a File object, this
#' indicates the referenced file should be added to the designated
#' output directory prior to executing the tool. Files added in this
#' way may be read-only, and may be implemented through bind mounts or
#' file system links in such a way as to avoid unecessary copying of
#' the input file.}
#'
#' }
#'
#' @export FileDef
#' @exportClass FileDef
#' @rdname ProcessRequirement
FileDef <- setRefClass(
    "FileDef",
    fields = list(
        filename    = "characterORExpressionORNULL",
        fileContent = "characterORExpressionORNULL"),
    methods = list(
        initialize = function(filename = NULL, fileContent = NULL) {
            if (is.list(filename)) {
                filename <<- do.call(Expression, filename)
            } else {
                filename <<- filename
            }
            if (is.list(fileContent)) {
                fileContent <<- do.call(Expression, fileContent)
            } else {
                fileContent <<- fileContent
            }
        }
    ))

#' @export FileDefList
#' @exportClass FileDefList
#'
#' @rdname ProcessRequirement
#' @aliases FileDefList FileDefList-class
FileDefList <- setListClass("FileDef")

#' @section CreateFileRequirement Class:
#' \describe{
#'
#' Define a list of files that must be created and placed by the
#' workflow platform in the designated output directory prior to
#' executing the command line tool. See FileDef for details.
#'
#' \item{\code{fileDef}}{(FileDefList) The list of files.}
#'
#' }
#'
#' @export CreateFileRequirement
#' @exportClass CreateFileRequirement
#' @rdname ProcessRequirement
#' @aliases CreateFileRequirement
CreateFileRequirement <- setRefClass(
    "CreateFileRequirement", contains = "ProcessRequirement",
    fields = list(
        fileDef = "FileDefList"
    ),
    method = list(
        initialize = function(
            class = "CreateFileRequirement",
            ...) {
            class <<- class
            callSuper(...)
        }
    ))

#' @section EnvironmentDef Class:
#' \describe{
#'
#' Define an environment variable that will be set in the runtime
#' environment by the workflow platform when executing the command
#' line tool. May be the result of executing an expression, such as
#' getting a parameter from input.
#'
#' \item{\code{envName}}{(character) The environment variable name. }
#'
#' \item{\code{envValue}}{(characterORExpression) The environment
#' variable value.}
#'
#' }
#' @export EnvironmentDef
#' @exportClass EnvironmentDef
#' @rdname ProcessRequirement
#' @aliases EnvironmentDef
EnvironmentDef <- setRefClass(
    "EnvironmentDef",
    fields = list(
        envName  = "characterORNULL",
        envValue = "characterORExpression"))

#' @param \dots element or list of the element.
#'
#' @export EnvironmentDefList
#' @exportClass EnvironmentDefList
#' @rdname ProcessRequirement
#' @aliases EnvironmentDefList EnvironmentDefList-class
EnvironmentDefList <- setListClass("EnvironmentDef")

#' @section EnvVarRequirement Class:
#' \describe{
#'
#' Define a list of environment variables which will be set in the
#' execution environment of the tool. See EnvironmentDef for details.
#'
#' \item{\code{envDef}}{(EnvironmentDefList) The list of environment
#' variables.}
#'
#' }
#'
#' @export EnvVarRequirement
#' @exportClass EnvVarRequirement
#' @rdname ProcessRequirement
#' @aliases EnvVarRequirement
EnvVarRequirement <- setRefClass(
    "EnvVarRequirement", contains = "ProcessRequirement",
    fields = list(envDef = "EnvironmentDefList"),
    method = list(
        initialize = function(
            class = "EnvVarRequirement",
            ...) {
            class <<- class
            callSuper(...)
        }
    ))

#' @section ScatterFeatureRequirement Class:
#'
#' Indicates that the workflow platform must support the scatter and
#' scatterMethod fields of (WorkflowStep)(#workflowstep).
#'
#' @export ScatterFeatureRequirement
#' @exportClass ScatterFeatureRequirement
#' @rdname ProcessRequirement
#' @aliases ScatterFeatureRequirement
ScatterFeatureRequirement <- setRefClass(
    "ScatterFeatureRequirement", contains = "ProcessRequirement",
    method = list(
        initialize = function(
            class = "ScatterFeatureRequirement",
            ...) {
            class <<- class
            callSuper(...)
        }
    ))

#' ProcessRequirementList
#'
#' @aliases ProcessRequirementList-class
#'
#' @export ProcessRequirementList
#' @exportClass ProcessRequirementList
#' @rdname ProcessRequirement
ProcessRequirementList <- setListClass("ProcessRequirement")

#' @section ExpressionEngineRequirement Class:
#' \describe{
#'
#' Define an expression engine, as described in Expressions.
#'
#' \item{\code{id}}{(character) Used to identify the expression engine in the
#' engine field of Expressions.}
#'
#' \item{\code{requirements}}{[ProcessRequirement]Requirements to run this
#' expression engine, such as DockerRequirement for specifying a
#' container with the engine.}
#'
#' \item{\code{engineCommand}}{ [character] The command line to invoke the
#' expression engine.}
#'
#' \item{\code{engineConfig}}{ [character] Additional configuration or code
#' fragments that will also be passed to the expression engine. The
#' semantics of this field are defined by the underlying expression
#' engine. Intended for uses such as providing function definitions
#' that will be called from CWL expressions.}
#'
#' }
#'
#' @export  ExpressionEngineRequirement
#' @exportClass ExpressionEngineRequirement
#' @rdname ProcessRequirement
#' @aliases ExpressionEngineRequirement
ExpressionEngineRequirement <- setRefClass(
    "ExpressionEngineRequirement", contains = "ProcessRequirement",
    fields = list(
        id = "characterORNULL",
        requirements  = "ProcessRequirementList",
        engineCommand = "characterORNULL",
        engineConfig  = "characterORNULL"
    ),
    method = list(
        initialize = function(
            class = "ExpressionEngineRequirement",
            ...){
            class <<- class
            callSuper(...)
        }
    ))

#----------------------------------------------------------------------
# Process
#----------------------------------------------------------------------

#' SchemaDefList
#'
#' @aliases SchemaDefList-class
#'
#' @export SchemaDefList
#' @exportClass SchemaDefList
#' @rdname Schema
#' @aliases SchemaDefList
SchemaDefList <- setListClass("SchemaDef")

setRefClass("SchemaDefRequirement",
            contains = "ProcessRequirement",
            fields = list(types = "SchemaDefList"))

#----------------------------------------------------------------------
# Binding
#----------------------------------------------------------------------

# setListClass("characterORExpression")

#' Binding
#'
#' @field loadContents [logical] Only applies when type is File. Read
#' up to the first 64 KiB of text from the file and place it in the
#' "contents" field of the file object for manipulation by
#' expressions.
#'
#' @field secondaryFiles [] Only applies when type is File. Describes
#' files that must be included alongside the primary file. If the
#' value is Expression, the context of the expression is the input or
#' output File parameter to which this binding applies. Where the
#' value is a string, it specifies that the following pattern should
#' be applied to the primary file: If string begins with one or more
#' caret characters, for each caret, remove the last file extension
#' from the path (the last period . and all following characters). If
#' there are no file extensions, the path is unchanged.  Append the
#' remainder of the string to the end of the file path.
#'
#' @export Binding
#' @exportClass Binding
#' @rdname Binding
#' @examples
#' Binding(loadContents = TRUE, secondaryFiles = "./test.txt")
Binding <- setRefClass(
    "Binding", contains = "CWL",
    method = list(
        initialize = function(loadContents   = NULL,
                              secondaryFiles = NULL, ...) {
            if (is.character(secondaryFiles)) {
                secondaryFiles <<- set_box(secondaryFiles)
            } else {
                secondaryFiles <<- secondaryFiles
            }
            loadContents <<- loadContents
            callSuper(...)
        }
    ),
    fields = list(
        loadContents   = "logicalORlistORNULL",
        secondaryFiles = "characterORExpressionORlistORNULL"  # FIXME: should be a list
    ))

setClassUnion("BindingORNULL", c("Binding", "NULL"))

#----------------------------------------------------------------------
# Parameter
#----------------------------------------------------------------------

#' Paramter class (reference class)
#'
#' Define an input or output parameter to a process.
#'
#' @field type [ANY] Specify valid types of data that may be assigned
#' to this parameter.
#'
#' @field label [character] A short, human-readable label of this
#' parameter object.
#'
#' @field description [character] A long, human-readable description
#' of this parameter object.
#'
#' @field streamable [logical] Currently only applies if type is
#' File. A value of true indicates that the file is read or written
#' sequentially without seeking. An implementation may use this flag
#' to indicate whether it is valid to stream file contents using a
#' named pipe. Default: false.
#'
#' @field default [ANY] The default value for this parameter if not
#' provided in the input object.
#'
#' @export Parameter
#' @exportClass Parameter
#'
#' @rdname Parameter
#'
#' @return Parameter object
#'
#' @examples
#' Parameter(type = "integer", label = "thread",
#'          description = "Specify the thread #",
#'          default = 0)
#'
#' ipl <- InputParameterList(
#'     InputParameter(id = "BAM", type = "File",
#'                    label = "input bam",
#'                    description = "input bam",
#'                    inputBinding = CommandLineBinding(
#'                        position = 1L
#'                    )),
#'     InputParameter(id = "level", type = "Integer",
#'                    label = "Compression level",
#'                    description = "Compression level",
#'                    inputBinding = CommandLineBinding(
#'                        position = 2L,
#'                        prefix = "-l"
#'                    ))
#' )
#' ipl
Parameter <- setRefClass(
    "Parameter", contains = "CWL",
    fields = list(
        type        = "DSCList",
        label       = "characterORNULL",
        description = "characterORNULL",
        streamable  = "logical",
        default     = "ANY"
    ),
    methods = list(
        initialize = function(..., type = "",
                              streamable = FALSE,
                              default = NULL) {
            if (is(type, "DSCList")) {
                type   <<- type
            } else {
                .type  <- type
                type   <<- DSCList(.type)
            }
            streamable <<- streamable
            default    <<- default
            callSuper(...)
        }
    ))

#' InputParameterList
#'
#' @aliases InputParameterList InputParameterList-class
#'
#' @param \dots element or list of the element.
#'
#'
#' @export InputParameterList
#' @exportClass InputParameterList
#' @rdname Parameter
InputParameterList <- setListClass("InputParameter")

#' OutputParameterList
#'
#' @aliases OutputParameterList OutputParameterList-class
#'
#' @export OutputParameterList
#' @exportClass OutputParameterList
#' @rdname Parameter
OutputParameterList <- setListClass("OutputParameter")

#' Process Class
#'
#' The base executable type in CWL is the Process object defined by
#' the document. Note that the Process object is abstract and cannot
#' be directly executed.
#'
#' @field id [character] The unique identifier for this process
#' object.
#'
#' @field inputs (InputParameterList) Defines the input parameters of
#' the process. The process is ready to run when all required input
#' parameters are associated with concrete values. Input parameters
#' include a schema for each parameter and is used to validate the
#' input object, it may also be used build a user interface for
#' constructing the input object.
#'
#' @field outputs (OutputParameterList) Defines the parameters
#' representing the output of the process. May be used to generate
#' and/or validate the output object.
#'
#' @field requirements [ProcessRequirementList] Declares requirements
#' that apply to either the runtime environment or the workflow engine
#' that must be met in order to execute this process. If an
#' implementation cannot satisfy all requirements, or a requirement is
#' listed which is not recognized by the implementation, it is a fatal
#' error and the implementation must not attempt to run the process,
#' unless overridden at user option.
#'
#' @field hints [ANY] Declares hints applying to either the runtime
#' environment or the workflow engine that may be helpful in executing
#' this process. It is not an error if an implementation cannot
#' satisfy all hints, however the implementation may report a warning.
#'
#' @field label [character] A short, human-readable label of this
#' process object.
#'
#' @field description [character] A long, human-readable description
#' of this process object.
#'
#' @export Process
#' @exportClass Process
#'
#' @rdname Process
#' @examples
#' ipl <- InputParameterList(
#'     InputParameter(id = "BAM", type = "File",
#'                    label = "input bam",
#'                    description = "input bam",
#'                    inputBinding = CommandLineBinding(
#'                        position = 1L
#'                    )),
#'     InputParameter(id = "level", type = "Integer",
#'                    label = "Compression level",
#'                    description = "Compression level",
#'                    inputBinding = CommandLineBinding(
#'                        position = 2L,
#'                        prefix = "-l"
#'                    ))
#' )
#' ipl
#' p <- Process(id = "process", inputs = ipl)
#' p
Process <- setRefClass(
    "Process", contains = "CWL",
    fields = list(
        id           = "characterORNULL",
        inputs       = "InputParameterList",
        outputs      = "OutputParameterList",
        requirements = "ProcessRequirementList",
        hints        = "ProcessRequirementList",
        label        = "characterORNULL",
        description  = "characterORNULL"
    ),
    methods = list(
        initialize = function(id = "", ...) {
            id <<- addIdNum(id)
            callSuper(...)
        }
    ))

#' InputSchema Class
#'
#' @field inputBinding [Binding] Describes how to handle a value in
#' the input object convert it into a concrete form for execution,
#' such as command line parameters.
#'
#' @export InputSchema
#' @exportClass InputSchema
#' @rdname Schema
#' @aliases InputSchema
#'
#' @return a Schema object or sbuclass object.
InputSchema <- setRefClass(
    "InputSchema", contains = "Schema",
    fields = list(inputBinding = "Binding"))

#' OutputSchema Class
#'
#' @export OutputSchema
#' @exportClass OutputSchema
#' @rdname Schema
#' @aliases OutputSchema
OutputSchema <- setRefClass("OutputSchema", contains = "Schema")

#' InputParameter Class
#'
#' @field id (character) The unique identifier for this parameter object.
#'
#' @field inputBinding [Binding] Describes how to handle the inputs of
#' a process and convert them into a concrete form for execution, such
#' as command line parameters.
#'
#' @export InputParameter
#' @exportClass InputParameter
#' @rdname Parameter
#' @aliases InputParameter InputParameter-class
InputParameter <- setRefClass(
    "InputParameter", contains = "Parameter",
    fields = list(
        id = "characterORNULL",
        inputBinding = "BindingORNULL"),
    methods = list(
        initialize = function(id = "", ...) {
            id <<- addIdNum(id)
            callSuper(...)
        }
    ))

#' OutputParameter Class
#'
#' @field id (character) The unique identifier for this parameter object.
#'
#' @export OutputParameter
#' @exportClass OutputParameter
#' @rdname Parameter
#' @aliases OutputParameter OutputParameter-class
OutputParameter <- setRefClass(
    "OutputParameter", contains = "Parameter",
    fields = list(id = "characterORNULL"),
    methods = list(
        initialize = function(id = "", ...) {
            id <<- addIdNum(id)
            callSuper(...)
        }
    ))

#----------------------------------------------------------------------
# ExpressionTool (FIXME)
#----------------------------------------------------------------------

#' ExpressionTool Class
#'
#' Execute an expression as a process step.
#'
#' @field expression (Expression) The expression to execute. The
#' expression must return a JSON object which matches the output
#' parameters of the ExpressionTool.
#'
#' @export ExpressionTool
#' @exportClass ExpressionTool
#'
#' @examples
#' ExpressionTool(expression =
#'                   Expression(engine = "cwl:JsonPointer",
#'                              script = "$job.inputs['threads']"))
ExpressionTool <- setRefClass(
    "ExpressionTool", contains = "Process",
    fields = list(
        class = "character",
        expression = "Expression"),
    method = list(
        initialize = function(
            class = "ExpressionTool",
            ...) {
            class <<- class
            callSuper(...)
        }
    ))

#----------------------------------------------------------------------
# CommandLineTool
#----------------------------------------------------------------------

#' CommandLineBinding Class
#'
#' When listed under inputBinding in the input schema, the term
#' "value" refers to the the corresponding value in the input
#' object. For binding objects listed in CommandLineTool.arguments,
#' the term "value" refers to the effective value after evaluating
#' valueFrom.
#'
#' @details The binding behavior when building the command line
#' depends on the data type of the value. If there is a mismatch
#' between the type described by the input schema and the effective
#' value, such as resulting from an expression evaluation, an
#' implementation must use the data type of the effective value.
#' \itemize{
#'   \item{character}{Add prefix and the string to the command line.}
#'   \item{numeric}{Add prefix and decimal representation to command line.}
#'   \item{logical}{If true, add prefix to the command line. If false,
#' add nothing.}
#'   \item{File}{Add prefix and the value of File.path to the command line.}
#'   \item{*Array}{If itemSeparator is specified, add prefix and the join the
#'   array into a single string with itemSeparator separating the items.
#'   Otherwise add prefix and recursively add individual elements.}
#' \item{*object}{Add prefix only, and recursively add object fields for
#' which inputBinding is specified.}
#'  \item{null}{Add nothing.}
#' }
#'
#' @field position [integer] The sorting key. Default position is 0.
#'
#' @field prefix [character] Command line prefix to add before the value.
#'
#' @field separate [logical] If true (default) then the prefix and
#' value must be added as separate command line arguments; if false,
#' prefix and value must be concatenated into a single command line
#' argument.
#'
#' @field itemSeparator [character] Join the array elements into a
#' single string with the elements separated by by itemSeparator.
#'
#' @field valueFrom [characterOrExpression] If valueFrom is a constant
#' string value, use this as the value and apply the binding rules
#' above. If valueFrom is an expression, evaluate the expression to
#' yield the actual value to use to build the command line and apply
#' the binding rules above. If the inputBinding is associated with an
#' input parameter, the "context" of the expression will be the value
#' of the input parameter. When a binding is part of the
#' CommandLineTool.arguments field, the valueFrom field is required.
#'
#' @export CommandLineBinding
#' @exportClass CommandLineBinding
#'
#' @examples
#' CommandLineBinding(position = 1L, prefix = "-l")
CommandLineBinding <- setRefClass(
    "CommandLineBinding",
    contains = c("Binding", "CWL"),
    fields = list(
        position      = "integerORNULL",
        # order       = "integerORNULL",  # not exist in CWL specification
        prefix        = "characterORNULL",
        separate      = "logical",
        itemSeparator = "characterORNULL",
        valueFrom     = "characterORExpressionORNULL"
    ),
    methods = list(
        initialize = function(
            position     = 0L,
            separate     = TRUE,
            valueFrom    = NULL,
            order        = NULL, # hack to pass order
            inputBinding = NULL, # hack to pass inputBinding
            ...) {

            if (is.list(valueFrom)) {
                valueFrom <<- do.call(Expression, valueFrom)
            } else {
                valueFrom <<- valueFrom
            }
            position <<- as.integer(position)
            separate <<- separate
            # order  <<- order
            callSuper(...)
        }
    ))

setClassUnion("characterORCommandLineBinding",
              c("character", "CommandLineBinding"))

#' characterORCommandLineBindingList Class
#'
#' @param \dots element or list of the element.
#' @export CCBList
#' @exportClass characterORCommandLineBindingList
#'
#' @aliases CCBList characterORCommandLineBindingList-class
#' @return CCBList
#'
#' @examples
#' CCBList("-o output.bam")
CCBList <- setListClass("characterORCommandLineBinding")

#' CommandLineTool Class
#'
#' A CommandLineTool process is a process implementation for executing
#' a non-interactive application in a POSIX environment. To help
#' accomodate of the enormous variety in syntax and semantics for
#' input, runtime environment, invocation, and output of arbitrary
#' programs, CommandLineTool provides the concept of "input binding"
#' to describe how to translate input parameters to an actual program
#' invocation, and "output binding" to describe how generate output
#' parameters from program output.
#'
#' @section Input binding:
#'
#' The tool command line is built by applying command line bindings to
#' the input object. Bindings are listed either as part of an input
#' parameter using the inputBinding field, or separately using the
#' arguments field of the CommandLineTool.
#'
#' The algorithm to build the command line is as follows. In this
#' algorithm, the sort key is a list consisting of one or more numeric
#' and string elements. Strings are sorted lexicographically based on
#' UTF-8 encoding.
#'
#' \itemize{
#'
#' \item{}{Collect CommandLineBinding objects from arguments. Assign a
#' sorting key [position, i] where position is
#' CommandLineBinding.position and the i is the index in the arguments
#' list.}
#'
#' \item{}{Collect CommandLineBinding objects from the inputs schema
#' and associate them with values from the input object. Where the
#' input type is a record, array, or map, recursively walk the schema
#' and input object, collecting nested CommandLineBinding objects and
#' associating them with values from the input object.  }
#'
#' \item{}{Assign a sorting key for each leaf binding object by appending
#' nested position fields together with the array index, or map key of
#' the data at each nesting level. If two bindings have the same
#' position, the tie must be broken using the lexographic ordering of
#' the field or parameter name immediately containing the binding.}
#'
#' \item{}{Sort elements using the assigned sorting keys. Numeric
#' entries sort before strings.}
#'
#' \item{}{In the sorted order, apply the rules defined in
#' CommandLineBinding to convert bindings to actual command line
#' elements.}
#'
#' \item{}{Insert elements from baseCommand at the beginning of the
#' command line.}
#'
#' }
#'
#' @section Runtime environment:
#'
#' All files listed in the input object must be made available in the
#' runtime environment. The implementation may use a shared or
#' distributed file system or transfer files via explicit
#' download. Implementations may choose not to provide access to files
#' not explicitly specified by the input object or process
#' requirements.
#'
#' Output files produced by tool execution must be written to the
#' designated output directory.
#'
#' The initial current working directory when executing the tool must
#' be the designated output directory.
#'
#' The TMPDIR environment variable must be set in the runtime
#' environment to the designated temporary directory. Any files
#' written to the designated temporary directory may be deleted by the
#' workflow platform when the tool invocation is complete.
#'
#' An implementation may forbid the tool from writing to any location
#' in the runtime environment file system other than the designated
#' temporary directory and designated output directory. An
#' implementation may provide read-only input files, and disallow
#' in-place update of input files.
#'
#' The standard input stream and standard output stream may be
#' redirected as described in the stdin and stdout fields.
#'
#' @section Extensions:
#'
#' DockerRequirement, CreateFileRequirement, and EnvVarRequirement,
#' are available as standard extensions to core command line tool
#' semantics for defining the runtime environment.
#'
#' @section Execution:
#'
#' Once the command line is built and the runtime environment is
#' created, the actual tool is executed.
#'
#' The standard error stream and standard output stream (unless
#' redirected by setting stdout) may be captured by platform logging
#' facilities for storage and reporting.
#'
#' Tools may be multithreaded or spawn child processes; however, when
#' the parent process exits, the tool is considered finished
#' regardless of whether any detached child processes are still
#' running. Tools must not require any kind of console, GUI, or web
#' based user interaction in order to start and run to completion.
#'
#' The exit code of the process indicates if the process completed
#' successfully. By convention, an exit code of zero is treated as
#' success and non-zero exit codes are treated as failure. This may be
#' customized by providing the fields successCodes,
#' temporaryFailCodes, and permanentFailCodes. An implementation may
#' choose to default unspecified non-zero exit codes to either
#' temporaryFailure or permanentFailure.
#'
#' @section Output binding:
#'
#' If the output directory contains a file called "cwl.output.json",
#' that file must be loaded and used as the output object. Otherwise,
#' the output object must be generated by walking the parameters
#' listed in outputs and applying output bindings to the tool
#' output. Output bindings are associated with output parameters using
#' the outputBinding field. See CommandOutputBinding for details.
#'
#'
#' @field baseCommand (character) Specifies the program to execute. If
#' the value is an array, the first element is the program to execute,
#' and subsequent elements are placed at the beginning of the command
#' line in prior to any command line bindings. If the program includes
#' a path separator character it must be an absolute path, otherwise
#' it is an error. If the program does not include a path separator,
#' search the $PATH variable in the runtime environment find the
#' absolute path of the executable.
#'
#' @field arguments [characterORCommandLineBinding] Command line
#' bindings which are not directly associated with input parameters.
#'
#' @field stdin [characterORExpression] A path to a file whose
#' contents must be piped into the command's standard input stream.
#'
#' @field stdout [characterORExpression] Capture the command's
#' standard output stream to a file written to the designated output
#' directory. If stdout is a string, it specifies the file name to
#' use.If stdout is an expression, the expression is evaluated and
#' must return a string with the file name to use to capture
#' stdout. If the return value is not a string, or the resulting path
#' contains illegal characters (such as the path separator /) it is an
#' error.
#'
#' @field successCodes [integer] Exit codes that indicate the process
#' completed successfully.
#'
#' @field temporaryFailCodes [integer] Exit codes that indicate the
#' process failed due to a possibly temporary condition, where
#' excuting the process with the same runtime environment and inputs
#' may produce different results.
#'
#' @field permanentFailCodes [integer] Exit codes that indicate the
#' process failed due to a permanent logic error, where excuting the
#' process with the same runtime environment and same inputs is
#' expected to always fail.
#'
#' @export CommandLineTool
#' @exportClass CommandLineTool
#' @rdname CommandLineTool
#'
#' @examples
#' ipl <- InputParameterList(
#'     InputParameter(id = "BAM", type = "File",
#'                    label = "input bam",
#'                    description = "input bam",
#'                    inputBinding = CommandLineBinding(
#'                        position = 1L
#'                    )),
#'     InputParameter(id = "level", type = "Integer",
#'                    label = "Compression level",
#'                    description = "Compression level",
#'                    inputBinding = CommandLineBinding(
#'                        position = 2L,
#'                        prefix = "-l"
#'                    ))
#' )
#'
#' clt <- CommandLineTool(inputs = ipl, baseCommand = "samtools sort")
CommandLineTool <- setRefClass(
    "CommandLineTool", contains = "Process",
    fields = list(
        class              = "character",
        baseCommand        = "characterORlistORNULL",
        arguments          = "characterORCommandLineBindingList",
        stdin              = "characterORExpressionORNULL",
        stdout             = "characterORExpressionORNULL",
        successCodes       = "listORNULL",
        temporaryFailCodes = "listORNULL",
        permanentFailCodes = "listORNULL"
    ),
    method = list(
        initialize = function(
            class       = "CommandLineTool",
            arguments   = "",
            baseCommand = NULL, ...) {

            # if (is.null(baseCommand)) {
            #     stop("baseCommand has to be provided")
            # }
            if (!is.list(baseCommand)) {
                if (is.character(baseCommand)) {
                    baseCommand <<- list(baseCommand)
                }
            }
            if (is.character(arguments)) {
                if (nchar(arguments)) {
                    arguments <<- CCBList(
                        CommandLineBinding(
                            valueFrom = arguments
                        ))
                }
            } else if (is(arguments, "CommandLineBinding")) {
                arguments <<- CCBList(arguments)
            } else if (is.list(arguments)) {
                # need to construct CLB list
                arguments <<- do.call(CCBList, lapply(arguments, function(x) {
                    do.call(CLB, x)
                }))
            } else {
                arguments <<- arguments
            }

            class <<- class
            callSuper(...)

        }
    ))

#' CommandInputParameter Class
#'
#' An input parameter for a CommandLineTool.
#'
#' @export CommandInputParameter
#' @exportClass CommandInputParameter
#'
#' @examples
#' ipl <- InputParameterList(
#'     CommandInputParameter(id = "BAM", type = "File",
#'                           label = "input bam",
#'                           description = "input bam",
#'                           inputBinding = CommandLineBinding(
#'                               position = 1L
#'                           )),
#'     CommandInputParameter(id = "level", type = "Integer",
#'                           label = "Compression level",
#'                           description = "Compression level",
#'                           inputBinding = CommandLineBinding(
#'                               position = 2L,
#'                               prefix = "-l"
#'                           ))
#' )
CommandInputParameter <-
    setRefClass("CommandInputParameter", contains = "InputParameter")

#' CommandInputSchema Class
#'
#' @export CommandInputSchema
#' @exportClass CommandInputSchema
#' @examples
#' CommandInputSchema()
CommandInputSchema <-
    setRefClass("CommandInputSchema", contains = "InputSchema")

#' CommandOutputBinding Class
#'
#' Describes how to generate an output parameter based on the files
#' produced by a CommandLineTool. The output parameter is generated
#' by applying these operations in the following order:
#' glob, loadContents, outputEval.
#'
#' @field glob [characterORExpression] Find files relative to the
#' output directory, using POSIX glob(3) pathname matching. If
#' provided an array, match all patterns in the array. If provided an
#' expression, the expression must return a string or an array of
#' strings, which will then be evaluated as a glob pattern. Only files
#' which actually exist will be matched and returned.
#'
#' @field outputEval [Expression] Evaluate an expression to generate
#' the output value. If glob was specified, the script context will be
#' an array containing any files that were matched. Additionally, if
#' loadContents is true, the file objects will include up to the first
#' 64 KiB of file contents in the contents field.
#'
#' @export CommandOutputBinding
#' @exportClass CommandOutputBinding
#'
#' @examples
#' CommandOutputBinding(glob = "*.bam")
CommandOutputBinding <- setRefClass(
    "CommandOutputBinding", contains = "Binding",
    fields = list(
        glob       = "characterORExpressionORNULL",
        outputEval = "ExpressionORNULL")
)

#' CommandOutputSchema
#'
#' @field outputBinding [CommandOutputBinding] Describes how to handle
#' the concrete outputs of a process step (such as files created by a
#' program) and describe them in the process output parameter.
#'
#' @export CommandOutputSchema
#' @exportClass CommandOutputSchema
#'
#' @examples
#' CommandOutputSchema()
CommandOutputSchema <- setRefClass(
    "CommandOutputSchema", contains = "Schema",
    fields = list(outputBinding = "CommandOutputBinding"))

#' CommandOutputParameter Class
#'
#' @field outputBinding [CommandOutputBinding] Describes how to handle
#' the concrete outputs of a process step (such as files created by a
#' program) and describe them in the process output parameter.
#'
#' @export CommandOutputParameter
#' @exportClass CommandOutputParameter
#'
#' @examples
#' CommandOutputParameter(outputBinding = CommandOutputBinding(glob = "*.bam"))
CommandOutputParameter <- setRefClass(
    "CommandOutputParameter", contains = "OutputParameter",
    fields = list(outputBinding = "CommandOutputBinding")
)

#----------------------------------------------------------------------
# Workflow
#----------------------------------------------------------------------

setClass("LinkMergeMethod", contains = "VIRTUAL")

#' @section WorkflowStepInput Class:
#' \describe{
#'
#' The input of a workflow step connects an upstream parameter (from
#' the workflow inputs, or the outputs of other workflows steps) with
#' the input parameters of the underlying process.
#'
#' If the sink parameter is an array, or named in a workflow scatter
#' operation, there may be multiple inbound data links listed in the
#' connect field. The values from the input links are merged depending
#' on the method specified in the linkMerge field. If not specified,
#' the default method is merge_nested:
#'
#' \item{merge_nested}{ The input shall be an array consisting of
#' exactly one entry for each input link. If merge_nested is specified
#' with a single link, the value from the link is wrapped in a
#' single-item list.  }
#'
#' \item{merge_flattened}{ 1) The source and sink parameters must be
#' compatible types, or the source type must be compatible with single
#' element from the "items" type of the destination array
#' parameter. 2) Source parameters which are arrays are concatenated;
#' source parameters which are single element types are appended as
#' single elements.  }
#'
#' Fields:
#'
#' \item{\code{id}}{ (character) A unique identifier for this workflow input
#' parameter.}
#'
#' \item{\code{source}}{[character] Specifies one or more workflow parameters
#' that will provide input to the underlying process parameter.}
#'
#' \item{\code{linkMerge}}{[LineMergeMethod] The method to use to merge
#' multiple inbound links into a single array. If not specified, the
#' default method is merge_nested:}
#'
#' \item{\code{default}}{ [ANY] The default value for this parameter if there
#' is no source field.}
#' }
#'
#' @export WorkflowStepInput
#' @exportClass WorkflowStepInput
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepInput WorkflowStepInput-class
#'
#' @return a WorkflowStep object or subclass object.
WorkflowStepInput <- setRefClass(
    "WorkflowStepInput", contains = "CWL",
    fields = list(
        id        = "characterORNULL",
        source    = "characterORNULL", ## fixme:
        linkMerge = "LinkMergeMethod",
        default   = "ANY"
    ),
    methods = list(
        initialize = function(id = "", source = NULL, default = NULL, ...) {
            id      <<- addIdNum(id)
            default <<- default
            if (is.character(source)) {
                .self$source <<- set_box(source)
            } else if (is.list(source) && is.character(source[[1]])) {
                .self$source  <<- set_box(source[[1]])
            } else {
                .self$source  <<- source
            }
            callSuper(...)
        }
    ))

#' @section WorkflowStepOutput Class:
#' \describe{
#'
#' Associate an output parameter of the underlying process with a
#' workflow parameter. The workflow parameter (given in the id field)
#' be may be used as a source to connect with input parameters of
#' other workflow steps, or with an output parameter of the process.
#'
#' \item{\code{id}}{ (character) A unique identifier for this workflow output
#' parameter. This is the identifier to use in the source field of
#' WorkflowStepInput to connect the output value to downstream
#' parameters.}
#'
#' }
#'
#' @export WorkflowStepOutput
#' @exportClass WorkflowStepOutput
#' @rdname WorkflowStep
#' @aliases WorkflowStepOutput WorkflowStepOutput-class
WorkflowStepOutput <- setRefClass(
    "WorkflowStepOutput", contains = "CWL",
    fields = list(
        id = "characterORNULL"
    ),
    methods = list(
        initialize = function(id = "", ...) {
            id <<- addIdNum(id)
            callSuper(...)
        }
    ))

#' WorkflowStepInputList
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepInputList WorkflowStepInputList-class
#'
#' @export WorkflowStepInputList
#' @exportClass WorkflowStepInputList
WorkflowStepInputList <- setListClass("WorkflowStepInput")

#' WorkflowStepOutputList
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepOutputList WorkflowStepOutputList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowStepOutputList
#' @exportClass WorkflowStepOutputList
WorkflowStepOutputList <- setListClass("WorkflowStepOutput")

#' WorkflowStepList
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepList-class WorkflowStepList
#' @export WorkflowStepList
#' @exportClass WorkflowStepList
WorkflowStepList <- setListClass("WorkflowStep")

#' @section WorkflowOutputParameter Class:
#' \describe{
#' Describe an output parameter of a workflow. The parameter must be
#' connected to one or more parameters defined in the workflow that
#' will provide the value of the output parameter.
#'
#' \item{\code{source}}{ [character] Specifies one or more workflow parameters
#' that will provide this output value.}
#'
#' \item{\code{linkMerge}}{ [LinkMergeMethod] The method to use to merge
#' multiple inbound links into a single array. If not specified, the
#' default method is merge_nested:}
#'}
#' @export WorkflowOutputParameter
#' @exportClass WorkflowOutputParameter
#'
#' @return a Workflow object.
#'
#' @rdname Workflow
#' @aliases WorkflowOutputParameter WorkflowOutputParameter-class
WorkflowOutputParameter <- setRefClass(
    "WorkflowOutputParameter", contains = "OutputParameter",
    fields = list(
        source    = "characterORNULL",
        linkMerge = "LinkMergeMethod"
    ),
    methods = list(
        initialize = function(source = NULL, ...) {
            if (is.character(source)) {
                .self$source <<- set_box(source)
            } else if (is.list(source) && is.character(source[[1]])) {
                .self$source <<- set_box(source[[1]])
            } else {
                .self$source <<- source
            }
            callSuper(...)
        }
    ))

#' @aliases WorkflowOutputParameterList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowOutputParameterList
#' @exportClass WorkflowOutputParameterList
#'
#' @rdname Workflow
WorkflowOutputParameterList <- setListClass("WorkflowOutputParameter", contains = "OutputParameterList")

SBGWorkflowOutputParameter <- setRefClass(
    "SBGWorkflowOutputParameter",
    contains = "WorkflowOutputParameter",
    fields = list(
        "sbg:x"              = "numericORNULL",
        "sbg:y"              = "numericORNULL",
        "sbg:includeInPorts" = "logicalORNULL",
        "required"           = "logicalORNULL",
        "sbg:fileTypes"      = "characterORNULL"
    ),
    methods = list(
        initialize = function(
            x = NULL, y = NULL,
            includeInPorts = TRUE,
            required = FALSE,
            fileTypes = NULL, ...) {
            args <- mget(names(formals()),
                         sys.frame(sys.nframe()))
            nms <- c("x", "y", "includeInPorts")
            for (nm in nms) {
                .self$field(paste0("sbg:", nm), args[[nm]])
            }
            .self$required <<- required
            callSuper(...)
        }
    ))

SBGWorkflowOutputParameterList <- setListClass("SBGWorkflowOutputParameter",
                                               contains = "OutputParameterList")

# setClassUnion("WorkflowOutputParameterListORSBGWorkflowOutputParameterList",
#               c("WorkflowOutputParameterList", "SBGWorkflowOutputParameterList"))

#' Workflow
#'
#' A workflow is a process consisting of one or more steps. Each step
#' has input and output parameters defined by the inputs and outputs
#' fields. A workflow executes as described in execution model.
#'
#' @section Dependencies:
#'
#' Dependencies between parameters are expressed using the source
#' field on workflow step input parameters and workflow output
#' parameters.
#'
#' The source field expresses the dependency of one parameter on
#' another such that when a value is associated with the parameter
#' specified by source, that value is propagated to the destination
#' parameter. When all data links inbound to a given step are
#' fufilled, the step is ready to execute.
#'
#' @section Extensions:
#'
#' ScatterFeatureRequirement and SubworkflowFeatureRequirement are
#' available as standard extensions to core workflow semantics.
#'
#' @field outputs (WorkflowOutputParameterList) Defines the parameters
#' representing the output of the process. May be used to generate
#' and/or validate the output object. Inherited from Process
#'
#' @field steps (WorkflowStepList) The individual steps that make up the
#' workflow. Steps are executed when all input data links are
#' fufilled. An implementation may choose to execute the steps in a
#' different order than listed and/or execute steps concurrently,
#' provided that dependencies between steps are met.
#'
#' @export Workflow
#' @exportClass Workflow
#'
#' @rdname Workflow
#' @examples
#' ## need better examples here
#' ws <- WorkflowStepList(WorkflowStep(id = "step1", label = "align-and-sort",
#'              description = "align and sort",
#'              inputs = WorkflowStepInputList(
#'                  WorkflowStepInput(id = "id1"),
#'                  WorkflowStepInput(id = "id2")
#'              )))
#' Workflow(steps = ws)
Workflow <- setRefClass(
    "Workflow", contains = "Process",
    fields = list(
        class   = "character",
        outputs = "OutputParameterList",
        # outputs = "WorkflowOutputParameterListORSBGWorkflowOutputParameterList",
        steps = "WorkflowStepList"
    ),
    method = list(
        initialize = function(class = "Workflow", ...) {
            class <<- class
            callSuper(...)
        }
    ))

setClass("ScatterMethod")

setClassUnion("CommandLineToolORExpressionToolORWorkflow",
              c("CommandLineTool", "ExpressionTool", "Workflow"))

#' WorkflowStep Class
#'
#' A workflow step is an executable element of a workflow.
#' It specifies the underlying process implementation (such as
#' CommandLineTool) in the run field and connects the input and output
#' parameters of the underlying process to workflow parameters.
#'
#' @section Scatter/gather:
#'
#' To use scatter/gather, ScatterFeatureRequirement must be specified
#' in the workflow or workflow step requirements.
#'
#' A "scatter" operation specifies that the associated workflow step
#' or subworkflow should execute separately over a list of input
#' elements. Each job making up a scatter operaution is independent
#' and may be executed concurrently.
#'
#' The scatter field specifies one or more input parameters which will
#' be scattered. An input parameter may be listed more than once. The
#' declared type of each input parameter is implicitly wrapped in an
#' array for each time it appears in the scatter field. As a result,
#' upstream parameters which are connected to scattered parameters may
#' be arrays.
#'
#' All output parameters types are also implicitly wrapped in arrays;
#' each job in the scatter results in an entry in the output array.
#'
#' If scatter declares more than one input parameter, scatterMethod
#' describes how to decompose the input into a discrete set of jobs.
#'
#' \itemize{
#'
#' \item{dotproduct}{ specifies that each the input arrays are aligned
#' and one element taken from each array to construct each job. It is
#' an error if all input arrays are not the same length.}
#'
#' \item{nested_crossproduct}{specifies the cartesian product of the
#' inputs, producing a job for every combination of the scattered
#' inputs. The output must be nested arrays for each level of
#' scattering, in the order that the input arrays are listed in the
#' scatter field.}
#'
#' \item{flat_crossproduct}{specifies the cartesian product of the
#' inputs, producing a job for every combination of the scattered
#' inputs. The output arrays must be flattened to a single level, but
#' otherwise listed in the order that the input arrays are listed in
#' the scatter field.}
#'
#' }
#'
#' @section Subworkflows:
#'
#' To specify a nested workflow as part of a workflow step,
#' SubworkflowFeatureRequirement must be specified in the workflow or
#' workflow step requirements.
#'
#' @field id [character] The unique identifier for this workflow step.
#'
#' @field inputs (WorkflowStepInputList) Defines the input parameters
#' of the workflow step. The process is ready to run when all required
#' input parameters are associated with concrete values. Input
#' parameters include a schema for each parameter and is used to
#' validate the input object, it may also be used build a user
#' interface for constructing the input object.
#'
#' @field outputs (WorkflowStepOutputList) Defines the parameters
#' representing the output of the process. May be used to generate
#' and/or validate the output object.
#'
#' @field requirements [ProcessRequirement] Declares requirements that
#' apply to either the runtime environment or the workflow engine that
#' must be met in order to execute this workflow step. If an
#' implementation cannot satisfy all requirements, or a requirement is
#' listed which is not recognized by the implementation, it is a fatal
#' error and the implementation must not attempt to run the process,
#' unless overridden at user option.
#'
#' @field hints [ANY] Declares hints applying to either the runtime
#' environment or the workflow engine that may be helpful in executing
#' this workflow step. It is not an error if an implementation cannot
#' satisfy all hints, however the implementation may report a warning.
#'
#' @field label [character] A short, human-readable label of this
#' process object.
#'
#' @field description [character] A long, human-readable description
#' of this process object.
#'
#' @field run (CommandLineToolORExpressionToolORWorkflow) Specifies
#' the process to run.
#'
#' @field scatter [character]
#'
#' @field scatterMethod [ScatterMethod] Required if scatter is an array of more
#' than one element.
#'
#' @export WorkflowStep
#' @exportClass WorkflowStep
#' @rdname WorkflowStep
#' @aliases WorkflowStep WorkflowStep-class
#'
#' @examples
#' ws <- WorkflowStepList(WorkflowStep(id = "step1", label = "align-and-sort",
#'              description = "align and sort",
#'              inputs = WorkflowStepInputList(
#'                  WorkflowStepInput(id = "id1"),
#'                  WorkflowStepInput(id = "id2")
#'              )))
WorkflowStep <- setRefClass(
    "WorkflowStep", contains = "CWL",
    fields = list(
        id            = "characterORNULL",
        inputs        = "WorkflowStepInputList",
        outputs       = "WorkflowStepOutputList",
        requirements  = "ProcessRequirement",
        hints         = "ProcessRequirementList",
        label         = "characterORNULL",
        description   = "characterORNULL",
        run           = "CommandLineToolORExpressionToolORWorkflow",
        scatter       = "characterORNULL",
        scatterMethod = "ScatterMethod"),
    methods = list(
        initialize = function(id = "", scatter = NULL, ...) {
            id <<- addIdNum(id)
            scatter <<- scatter
            callSuper(...)
        }
    ))

# SBG only

#' Shorthand functions for cwl packages constructors
#'
#' Shorthand functions for cwl packages constructors
#'
#' @param type [ANY] Specify valid types of data that may be assigned
#' to this parameter.
#'
#' @param label [character] A short, human-readable label of this
#' parameter object.
#'
#' @param description [character] A long, human-readable description
#' of this parameter object.
#'
#' @param streamable [logical] Currently only applies if type is
#' File. A value of true indicates that the file is read or written
#' sequentially without seeking. An implementation may use this flag
#' to indicate whether it is valid to stream file contents using a
#' named pipe. Default: false.
#'
#' @param default [ANY] The default value for this parameter if not
#' provided in the input object.
#'
#' @param \dots For \code{InputParameter}, it will be
#' passed to [CommandLineBinding], which could be created by command
#' \code{CLB}. For parameters that accepted please check
#' \code{CommandLineBiding} in cwl package. For your convenience, this
#' manual also contain a section for \code{CommandLineBinding}. For
#' \code{OutPar} or \code{OutputParameter}, it will be passed to
#' \code{CommandOutputParameter}. Please check the following section
#' as well.
#'
#' @section Shorthand:
#' CLB <- CommandLineBinding
#' argslist <- CLBList <- CommandLineBindingList
#' COB <- CommandOutputBinding
#' IPList <- InputParameterList
#' OPList <- OutputParameterList
#' InPar <- InputParameter
#' OutPar <- OutputParameter
#'
#' @section CommandLineBinding:
#' \describe{
#'
#' \item{position}{[integer] The sorting key. Default position is 0.}
#'
#' \item{prefix}{[character] Command line prefix to add before the
#' value.}
#'
#' \item{separate}{[logical] If true (default) then the prefix and
#' value must be added as separate command line arguments; if false,
#' prefix and value must be concatenated into a single command line
#' argument.}
#'
#' \item{itemSeparator}{[character] Join the array elements into a
#' single string with the elements separated by by itemSeparator.}
#'
#' \item{valueFrom}{[characterOrExpression] If valueFrom is a constant
#' string value, use this as the value and apply the binding rules
#' above. If valueFrom is an expression, evaluate the expression to
#' yield the actual value to use to build the command line and apply
#' the binding rules above. If the inputBinding is associated with an
#' input parameter, the "context" of the expression will be the value
#' of the input parameter. When a binding is part of the
#' CommandLineTool.arguments field, the valueFrom field is required.
#' }
#'}
#'
#' @section CommandOutputParameter:
#' \describe{
#'
#' \item{glob}{[characterORExpression] Find files relative to the
#' output directory, using POSIX glob(3) pathname matching. If
#' provided an array, match all patterns in the array. If provided an
#' expression, the expression must return a string or an array of
#' strings, which will then be evaluated as a glob pattern. Only files
#' which actually exist will be matched and returned.}
#'
#' \item{outputEval}{[Expression] Evaluate an expression to generate
#' the output value. If glob was specified, the script context will be
#' an array containing any files that were matched. Additionally, if
#' loadContents is true, the file objects will include up to the first
#' 64 KiB of file contents in the contents field.}
#' Following fields inherited from \code{Binding}
#'
#' \item{loadContents}{ [logical] Only applies when type is File. Read
#' up to the first 64 KiB of text from the file and place it in the
#' "contents" field of the file object for manipulation by
#' expressions.}
#'
#' \item{secondaryFiles}{Only applies when type is File. Describes
#' files that must be included alongside the primary file. If the
#' value is Expression, the context of the expression is the input or
#' output File parameter to which this binding applies. Where the
#' value is a string, it specifies that the following pattern should
#' be applied to the primary file: If string begins with one or more
#' caret characters, for each caret, remove the last file extension
#' from the path (the last period . and all following characters). If
#' there are no file extensions, the path is unchanged.  Append the
#' remainder of the string to the end of the file path.}
#'
#'
#' }
#'
#' @rdname shorthand-cwl
#' @name CLB
#' @aliases CLB argslist COB IPList OPList input output InPar OutPar
#'
#'
#' @export CLB argslist COB IPList OPList input output InPar OutPar
#' @examples
#' ipl <- IPList(
#'     input(id = "bam",
#'           type = "File",
#'           label = "Bam file",
#'           description = "Input bam file",
#'           position = 1L,
#'           separate = TRUE),
#'     input(id = "level",
#'           type = "Integer",
#'           label = "Compression Level",
#'           description = "Set compression level, from 0 (uncompressed) to 9 (best)",
#'           position = 2L),
#'     input(id = "prefix",
#'           type = "String",
#'           label = "Prefix",
#'           description = "Write temporary files to PREFIX.nnnn.bam",
#'           position = 3L)
#' )
CLB      <- CommandLineBinding
argslist <- CCBList
COB      <- CommandOutputBinding
IPList   <- InputParameterList
OPList   <- OutputParameterList
InPar    <- InputParameter
OutPar   <- OutputParameter

SCLB <- SBGCommandLineBinding <- setRefClass(
    "SBGCommandLineBinding", contains = "CommandLineBinding",
    fields = list("sbg:cmdInclude" = "logicalORNULL"),
    methods = list(initialize = function(cmdInclude = FALSE, ...) {
        .self$field("sbg:cmdInclude", cmdInclude)
        callSuper(...)
    }))

SBGInputParameter <- setRefClass(
    "SBGInputParameter", contains = "InputParameter",

    fields = list("sbg:category"         = "characterORlistORNULL",
                  "sbg:fileTypes"        = "characterORNULL",
                  "sbg:stageInput"       = "characterORNULL",
                  "sbg:x"                = "numericORNULL",
                  "sbg:y"                = "numericORNULL",
                  "sbg:includeInPorts"   = "logicalORNULL",
                  "sbg:toolDefaultValue" = "characterORNULL",
                  "sbg:altPrefix"        = "characterORNULL",
                  "sbg:suggestedValue"   = "listORNULL",
                  "required"             = "logicalORNULL",
                  "batchType"            = "characterORNULL"),

    methods = list(
        initialize = function(category         = NULL,
                              fileTypes        = NULL,
                              stageInput       = NULL,
                              x                = NULL,
                              y                = NULL,
                              includeInPorts   = NULL,
                              toolDefaultValue = NULL,
                              altPrefix        = NULL,
                              suggestedValue   = NULL,
                              required         = FALSE,
                              batchType        = NULL, ...) {

            if (!is.null(stageInput)) {
                if (!stageInput %in% c("copy", "link")) {
                    stop("stageInput has to be NULL, copy or link")
                }
            }

            .self$field("sbg:stageInput", stageInput)
            .self$field("sbg:category", category)
            .self$field("sbg:fileTypes", fileTypes)
            .self$field("sbg:x", x)
            .self$field("sbg:y", y)
            .self$field("sbg:includeInPorts", includeInPorts)
            .self$field("sbg:toolDefaultValue", toolDefaultValue)
            .self$field("sbg:altPrefix", altPrefix)
            .self$field("sbg:suggestedValue", suggestedValue)
            .self$field("batchType", batchType)
            .self$field("required", required)
            callSuper(...)

        }))

.is_required = function(x) {
    # x is input item
    !(is.character(x$type[[1]]) && x$type[[1]] == "null")
}

input <- function(id = NULL, type = NULL, label = "",
                  description = "", streamable = FALSE,
                  default = "", required = FALSE,
                  category = NULL, fileTypes = NULL,
                  stageInput = NULL,
                  cmdInclude = FALSE, ...) {

    # if (is.null(id)) {
    #     stop("id has to be provided")
    # }

    if (!length(label)) {
        label <- id
    }

    if (is.list(id)) {

        # convert a list to input
        in.lst <- lapply(id, function(o) {

            o.b <- o$inputBinding
            if (is.null(o.b)) {
                ib <- NULL
            } else {
                ib <- do.call(SCLB, o.b)
            }


            o <- c(o[!names(o) %in% c("inputBinding",
                                      "sbg:category",
                                      "required",
                                      "sbg:fileTypes",
                                      "type",
                                      "fileTypes",
                                      "sbg:stageInput")],
                   list(inputBinding = ib,
                        required     = .is_required(o),
                        type         = format_type(o$type),
                        category     = o[["sbg:category"]],
                        fileTypes    = o[["sbg:fileTypes"]],
                        stageInput   = o[["sbg:stageInput"]]))

            do.call(SBGInputParameter, o)

        })

        res.in <- do.call("IPList", in.lst)
        return(res.in)

    }

    if (is.null(type)) {
        stop("type has to be provided: file, enum, string, int, float, boolean, array, record, map")
    }

    type <- deType(type)

    if (length(type) == 1) {
        if (!required) {
            type = c("null", type)
        } else {
            type <- list(type)
        }
    }

    lstData <- .dotargsAsList(...)
    if (length(lstData)) cmdInclude <- TRUE

    if (cmdInclude) {
        SBGInputParameter(id = id, type = type, label = label,
                          description = description,
                          streamable = streamable,
                          default = default, category = category, fileTypes = fileTypes,
                          stageInput = stageInput,
                          inputBinding = SCLB(cmdInclude = cmdInclude, ...))
    } else {
        SBGInputParameter(id = id, type = type, label = label,
                          description = description,
                          streamable = streamable,
                          stageInput = stageInput,
                          default = default, category = category, fileTypes = fileTypes,
                          inputBinding = NULL)
    }

}

SBGCommandOutputBinding <- setRefClass(
    "SBGCommandOutputBinding", contains = "CommandOutputBinding",
    field = list(
        "sbg:inheritMetadataFrom" = "characterORNULL",
        "sbg:metadata"            = "listORNULL"
    ),
    methods = list(
        initialize = function(inheritMetadataFrom = NULL,
                              metadata            = NULL, ...) {

            # args <- mget(names(formals()),sys.frame(sys.nframe()))
            # nms  <- c("metadata", "inheritMetadataFrom")
            .self$field("sbg:metadata", metadata)
            .self$field("sbg:inheritMetadataFrom", addIdNum(inheritMetadataFrom))

            callSuper(...)

        }
    ))
SBGCOB <- SBGCommandOutputBinding

SBGCommandOutputParameter <- setRefClass(
    "SBGCommandOutputParameter", contains = "CommandOutputParameter",
    fields = list(
        "sbg:fileTypes" = "characterORNULL"
    ),
    methods = list(
        initialize = function(fileTypes  = NULL, ...) {
            nm <- "fileTypes"
            .self$field(paste0("sbg:", nm), fileTypes)
            callSuper(...)
        }
    ))

output <- function(id = NULL, type = "file", label = "",
                   description = "",
                   required = FALSE,
                   streamable = FALSE, default = "",
                   fileTypes = NULL, ...) {

    # if (is.null(id)) {
    #     stop("id has to be provided")
    # }

    if (!length(label)) label <- id

    if (is.list(id)) {
        out.lst <- lapply(id, function(o) {

            o.b <- o$outputBinding
            # glob
            if (length(o.b$glob)) {
                if (length(o.b$glob) == 1 && is.character(o.b$glob)) {
                    res.glob <- o.b$glob
                } else {
                    res.glob <- do.call("Expression", o.b$glob)
                }
            } else {
                res.glob <- NULL
            }

            # load Contents
            if (length(o.b$loadContents)) {
                res.load <- o.b$loadContetns
            } else {
                res.load <- NULL
            }
            #
            if (length(o.b$outputEval)) {
                if (length(o.b$outputEval) == 1 &&
                    is.character(o.b$outputEval)) {
                    res.eval <- o.b$outputEval
                } else {
                    res.eval <- do.call("Expression", o.b$outputEval)
                }
            } else {
                res.eval <- NULL
            }

            ob <- SBGCOB(glob                = res.glob,
                         loadContents        = res.load,
                         outputEval          = res.eval,
                         inheritMetadataFrom = o$`sbg:inheritMetadataFrom`,
                         metadata            = o$`sbg:metadata`,
                         secondaryFiles      = o$seconaryFiles)

            o <- c(o[!names(o) %in%
                         c("sbg:fileTypes",
                           "outputBinding",
                           "type",
                           "fileTypes",
                           "sbg:inheritMetadataFrom",
                           "sbg:metadata")],
                   list(type = format_type(o$type),
                        outputBinding = ob,
                        fileTypes = o[["sbg:fileTypes"]]))

            do.call(SBGCommandOutputParameter, o)

        })

        res.out <- do.call("OPList", out.lst)
        return(res.out)

    }

    # if (is.null(type)) {
    #     stop("type has to be provided: file, enum, string, int, float, boolean, array, record, map")
    # }

    type <- deType(type)

    if (length(type) == 1) {
        if (!required) {
            type = c("null", type)
        } else {
            type <- list(type)
        }
    }

    SBGCommandOutputParameter(id = id, type = type, label = label,
                              description = description,
                              streamable = streamable,
                              default = default,
                              fileTypes = fileTypes,
                              outputBinding = SBGCOB(...))

}

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
CPURequirement <- setRefClass(
    "CPURequirement", contains = "ProcessRequirement",
    fields = list(
        value = "integerORExpression"
    ),
    methods = list(
        initialize = function(value = 1L,
                              class = "sbg:CPURequirement", ...) {
            class <<- class
            if (is.numeric(value)) {
                .v <- as.integer(value)
            } else {
                .v <- do.call(Expression, value)
            }

            # # comment out this, conform to server requirements
            # if (!.v %in% c(1L, 0L)) {
            #     warning("For now, CPU value must be 0L (multi-treads) or 1L (single-thread)")
            #     if (.v > 0) {
            #         message("Convert CPU value ", .v, " to ", 1L)
            #         .v <- 1L
            #     }
            # }
            value <<- .v
            callSuper(...)
        }
    ))

cpu <- CPURequirement

#' @rdname requirements
#' @aliases docker
#' @param pull [short form argument] Docker Repository[:Tag] like rocker/r-base
#' @param imageId [short form argument] The image id that will be used for
#' docker run, imageId Optionally set the id of image you get from SDK.
#' @param load [short form argument] Specify a HTTP URL from which to download
#' a Docker image using docker load.
#' @param file [short form argument] Supply the contents of a Dockerfile
#' which will be built using docker build.
#' @param output [short form argument] Set the designated output directory
#' to a specific location inside the Docker container.
#' @param dockerPull Docker Repository[:Tag] like rocker/r-base
#' @param dockerImageId The image id that will be used for docker run,
#' imageId Optionally set the id of image you get from SDK.
#' @param dockerLoad Specify a HTTP URL from which to download a Docker
#' image using docker load.
#' @param dockerFile Supply the contents of a Dockerfile which will be
#' built using docker build.
#' @param dockerOutputDirectory Set the designated output directory to
#' a specific location inside the Docker container.
#' @param ... extra aguments passed
#' @return A Requirement subclass.
#' @export docker
#' @examples
#' docker("rocker/r-base")
docker <- function(pull = NULL, imageId = NULL, load = NULL,
                   file = NULL, output = NULL,
                   dockerPull = pull,
                   dockerImageId = imageId,
                   dockerLoad = load,
                   dockerFile = file,
                   dockerOutputDirectory = output, ...) {

    DockerRequirement(
        dockerImageId = dockerImageId,
        dockerPull = dockerPull,
        dockerLoad = dockerLoad,
        dockerFile = dockerFile,
        dockerOutputDirectory = dockerOutputDirectory, ...)

}

#' requirements and hints
#'
#' requirements and hints
#'
#' It constructs ProesssRequirementList object, or from a returned raw list contains or requirements.
#'
#' @rdname requirements
#' @export requirements
#' @examples
#' requirements(docker("rocker/r-base"), cpu(1), mem(1024))
requirements <- function(...) {
    listData <- .dotargsAsList(...)
    idx.fd <- sapply(listData, is, "FileDef")
    if (all(idx.fd)) {
        return(ProcessRequirementList(CreateFileRequirement(fileDef = FileDefList(listData))))
    } else if (sum(idx.fd)) {
        fdef <- listData[idx.fd]
        fdef <- CreateFileRequirement(fileDef = FileDefList(fdef))
    } else {
        fdef <- NULL
    }

    # process
    listData <- lapply(listData[!idx.fd], function(x){
        # check to see if it's a convertable class
        if ("class" %in% names(x)) {
            cls <- x$class
            switch(cls,
                   "DockerRequirement" = {
                       return(do.call(docker, x))
                   },
                   "sbg:CPURequirement" = {
                       return(do.call(cpu, x))
                   },
                   "sbg:MemRequirement" = {
                       return(do.call(mem, x))
                   },
                   "SubworkflowFeatureRequirement" = {
                       return(do.call("SubworkflowFeatureRequirement", x))
                   },
                   "CreateFileRequirement" = {
                       lst <- lapply(x$fileDef, function(f) {
                           do.call(FileDef,f)
                       })
                       return(CreateFileRequirement(fileDef = FileDefList(lst)))
                   },
                   "EnvVarRequirement" = {
                       lst <- lapply(x$envDef, function(f) {
                           do.call(EnvironmentDef, f)
                       })
                       return(EnvVarRequirement(envDef = EnvironmentDefList(lst)))
                   },
                   "ScatterFeatureRequirement" = {
                       return(do.call("ScatterFeatureRequirement", x))
                   },
                   "ExpressionEngineRequirement" = {
                       req <- requirements(x$requirements)
                       res <- ExpressionEngineRequirement(
                           id = x$id,
                           requirements = req,
                           engineCommand = x$engineCommand,
                           engineConfig = x$engineConfig)
                       return(res)
                   },
                   "sbg:AWSInstanceTypeRequirement" = {
                       return(do.call(aws, x))
                   },
                   {
                       return(do.call(anyReq, x))
                   })

        }
        # work with file def
        if (is.list(x)) {
            if (all(sapply(x, is, "FileDef"))) {
                return(CreateFileRequirement(fileDef = FileDefList(x)))
            } else {
                stop("not all FileDefList are FileDef object")
            }
        } else {
            return(x)
        }
    })

    # validation
    idx <- sapply(listData, function(x) {
        is(x, "ProcessRequirement")
    })
    if (!all(idx)) {
        print(listData[!idx])
        stop("Has to be ProcessRequirement class, use docker(), cpu(), mem(), fileDef(), function to help")
    }
    listData <- c(fdef, listData)
    ProcessRequirementList(listData)
}

#' @param name file name
#' @param content file content, could be script
#'
#' @rdname requirements
#' @aliases fileDef
#' @export fileDef
fileDef <- function(name = NULL, content = NULL) {
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
MemRequirement <- setRefClass(
    "MemRequirement", contains = "ProcessRequirement",
    fields = list(
        value = "integerORExpression"
    ),
    methods = list(
        initialize = function(value = 1000L,
                              class = "sbg:MemRequirement", ...) {

            if (is.numeric(value)) {
                .v <- as.integer(value)
            } else {
                .v <- do.call(Expression, value)
            }
            value <<- .v
            class <<- class
            callSuper(...)
        }
    ))

mem <- MemRequirement

#' @rdname requirements
#' @aliases AWSInstanceTypeRequirement AWSInstanceTypeRequirement-class aws
#' @export AWSInstanceTypeRequirement aws
#' @exportClass AWSInstanceTypeRequirement
#' @examples
#' aws("c3.8xlarge")
AWSInstanceTypeRequirement <- setRefClass(
    "AWSInstanceTypeRequirement", contains = "ProcessRequirement",
    fields = list(
        value = "characterORNULL"
    ),
    methods = list(
        initialize = function(value = NULL,
                              class = "sbg:AWSInstanceType", ...){
            value <<- value
            class <<- class
            callSuper(...)
        }
    ))

aws <- AWSInstanceTypeRequirement

#' @rdname requirements
#' @aliases AnyRequirement AnyRequirement-class anyReq
#' @export AnyRequirement anyReq
#' @exportClass AnyRequirement
#' @examples
#' anyReq("any")
AnyRequirement <- setRefClass(
    "AnyRequirement", contains = "ProcessRequirement",
    fields = list(
        value = "ANY"
    ),
    methods = list(
        initialize = function(value = NULL,
                              class = "", ...) {
            value <<- value
            class <<- class
            callSuper(...)
        }
    ))

anyReq <- AnyRequirement

SBGStep <- setRefClass(
    "SBGStep", contains = "WorkflowStep",
    fields = list("sbg:x" = "numericORNULL",
                  "sbg:y" = "numericORNULL"),
    methods = list(
        initialize = function(x = NULL, y = NULL, ...) {
            args <- mget(names(formals()),
                         sys.frame(sys.nframe()))
            nms <- c("x", "y")
            for (nm in nms) {
                .self$field(paste0("sbg:", nm), args[[nm]])
            }
            callSuper(...)
        }
    ))

SBGStepList <- setListClass("SBGStep", contains = "WorkflowStepList")

setAs("SBGInputParameter", "data.frame", function(from) {

    lst = from$toList()
    ib = lst$inputBinding
    res =  c(lst[!names(lst) %in% c("inputBinding",
                                    "sbg:category",
                                    "required",
                                    "sbg:fileTypes",
                                    "type", "fileTypes",
                                    "sbg:stageInput")],
             list(
                 # inputBinding = ib,
                 required   = .is_required(lst),
                 type       = .make_type(lst$type),
                 category   = lst[["sbg:category"]],
                 fileTypes  = lst[["sbg:fileTypes"]],
                 stageInput = lst[["sbg:stageInput"]]),
             ib)

    res = lapply(res, function(x) {
        if (is.null(x))
            return("null")
        else
            return(x)
    })

    res = do.call(data.frame, res)
    .fullnames = names(res)

    .names.sbg      = sort(.fullnames[grep("^sbg", .fullnames)])
    .names.other    = sort(setdiff(.fullnames, .names.sbg))
    .names.priority = c("id", "type", "required", "fileTypes", "label")
    .names.p2       = sort(setdiff(.names.other, .names.priority))
    new.order       = c(.names.priority, .names.p2, .names.sbg)

    res[, new.order]

})

setAs("InputParameterList", "data.frame", function(from) {
    lst = lapply(from, function(x) {
        as(x, "data.frame")
    })
    res = do.call("bind_rows", lst)
    # reorder for File File...
    idx = res$type %in% c("File", "File...")
    res1 = res[idx, ]
    res2 = res[!idx, ]
    rbind(res1, res2)
})

setAs("SBGCommandOutputParameter", "data.frame", function(from) {

    lst = from$toList()

    o.b <- lst$outputBinding
    # glob
    if (length(o.b$glob) == 1 && is.character(o.b$glob)) {
        res.glob <- o.b$glob
    } else {
        res.glob <- o.b$glob$script
    }
    # load Contents
    if (length(o.b$loadContents)) {
        res.load <- o.b$loadContetns
    } else {
        res.load <- NULL
    }
    #
    if (length(o.b$outputEval)) {
        if (length(o.b$outputEval) == 1 &&
            is.character(o.b$outputEval)){
            res.eval <- o.b$outputEval
        } else {
            res.eval <- o.b$outputEval$script
        }
    } else {
        res.eval <- NULL
    }
    ob <- list(glob = res.glob,
               loadContents = res.load,
               outputEval = res.eval,
               inheritMetadataFrom = lst$`sbg:inheritMetadataFrom`,
               metadata = lst$`sbg:metadata`,
               secondaryFiles = lst$seconaryFiles)

    res =  c(lst[!names(lst) %in% c("sbg:fileTypes",
                                    "outputBinding",
                                    "type",
                                    "fileTypes",
                                    "sbg:inheritMetadataFrom",
                                    "sbg:metadata")],
             list(type = .make_type(lst$type),

                  fileTypes = lst[["sbg:fileTypes"]]), ob)

    res = lapply(res, function(x) {
        if (is.null(x))
            return("null")
        else
            return(x)
    })

    res = do.call(data.frame, res)
    .fullnames = names(res)

    .names.sbg      = sort(.fullnames[grep("^sbg", .fullnames)])
    .names.other    = sort(setdiff(.fullnames, .names.sbg))
    .names.priority = c("id", "label", "type")
    .names.p2       = sort(setdiff(.names.other, .names.priority))
    new.order       = c(.names.priority, .names.p2, .names.sbg)

    res[, new.order]

})

setAs("OutputParameterList", "data.frame", function(from) {
    lst = lapply(from, function(x) {
        as(x, "data.frame")
    })
    res  = do.call("bind_rows", lst)
    # reorder for File File...
    idx  = res$type %in% c("File", "File...")
    res1 = res[idx, ]
    res2 = res[!idx, ]
    rbind(res1, res2)
})

setAs("SBGWorkflowOutputParameter", "data.frame", function(from) {

    lst = from$toList()

    res =  c(lst[!names(lst) %in% c("sbg:fileTypes",
                                    "type",
                                    "fileTypes",
                                    "sbg:inheritMetadataFrom",
                                    "sbg:metadata")],
             list(type = .make_type(lst$type),

                  fileTypes = lst[["sbg:fileTypes"]]))

    res = lapply(res, function(x) {
        if (is.null(x))
            return("null")
        else
            return(x)
    })

    res = do.call(data.frame, res)
    .fullnames = names(res)

    .names.sbg      = sort(.fullnames[grep("^sbg", .fullnames)])
    .names.other    = sort(setdiff(.fullnames, .names.sbg))
    .names.priority = c("id", "label", "type")
    .names.p2       = sort(setdiff(.names.other, .names.priority))
    new.order       = c(.names.priority, .names.p2, .names.sbg)

    res[, new.order]

})

setAs("SBGWorkflowOutputParameterList", "data.frame", function(from) {

    lst = lapply(from, function(x) {
        as(x, "data.frame")
    })
    res  = do.call("bind_rows", lst)
    # reorder for File File...
    idx  = res$type %in% c("File", "File...")
    res1 = res[idx, ]
    res2 = res[!idx, ]
    rbind(res1, res2)

})
