.getFields <- function(x, values) {
    # from Martin's code
    flds = names(x$getRefClass()$fields())
    if (!missing(values)) flds = flds[flds %in% values]
    result = setNames(vector("list", length(flds)), flds)
    for (fld in flds) result[fld] = list(x[[fld]])
    result
}

stopifnot_provided = function(..., msg = "is not provided") {

    n <- length(ll <- list(...))
    if (n == 0) return(invisible())
    mc <- match.call()
    x = NULL
    for (i in 1:n) {
        if (!(is.logical(r <- eval(ll[[i]])) && all(r))) {
            l <- mc[[i+1]][[2]]
            x <- c(x, deparse(l[[length(l)]]))
        }
    }

    if (length(x)) stop(paste(paste(x, collapse = ","), msg), call. = FALSE)

}

m.fun = function(x, y, exact = TRUE, ignore.case = TRUE, ...) {

    if (exact) {
        res = pmatch(x, y, ...)
    } else {
        res = unlist(sapply(x, function(i) {
            grep(i, y, ignore.case = ignore.case)
        }))
        if (is.matrix(res)) res = res[, 1]
    }

    res

}

# match by id and name
m.match <- function(obj,
                    id = NULL,  name = NULL,
                    .id = "id", .name = "name",
                    exact = TRUE, ignore.case = TRUE) {

    # if no match, return whole list
    if (is.null(id)) {
        if (is.null(name)) {
            if (length(obj) == 1) {
                return(obj[[1]])
            } else {
                return(obj)
            }

        } else {
            # id is null, use name
            nms <- sapply(obj, function(x) x[[.name]])
            if (ignore.case) {
                name <- tolower(name)
                nms  <- tolower(nms)
            }
            index <- m.fun(name, nms, exact = exact,
                           ignore.case = ignore.case)
        }
    } else {
        # id is not NULL
        ids <- sapply(obj, function(x) x[[.id]])
        index <- m.fun(id, ids, exact = exact,
                       ignore.case = ignore.case)
    }

    if (length(index) == 1 && is.na(index)) {
        message("sorry, no matching ")
        return(NULL)
    } else {
        if (length(index) ==1) {
            obj[[index]]
        } else {
            obj[index]
        }
    }

}

# workaround for knitr print `message()` position issues
message2 = function(...) cat(paste0(..., "\n"))

.showFields = function(x, title = NULL, values = NULL,
                       full = FALSE, con.char = " / ") {

    if (missing(values)) {
        flds = names(x$getRefClass()$fields())
    } else {
        flds = values
    }

    if (!length(x)) return(NULL)

    if (!full) {
        idx <- sapply(flds, is.null)
        if (!is.null(title) && !all(idx)) message2(title)

        # ugly, change later
        for (fld in flds[!idx]) {
            if (is.list(x[[fld]])) {
                if (length(x[[fld]])) {
                    message2(fld, ":")
                    .showList(x[[fld]], space =  "  ")
                }
            } else if (is(x[[fld]], "Item")) {
                x[[fld]]$show()
            } else {
                if (is.character(x[[fld]])) {
                    if (x[[fld]] != "" && length(x[[fld]])) {
                        message2(fld, " : ", paste0(x[[fld]], collapse = con.char))
                    }
                } else {
                    if (!is.null(x[[fld]]) && length(x[[fld]]))
                        message2(fld, " : ", x[[fld]])
                }
            }
        }
    } else {
        message2(title)
        # ugly, change later
        for (fld in flds) {
            if (is.list(x[[fld]])) {
                message2(fld, ":")
                .showList(x[[fld]], space =  "  ", full = full)
            } else if (is(x[[fld]], "Item")) {
                x[[fld]]$show()
            } else {
                if (is.character(x[[fld]])) {
                    message2(fld, " : ", paste0(x[[fld]], collapse = con.char))
                } else {
                    message2(fld, " : ", x[[fld]])
                }
            }
        }
    }
}

# full = TRUE, show empty filed as well
.showList <- function(x, space = "", full = FALSE) {

    if (length(x)) {

        if (all(sapply(x, is.list))) {
            sapply(x, .showList, space = paste0(space, ""))
            return(invisible())
        }

        if (!full) {
            idx <- sapply(x, function(s) {
                if (is.character(s)) {
                    idx <- nchar(s)
                } else {
                    idx <- TRUE
                }
                !is.null(s) && idx
            })
            x <- x[which(idx)]
        }

        for (i in seq_len(length(x))) {
            fld <- names(x[i])
            if (all(is.character(x[[i]]))) {
                msg <- paste0(x[[i]], collapse = " \n ")
                if (is.null(fld)) {
                    message2(space, " - ",  msg)
                } else {
                    message2(space, fld, " : ", msg)
                }

            } else {

                if (is(x[[i]], "Meta")) {
                    msg <- as.character(x[[i]]$data)
                    if (is.null(fld)) {
                        message2(space, " - ",  msg)
                    } else {
                        message2(space, fld, " : ", msg)
                    }

                } else if (is.list(x[[i]])) {
                    if (is.null(fld)) {
                        message2(space, " - ", length(x[[i]]), " items")
                    } else {
                        message2(space, fld, " : ", length(x[[i]]), " items")
                    }
                    .showList(x[[i]], space = paste0(space, "  "))
                } else {
                    msg <- as.character(x[[i]])
                    if (is.null(fld)) {
                        message2(space, " - ",  msg)
                    } else {
                        message2(space, fld, " : ", msg)
                    }
                }

            }
        }

    }

}

.update_list <- function(o, n) {

    o.nm <- names(o)
    n.nm <- names(n)
    i.nm <- intersect(o.nm, n.nm)

    if (length(i.nm)) {
        o.nm <- setdiff(o.nm, i.nm)
        c(o[o.nm], n)
    } else {
        c(o, n)
    }

}

# guess version based on URL and save it
.ver <- function(url) str_match(url, "https://.*/(.*)/$")[, 2]

# parse an item from a v2 request object
parseItem <- function(x) {

    obj <- x$items
    attr(obj, "href") <- x$href
    attr(obj, "response") <- x$response
    obj

}

hasItems <- function(x) "items" %in% names(x)

ptype <- function(x) ifelse(grepl("\\/", x), "v2", "1.1")

isv2 <- function(version) version == "v2"

v2Check <- function(version,
                    msg = "This function only supported in API V2") {

    if (version != "v2") stop(msg, call. = FALSE)

}

# a quick fix for List class
Item0 <- setClass("Item0",
                  slots = list(href = "characterORNULL", response = "ANY"))

# A function from class-cwl.R

#' List Class generator.
#'
#' Extends IRanges SimpleList class and return constructor.
#'
#' @param elementType [character]
#' @param suffix [character] default is "List"
#' @param contains [character] class name.
#' @param where environment.
#' @return S4 class constructor
setListClass <- function(elementType = NULL, suffix = "List",
                         contains = NULL, where = topenv(parent.frame())) {

    stopifnot(is.character(elementType))
    name <- paste0(elementType, suffix)
    setClass(name, contains = c("SimpleList", contains), where = where,
             prototype = prototype(elementType = elementType))
    setMethod("show", name, function(object) {
        if (length(object)) {
            for(i in 1:length(object)) {
                message2("[[", i, "]]")
                show(object[[i]])
            }
        }
    })

    # constructor
    function(...) {
        listData <- .dotargsAsList(...)
        return(eval(parse(text = "S4Vectors:::new_SimpleList_from_list(name, listData)")))
    }

}

# Function from IRanges
.dotargsAsList <- function(...) {
    listData <- list(...)
    if (length(listData) == 1) {
        arg1 <- listData[[1]]
        if (is.list(arg1) || is(arg1, "List"))
            listData <- arg1
        # else if (type == "integer" && class(arg1) == "character")
        # listData <- strsplitAsListOfIntegerVectors(arg1) # weird special case
    }
    listData
}

.update_revision <- function(id, revision = NULL) {

    if (!is.null(revision)) {
        if (grepl("/[0-9]+$", id)) {
            res <- gsub("/[0-9]+$", revision, id, perl = TRUE)
        } else {
            id = gsub("/$", "", id)
            res <- paste0(id, "/", revision)
        }
    } else {
        res <- id
    }

    res

}

validateApp <- function(req) {
    res <- content(req)$raw[["sbg:validationErrors"]]
    if (length(res)) {
        message("App pushed but cannot be ran, because it doesn't pass validation")
        lapply(res, function(x) stop(x))
    }
}

.flowsummary <- function(a, id, revision = NULL, includeFile = FALSE) {
    # developed for Andrew : )
    app <- a$app(id = id, revision = revision)
    cwl <- app$raw
    .name <- app$name
    if (cwl$class == "Workflow") {
        .arg <- sum(sapply(cwl$steps, function(x) {
            ins <- x$run$input
            if (includeFile) {
                length(ins)
            } else {
                idx = sapply(ins, function(i) {
                    any(sapply(i$type, function(tp) {
                        "File" %in% tp
                    }))
                })
                if (sum(idx)) {
                    length(ins[!idx])
                } else {
                    length(ins)
                }
            }
        }))
        .tool <- length(cwl$steps)
        message("name: ", .name)
        message("id: ", id)
        message("Total tool: ", .tool)
        message("Total arguments: ", .arg)
        c('id' = id, 'tool' = .tool, 'arg' = .arg, 'name' = .name)
    } else {
        return(NULL)
    }
}

iterId <- function(ids, fun, ...) {
    res <- lapply(ids, function(id) {
        fun(id = id, ...)
    })
    # try convert it into a simple list
    .class <- class(res[[1]])
    .newclass <- paste0(.class, "List")
    if (!is.null(tryNew(.newclass,where = topenv(parent.frame())))) {
        # exists
        res <- do.call(.newclass, res)
    }
    res
}

#' Set testing env
#'
#' Checks if docker is installed, is running and has
#' required images downloaded and if do creates container
#'
#' @param type "dind" or "host"
#' @param docker_image required docker image with
#' pre-installed bunny, default: tengfei/testenv
#' @param data_dir directory with data to mount
#' (also will be execution directory)
#' @export set_test_env
#' @return docker stdout
#' @examples
#' \dontrun{
#' set_test_env("dind", "tengfei/testenv", "/Users/<user>/tools")}

set_test_env = function(type = "host",
                        docker_image = "tengfei/testenv",
                        data_dir = getwd()) {

    stopifnot(type %in% c("dind", "host"))

    switch(Sys.info()[['sysname']],
           Windows= {
               message("[INFO]: Windows OS detected: trying native docker support or docker-machine ...")
               .docker_env_vars()
           },
           Linux  = {
               message("[INFO]: Linux OS detected: trying docker command, if failed please ensure server and client are connected")
               .test_docker_setup()
           },
           Darwin = {
               message("[INFO]: macOS detected: trying native docker support or docker-machine ...")
               .docker_env_vars()
           }
    )

    # cleanup
    container.name <- .set_container_name()
    system2("docker", c("pull", docker_image), stdout = FALSE, stderr = FALSE)
    system2("docker", c("rm -f", container.name), stdout = FALSE, stderr = FALSE)
    #system2("docker", "rm $(docker ps -aq -f status=exited -f status=created)", stdout = TRUE, stderr = TRUE)
    system2("docker", "volume rm $(docker volume ls -qf dangling=true)", stdout = FALSE, stderr = FALSE)

    # start container as docker-in-docker (dind) or docker-beside-docker (host)
    if (type == "dind"){
        docker_run_args <- paste("run --privileged --name ", container.name, " -v ", data_dir, ":/bunny_data -dit ", docker_image, sep="")
        system2("docker", c(docker_run_args), stdout = TRUE, stderr = TRUE)
        system2("docker", c("exec", container.name, "bash -c 'service docker start'"), stdout = TRUE, stderr = TRUE)
        system2("docker", c("inspect --format '{{.Id}}'", container.name), stdout = TRUE, stderr = TRUE)
    } else {
        docker_run_args <- paste("run --privileged --name ", container.name, " -v /var/run/docker.sock:/var/run/docker.sock -v ", data_dir, ":/bunny_data -dit ", docker_image, sep="")
        system2("docker", c(docker_run_args), stdout = TRUE, stderr = TRUE)
    }
}

.set_container_name = function() {
    paste("bunny", Sys.getpid(), sep = "-")
}

.test_docker_setup = function() {
    docker.info <- system2("docker", "info", stdout = FALSE, stderr = FALSE)
    if (docker.info == 1) {
        stop("[ERROR]: Native docker or Docker-machine is not running.")
    } else {
        message("[INFO]: Docker set.")
    }
}

.docker_env_vars = function() {
    # TODO:
    # This flow should really be handled by separate class or
    # R Docker Remote API client package
    docker.info <- system2("docker", "info", stdout = FALSE, stderr = FALSE)
    if (docker.info == 0) {
        message("[INFO]: Docker set.")
        message("[INFO]: Pulling and running sevenbridges testenv docker image...")
    } else {
        docker_machine_args <- "ls --filter state=Running --format '{{ .Name }}'"
        docker.vm <- system2("docker-machine", c(docker_machine_args), stdout = TRUE, stderr = TRUE)
        if (identical(docker.vm, character(0))) {
            stop("[ERROR]: Native docker or Docker-machine is not running.")
        }
        envs <- substring(system2("docker-machine", c("env", docker.vm), stdout = TRUE, stderr = TRUE)[1:4], 8)
        envs <- gsub("\"", "", unlist(strsplit(envs, "="))[c(FALSE, TRUE)])
        Sys.setenv(DOCKER_TLS_VERIFY = envs[1], DOCKER_HOST = envs[2], DOCKER_CERT_PATH = envs[3], DOCKER_MACHINE_NAME = envs[4])

        .test_docker_setup()
    }
}

#' Test tools in rabix/bunny
#'
#' Test tools locally in rabix/bunny inside docker container
#'
#' @param rabix_tool rabix tool from Tool class
#' @param inputs input parameters declared as json (or yaml) string
#' @export test_tool_bunny
#' @return bunny stdout
#' @examples
#' \dontrun{
#' inputs <- '{"counts_file": {"class": "File", "path": "./FPKM.txt"}, "gene_names": "BRCA1"}'
#' rbx <- <define rabix tool>
#' set_test_env("tengfei/testenv", "<mount_dir>")
#' test_tool_bunny(rbx, inputs)}

test_tool_bunny = function(rabix_tool, inputs) {

    container.name <- .set_container_name()
    check_cmd <- paste0("ps --filter status=running --filter name=", container.name, " --format '{{.Names}}: running for {{.RunningFor}}'")
    container <- system2("docker", c(check_cmd), stdout = TRUE, stderr = TRUE)
    if (identical(container, character(0))) {
        message("Test container not running. Try setting testing env first (set_test_env())")
    } else {
        message("Trying the execution...")
        check_cmd <- paste0("inspect --format '{{ range .Mounts }}{{ if eq .Destination \"/bunny_data\" }}{{ .Source }}{{ end }}{{ end }}' ", container.name)
        mount_point <- system2("docker", c(check_cmd), stderr = TRUE, stdout = TRUE)

        tool_path <- paste0(mount_point, "/tool.json")
        inputs_path <- paste0(mount_point, "/inputs.json")
        stdout_path <- paste0(mount_point, "/stdout.log")
        stderr_path <- paste0(mount_point, "/stderr.log")

        # cleanup
        if (file.exists(tool_path)) {
            system2("docker", paste0("exec ", container.name, " bash -c 'rm /bunny_data/tool.json'"))
        }
        if (file.exists(inputs_path)) {
            system2("docker", paste0("exec ", container.name, " bash -c 'rm /bunny_data/inputs.json'"))
        }

        write(rabix_tool$toJSON(pretty = TRUE), file = tool_path)
        write(toJSON(inputs, pretty = TRUE, auto_unbox = TRUE), file = inputs_path)

        run_cmd <- paste0("exec ", container.name, " bash -c 'cd /opt/bunny && ./rabix -b /bunny_data /bunny_data/tool.json /bunny_data/inputs.json'")
        system2("docker", run_cmd, stdout = stdout_path, stderr = stderr_path)
        cat( readLines( stdout_path ) , sep = "\n" )
    }

}

#' Test tools in rabix/rabix-devel (DEPRECATED)
#'
#' Test tools locally in rabix/rabix-devel python executor (DEPRECATED)
#'
#' @param rabix_tool rabix tool from Tool class
#' @param inputs input parameters declared as json (or yaml) string
#' @export test_tool_rabix
#' @return rabix stdout
#' @examples
#' \dontrun{
#' inputs <- '{"counts_file": {"class": "File", "path": "./FPKM.txt"}, "gene_names": "BRCA1"}'
#' rbx <- <define rabix tool>
#' set_test_env("tengfei/testenv", "<mount_dir>")
#' test_tool_rabix(rbx, inputs)}
test_tool_rabix = function(rabix_tool, inputs=list()) {

    container.name <- .set_container_name()
    check_cmd <- paste0("ps --filter status=running --filter name=", container.name, " --format '{{.Names}}: running for {{.RunningFor}}'")
    container <- system2("docker", c(check_cmd), stdout = TRUE, stderr = TRUE)
    if (identical(container, character(0))) {
        message("Test container not running. Try setting testing env first (set_test_env())")
    } else {
        message("Trying the execution...")
        check_cmd <- paste0("inspect --format '{{ range .Mounts }}{{ if eq .Destination \"/bunny_data\" }}{{ .Source }}{{ end }}{{ end }}' ", container.name)
        mount_point <- system2("docker", c(check_cmd), stderr = TRUE, stdout = TRUE)

        tool_path <- paste0(mount_point, "/tool.json")
        inputs_path <- paste0(mount_point, "/inputs.json")
        stdout_path <- paste0(mount_point, "/stdout.log")
        stderr_path <- paste0(mount_point, "/stderr.log")

        # cleanup
        if (file.exists(tool_path)) {
            system2("docker", paste0("exec ", container.name, " bash -c 'rm /bunny_data/tool.json'"))
        }
        if (file.exists(inputs_path)) {
            system2("docker", paste0("exec ", container.name, " bash -c 'rm /bunny_data/inputs.json'"))
        }

        write(rabix_tool$toJSON(pretty = TRUE), file = tool_path)
        write(toJSON(inputs, pretty = TRUE, auto_unbox=TRUE), file = inputs_path)
        out_dir <- paste0(format(Sys.time(), "%H%M%s-%d%m%Y-"), "rabix")
        out_dir_abs <- paste("/bunny_data", out_dir, sep = "/")
        run_cmd <- paste0("exec ", container.name, " bash -c 'cd /bunny_data && rabix -v -v -v /bunny_data/tool.json -i /bunny_data/inputs.json'")
        system2("docker", run_cmd, stdout = stdout_path, stderr = stderr_path)
        cat( readLines( stdout_path ) , sep = "\n" )
    }

}

#' add \code{#} prefix to id
#'
#' add \code{#} prefix to id
#'
#' @param x (character) with \code{#} or not.
#'
#' @return a character with \code{#} prefix.
#'
#' @export addIdNum
#' @examples
#' addIdNum(c("bam", "#fastq"))
addIdNum <- function(x) {
    if (!is.null(x)) {
        sapply(x, .addIdNum)
    } else {
        NULL
    }
}

.addIdNum <- function(x) {
    if (!is.null(x)) {
        x <- parseLabel(x)
        .first <- substr(x, 1, 1)
        if (.first != "#") {
            return(paste0("#", x))
        } else {
            return(x)
        }
    } else {
        return(NULL)
    }
}

parseLabel = function(x) gsub("[[:space:]]+", "_", x)

getId = function(x) addIdNum(x$label)

getInputId <- function(x) {
    .id <- addIdNum(x$label)
    ins <- x$inputs
    if (length(ins)) {
        lapply(ins, function(i){
            .in.id <- gsub("^#", "", i$id)
            paste(.id, .in.id, sep = ".")
        })
    } else {
        return(NULL)
    }
}

getOutputId <- function(x) {
    .id <- addIdNum(x$label)
    os <- x$outputs
    if (length(os)) {
        lapply(os, function(i){
            .out.id <- gsub("^#", "", i$id)
            paste(.id, .out.id, sep = ".")
        })
    } else {
        return(NULL)
    }
}

make_type = function(.t) {
    .t = sapply(.t, function(s) {
        # file array problem
        if (!is.null(names(s)) && "type" %in% names(s)) {
            if(s$type == "array") {
                return(paste0(s$items, "..."))
            } else if (s$type == "enum") {
                return("enum")
            } else {
                return("null")
            }
        } else {
            if (is.list(s)) {
                return(s[[1]])
            } else {
                if (length(s) > 1) {
                    return(s[s != "null"])
                } else {
                    return(s)
                }
            }
        }
    })
    .t[.t != "null"]
}

getInputType <- function(x) {
    ins <- x$inputs
    if (length(ins)) {
        sapply(ins, function(i) {
            .t <- i$type
            .id <- gsub("^#", "", i$id)
            .t <- make_type(.t)
            res <- .t
            names(res) <- .id
            res
        })
    } else {
        NULL
    }
}

getOutputType <- function(x) {
    os <- x$outputs
    if (length(os)) {
        sapply(os, function(i) {
            .t <- i$type
            .id <- gsub("^#", "", i$id)
            .t <- make_type(.t)

            res <- .t
            names(res) <- .id
            res
        })} else {
            NULL
        }
}

#' Test tools with cwl-runner
#'
#' Test tools locally cwl-runner
#' (https://github.com/common-workflow-language/cwltool)
#'
#' @param rabix_tool rabix tool from Tool class
#' @param inputs input parameters declared as json (or yaml) string
#' @export test_tool_cwlrun
#' @return cwl-runner stdout
#' @examples
#' \dontrun{
#' inputs <- '{"counts_file": {"class": "File", "path": "./FPKM.txt"}, "gene_names": "BRCA1"}'
#' rbx <- <define rabix tool>
#' set_test_env("tengfei/testenv", "<mount_dir>")
#' test_tool_cwlrun(rbx, inputs)
#' }
test_tool_cwlrun = function(rabix_tool, inputs=list()) {

    container.name <- .set_container_name()
    check_cmd <- paste0("ps --filter status=running --filter name=", container.name, " --format '{{.Names}}: running for {{.RunningFor}}'")
    container <- system2("docker", c(check_cmd), stdout = TRUE, stderr = TRUE)
    if (identical(container, character(0))) {
        message("Test container not running. Try setting testing env first (set_test_env())")
    } else {
        message("Trying the execution...")
        check_cmd <- paste0("inspect --format '{{ range .Mounts }}{{ if eq .Destination \"/bunny_data\" }}{{ .Source }}{{ end }}{{ end }}' ", container.name)
        mount_point <- system2("docker", c(check_cmd), stderr = TRUE, stdout = TRUE)

        tool_path <- paste0(mount_point, "/tool.json")
        inputs_path <- paste0(mount_point, "/inputs.json")
        stdout_path <- paste0(mount_point, "/stdout.log")
        stderr_path <- paste0(mount_point, "/stderr.log")

        # cleanup
        if (file.exists(tool_path)) {
            system2("docker", paste0("exec ", container.name, " bash -c 'rm /bunny_data/tool.json'"))
        }
        if (file.exists(inputs_path)) {
            system2("docker", paste0("exec ", container.name, " bash -c 'rm /bunny_data/inputs.json'"))
        }

        write(rabix_tool$toJSON(pretty = TRUE), file = tool_path)
        write(toJSON(inputs, pretty = TRUE, auto_unbox = TRUE), file = inputs_path)
        out_dir <- paste0(format(Sys.time(), "%H%M%s-%d%m%Y-"), "cwlrunner")
        out_dir_abs <- paste("/bunny_data", out_dir, sep = "/")
        run_cmd <- c("exec", container.name, "bash -c 'mkdir ", out_dir_abs,
                     " && cd ", out_dir_abs,
                     " && cwl-runner --non-strict --tmpdir-prefix . --tmp-outdir-prefix . /bunny_data/tool.json /bunny_data/inputs.json'")
        system2("docker", run_cmd, stdout = stdout_path, stderr = stderr_path)
        cat( readLines( stdout_path ) , sep = "\n" )
    }

}

set_box <- function(x) {
    .c <- class(x)
    class(x) <- c(.c, "box")
    x
}

# CWL Utilities

#' Get class from CWL JSON file
#'
#' Get class from CWL JSON file
#'
#' @param input cwl json file path
#'
#' @return character for cwl class "Workflow" or "CommandLineTool"
#' @export get_cwl_class is_commandlinetool is_workflow
#' @rdname cwl-utils
#' @aliases is_commandlinetool is_workflow
#' @examples
#' tool.in = system.file("extdata/app", "tool_unpack_fastq.json", package = "sevenbridges")
#' flow.in = system.file("extdata/app", "flow_star.json", package = "sevenbridges")
#' get_cwl_class(tool.in)
#' is_commandlinetool(tool.in)
#' is_workflow(tool.in)
#' get_cwl_class(flow.in)
#' is_commandlinetool(flow.in)
#' is_workflow(flow.in)
get_cwl_class = function(input) fromJSON(input)$class

is_commandlinetool = function(input) get_cwl_class(input) == "CommandLineTool"

is_workflow = function(input) get_cwl_class(input) == "Workflow"

# format type list into a DSCList
format_type = function(x) {
    lst = lapply(x, .format_type)
    if (all(sapply(lst, is.character))) return(unlist(lst))
    do.call(DSCList, lst)
}

.format_type = function(x) {
    if ("type" %in% names(x)) {
        switch(x$type,
               array = {
                   do.call(ItemArray, x)
               },
               enum = {
                   do.call(enum, x)
               })
    } else {
        as.character(x)
    }
}

get_id_from_label = function(x, suffix = "#") {
    paste0(suffix, gsub("[[:space:]+]", "_", x))
}

de_sharp = function(x) gsub("[#+]", "", x)

add_sharp = addIdNum

is_full_name = function(x) grepl("[.]", x)

get_tool_id_from_full = function(x) {

    sapply(x, function(i) {
        if (is_full_name(i)) {
            add_sharp(strsplit(i, "\\.")[[1]][1])
        } else {
            add_sharp(i)
        }
    })

}

get_input_id_from_full = function(x) {

    sapply(x, function(i) {
        if (is_full_name(i)) {
            add_sharp(strsplit(i, "\\.")[[1]][2])
        } else {
            add_sharp(i)
        }
    })

}

deType <- function(x) {

    # string
    str_type <- c('STRING', 'STR', '<string>', '<str>', 'str', "character",
                  "string", "String")
    # int
    int_type <- c('INTEGER', 'INT', '<integer>', '<int>', 'int',
                  "integer", "Integer")
    # float
    float_type <- c('FLOAT', '<float>', 'float', 'Float')

    # File
    file_type <- c('FILE', '<file>', 'File', 'file')

    # enum
    enum_type <- c('ENUM', '<enum>', 'enum', "Enum")

    .array <- FALSE
    if (is.character(x)) {
        res <- ""
        if (grepl("\\.\\.\\.", x)) {
            .array <- TRUE
            x <- gsub("[^[:alnum:]]", "", x)
        }

        if (x %in% str_type) {
            res <- "string"
        } else if (x %in% int_type) {
            res <- "int"
        } else if (x %in% float_type) {
            res <- "float"
        } else if (x %in% file_type) {
            res <- "File"
        } else if (x %in% enum_type) {
            res <- "enum"
        } else {
            res <- x
        }
        if (.array) {
            res <- ItemArray(res)
        }
    } else {
        res <- x
    }
    res
}

#' Get input/output matrix out of JSON CWL file directly
#'
#' An efficient way to access JSON file, no need to convert a JSON into a
#' \code{Tool} or \code{Flow} object before access, directly operate on a
#' list parsed from JSON file. Compare to \code{convert_app}, it is much faster.
#'
#' @param from JSON file path
#' @param new.order a vector of column orders by default for input it's
#' \code{"id"}, \code{"label"}, \code{"type"}, \code{"required"},
#' \code{"prefix"}, \code{"fileTypes"}; For output it's
#' \code{"id"}, \code{"label"}, \code{"type"}, \code{"fileTypes"}
#' @param required logical value, show requried input node only or not.
#' @export input_matrix
#' @rdname input_output_matrix
#' @examples
#' tool.in = system.file("extdata/app", "tool_unpack_fastq.json", package = "sevenbridges")
#' flow.in = system.file("extdata/app", "flow_star.json", package = "sevenbridges")
#' input_matrix(tool.in)
#' input_matrix(tool.in, required = TRUE)
#' input_matrix(flow.in)
#' input_matrix(flow.in, c("id", "type"))
#' input_matrix(flow.in, required = TRUE)
input_matrix = function(from,
                        new.order = c("id", "label", "type", "required", "prefix", "fileTypes"),
                        required = NULL) {

    if (is.character(from) && file.exists(from)) {
        ## json cwl file
        obj <- fromJSON(from, FALSE)
    } else {
        ## parsed list
        obj <- from
    }

    in.lst = obj$inputs
    lst = lapply(in.lst, function(x) {
        ib = x$inputBinding
        res =  c(x[!names(x) %in% c("inputBinding",
                                    "sbg:category",
                                    "required",
                                    "sbg:fileTypes",
                                    "type", "fileTypes",
                                    "sbg:stageInput")],
                 list(required   = sevenbridges:::is_required(x),
                      type       = sevenbridges:::make_type(x$type),
                      category   = x[["sbg:category"]],
                      fileTypes  = x[["sbg:fileTypes"]],
                      stageInput = x[["sbg:stageInput"]]),
                 ib)

        res[sapply(res, is.null)] <- "null"
        res = do.call(data.frame, res)
        .fullnames = names(res)

        .names.sbg      = sort(.fullnames[grep("^sbg", .fullnames)])
        .names.other    = sort(setdiff(.fullnames, .names.sbg))
        .names.priority = c("id", "type", "required", "fileTypes", "label")
        .names.p2       = sort(setdiff(.names.other, .names.priority))
        new.order       = c(.names.priority, .names.p2, .names.sbg)

        res[, new.order]

    })

    res = suppressWarnings(do.call("bind_rows", lst))

    # reorder for File File...
    idx  = res$type %in% c("File", "File...")
    res1 = res[idx, ]
    res2 = res[!idx, ]
    res  = rbind(res1, res2)

    # required or not
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

}

#' @rdname input_output_matrix
#' @export output_matrix
#' @examples
#' tool.in = system.file("extdata/app", "tool_unpack_fastq.json", package = "sevenbridges")
#' flow.in = system.file("extdata/app", "flow_star.json", package = "sevenbridges")
#' output_matrix(tool.in)
#' output_matrix(flow.in)
output_matrix = function(from,
                         new.order = c("id", "label", "type", "fileTypes")) {

    if (is.character(from) && file.exists(from)) {
        ## json cwl file
        obj <- fromJSON(from, FALSE)
    } else {
        ## parsed list
        obj <- from
    }

    .c = obj$class
    out.lst = obj$outputs
    switch(.c,

           "CommandLineTool" = {

               lst = lapply(out.lst, function(x) {

                   o.b <- x$outputBinding
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
                              inheritMetadataFrom = x$`sbg:inheritMetadataFrom`,
                              metadata = x$`sbg:metadata`,
                              secondaryFiles = x$seconaryFiles)

                   res =  c(x[!names(x) %in% c("sbg:fileTypes",
                                               "outputBinding",
                                               "type",
                                               "fileTypes",
                                               "sbg:inheritMetadataFrom",
                                               "sbg:metadata")],
                            list(type = sevenbridges:::make_type(x$type),
                                 fileTypes = x[["sbg:fileTypes"]]),
                            ob)

                   res[sapply(res, is.null)] <- "null"

                   res = do.call(data.frame, res)
                   .fullnames = names(res)

                   .names.sbg      = sort(.fullnames[grep("^sbg", .fullnames)])
                   .names.other    = sort(setdiff(.fullnames, .names.sbg))
                   .names.priority = c("id", "label", "type")
                   .names.p2       = sort(setdiff(.names.other, .names.priority))
                   new.order       = c(.names.priority, .names.p2, .names.sbg)

                   res[, new.order]

               })

               res  = suppressWarnings(do.call("bind_rows", lst))
               # reorder for File File...
               idx  = res$type %in% c("File", "File...")
               res1 = res[idx, ]
               res2 = res[!idx, ]
               res = rbind(res1, res2)

               # new order
               if (!is.null(new.order)) {
                   new.order = intersect(new.order, names(res))
                   res[, new.order]
               } else {
                   res
               }
           },

           "Workflow" = {

               lst = lapply(out.lst, function(x) {

                   res =  c(x[!names(x) %in% c("sbg:fileTypes",
                                               "type",
                                               "fileTypes",
                                               "sbg:inheritMetadataFrom",
                                               "sbg:metadata")],
                            list(type = sevenbridges:::make_type(x$type),
                                 fileTypes = x[["sbg:fileTypes"]]))

                   res[sapply(res, is.null)] <- "null"
                   res = do.call(data.frame, res)
                   .fullnames = names(res)

                   .names.sbg      = sort(.fullnames[grep("^sbg", .fullnames)])
                   .names.other    = sort(setdiff(.fullnames, .names.sbg))
                   .names.priority = c("id", "label", "type")
                   .names.p2       = sort(setdiff(.names.other, .names.priority))
                   new.order       = c(.names.priority, .names.p2, .names.sbg)

                   res[, new.order]
               })

               res  = suppressWarnings(do.call("bind_rows", lst))

               # reorder for File File...
               idx  = res$type %in% c("File", "File...")
               res1 = res[idx, ]
               res2 = res[!idx, ]
               res  = rbind(res1, res2)
               # new order
               if ("link_to" %in% new.order || is.null(new.order)) {
                   lm = link_map()
                   res$link_to = sapply(res$id, function(i) {
                       paste0(as.character(lm[which(lm$id == i), "source"]),
                              collapse = " | ")
                   })
               }

               if (!is.null(new.order)) {
                   new.order = intersect(new.order, names(res))
                   res[, new.order]
               } else {
                   res
               }

           })

}
