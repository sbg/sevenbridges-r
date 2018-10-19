.response_app <- c("href", "id", "name", "project", "revision")
# setClassUnion("SBGWorkflowORToolORNULL", c("SBGWorkflow", "Tool", "NULL"))
# # remove "raw" from default showing methods

#' Class App
#'
#' Class App
#'
#' @field id app id
#' @field project project id
#' @field name app name
#' @field revision app revision
#' @field raw raw cwl list, if doesn't have any, call \code{cwl()} method
#'
#' @export App
#' @return App object.
#' @aliases App
#' @examples
#' \dontrun{
#' a <- Auth(url = "https://api.sbgenomics.com/v2/", token = "your_token")
#' # get a public app
#' app <- a$public_app(id = "admin/sbg-public-data/rna-seq-alignment-star")
#' app$input_matrix()
#' app$output_matrix()
#' # get a public app
#' app <- a$public_app(id = "admin/sbg-public-data/star")
#' app$input_matrix()
#' app$output_matrix()}
App <- setRefClass(
  "App",
  contains = "Item",

  fields = list(
    id = "characterORNULL",
    project = "characterORNULL",
    name = "characterORNULL",
    revision = "characterORNULL",
    raw = "ANY",
    raw_obj = "ANY"
  ),

  methods = list(
    initialize = function(id = NULL, project = NULL, name = NULL, revision = NULL, raw = NULL, raw_obj = NULL, ...) {
      id <<- id
      project <<- project
      name <<- name
      revision <<- revision
      raw <<- raw
      raw_obj <<- raw_obj
      callSuper(...)
    },

    copyTo = function(project = NULL, name = NULL) {
      auth$copy_app(id, project = project, name = name)
    },

    copy_to = function(project = NULL, name = NULL) {
      copyTo(project = project, name = name)
    },

    cwl = function(revision = NULL, ...) {
      if (!is.null(revision)) {
        .id <- .update_revision(id, revision)
      } else {
        .id <- id
      }
      if (is.null(auth)) {
        stop("auth missing")
      }
      raw <<- auth$api(
        path = paste0("apps/", .id, "/raw"),
        methods = "GET", ...
      )
      raw
    },

    get_required = function() {
      obj <- convert_app(.self)
      obj$get_required()
    },

    input_matrix = function(...) {
      if (is.null(raw)) {
        message("get cwl raw file")
        cwl()
      }
      sevenbridges:::input_matrix(raw, ...)
    },

    output_matrix = function(...) {
      if (is.null(raw)) {
        message("get cwl raw file")
        cwl()
      }
      sevenbridges:::output_matrix(raw, ...)
    },

    input_type = function(...) {
      if (is.null(raw)) {
        message("get cwl raw file")
        cwl()
      }
      # obj = convert_app(.self)
      # obj$input_type(...)
      getInputType(raw)
    },

    output_type = function(...) {
      if (is.null(raw)) {
        message("get cwl raw file")
        cwl()
      }
      # obj = convert_app(.self)
      # obj$output_type(...)
      getOutputType(raw)
    },

    set_batch = function(input = NULL, criteria = NULL, type = c("ITEM", "CRITERIA")) {
      obj <- convert_app(.self)
      if (is(obj, "Tool")) {
        stop("Tool is not supported for batching yet, only Workflow supports batch")
      }

      obj$set_batch(input = input, criteria = criteria, type = type)
      message("updating app ...")
      p <- auth$project(id = project)

      pattern <- ".+\\/.+\\/(.+)/.+"
      shortname <- str_match(id, pattern)[1, 2]
      p$app_add(shortname, obj)
      message("done")
    },

    input_check = function(input, batch = NULL, proj = NULL) {
      message("check id match")
      in_type <- input_type()
      in_id <- names(in_type)
      cus_id <- names(input)
      idx <- cus_id %in% in_id

      if (sum(!idx)) {
        stop(
          "id not matched: ", paste(cus_id[!idx], collapse = " "),
          ".", "\n Inputs id should be \n", paste(in_id, collapse = " ")
        )
      }

      .type <- in_type[match(cus_id, in_id)]
      # conversion for single file trick
      id.fl <- which("File" == .type)
      id.fls <- which("File..." == .type)

      # convert string (id or names) first!
      # Based on: is a string a valid file id or not
      is_file_id <- function(x) {
        nchar(x) == 24 && !grepl("[^0-9a-fA-F]", x)
      }

      # convert a string to Files/FilesList
      # p: Project
      as_files <- function(p, x) {
        if (!all(sapply(x, is.character))) {
          stop("please provide file id(s)")
        }

        res <- lapply(x, function(f) {
          if (is_file_id(f)) {
            r <- p$file(id = f)
            if (is.null(r)) {
              stop(paste("id doesn't exists: ", f))
            } else {
              message("file id: ", f)
            }
            r
          } else {
            r <- p$file(name = f, exact = TRUE)
            if (is.null(r)) {
              stop(paste("name doesn't exists: ", f))
            } else {
              message("file name: ", f)
            }
            r
          }
        })

        if (length(x) == 1) {
          res[[1]]
        } else {
          do.call(FilesList, res)
        }
      }

      for (i in c(id.fl, id.fls)) {
        if (is.character(input[[i]])) {
          input[[i]] <- as_files(proj, input[[i]])
        }
      }


      # solve edge cases
      if (length(id.fl)) {
        # solve edge case
        for (i in id.fl) {
          # input should be File
          if (is(input[[i]], "Files")) {
            if (!is.null(batch) && cus_id[i] == batch$batch_input) {
              # only operate on the batch input node, otherwise will cause error
              message("Converting single Files as a list for batch mode: ", cus_id[i])
              input[[i]] <- list(input[[i]])
            }
          }
          if (is(input[[i]], "FilesList")) {
            if (length(input[[i]]) == 1) {
              if (is.null(batch)) {
                message("Converting to single Files type: ", names(input[[i]]))
                input[[i]] <- input[[i]][[1]]
              }
            } else {
              if (is.null(batch)) {
                stop(in_id[i], " only accept single File")
              }
            }
          }

          if (is.list(input[[i]])) {
            if (length(input[[i]]) == 1 && is(input[[i]][[1]], "Files")) {
              if (is.null(batch)) {
                message("Converting to single Files type: ", names(input[[i]]))
                input[[i]] <- input[[i]][[1]]
              }
            }
            if (length(input[[i]]) > 1) {
              if (is.null(batch)) {
                stop(in_id[i], " only accept single File")
              }
            }
          }
        }
      }

      if (length(id.fls)) {
        # solve edge case
        for (i in id.fls) {
          if (is(input[[i]], "Files")) {
            message("Coverting your single File to a FileList")
            input[[i]] <- list(input[[i]])
          }
        }
      }

      input
    },

    show = function() .showFields(.self, "== App ==", .response_app)
  )
)

.asApp <- function(x) {
  if (!is.null(x$revision)) {
    r <- as.character(x$revision)
  } else {
    r <- x$revision
  }

  App(
    id = x$id,
    name = x$name,
    project = x$project,
    revision = r,
    raw = x$raw,
    response = response(x)
  )
}

AppList <- setListClass("App", contains = "Item0")

.asAppList <- function(x) {
  obj <- AppList(lapply(x$items, .asApp))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}

#' Convert App or a CWL JSON file to Tool or Flow object
#'
#' Convert App or a CWL JSON file to Tool or Flow object
#'
#' This function import CWL JSON file, based on its class: CommandLineTool
#' or Worklfow to relevant object in R, Tool object or Flow object.
#'
#' @param from an App object or a CWL JSON
#' @rdname convert_app
#' @export convert_app
#' @aliases convert_app
#' @return Tool or Flow object depends on CWL type.
#' @examples
#' tool.in <- system.file("extdata/app", "tool_star.json", package = "sevenbridges")
#' flow.in <- system.file("extdata/app", "flow_star.json", package = "sevenbridges")
#' # convert to Tool object
#' convert_app(tool.in)
#' # convert to Flow object
#' convert_app(flow.in)
convert_app <- function(from) {
  if (is(from, "App")) {
    if (is.null(from$raw)) {
      message("cannot find raw file, pull raw cwl from internet")
      from$cwl()
    }
    obj <- from$raw
  } else if (is.character(from) && file.exists(from)) {
    obj <- fromJSON(from, FALSE)
  } else {
    stop("object to be converted should be either a App object or cwl json file")
  }
  .convert_app(obj)
}

.convert_app <- function(obj) {
  cls <- obj$class
  switch(cls,
    "CommandLineTool" = .asTool(obj),
    "Workflow" = .asFlow(obj)
  )
}

.asTool <- function(obj) {

  # obj should be raw list
  args.inputs <- obj$inputs
  args.outputs <- obj$outputs
  args.requirements <- obj$requirements
  args.hints <- obj$hints
  args.stdin <- obj$stdin
  args.stdout <- obj$stdout

  .diy <- c("inputs", "outputs", "requirements", "hints", "stdin", "stdout")

  # inputs
  if (length(args.inputs)) {
    res.in <- input(args.inputs)
  } else {
    res.in <- IPList()
  }

  # outputs
  if (length(args.outputs)) {
    res.out <- output(args.outputs)
  } else {
    res.out <- OPList()
  }

  # hints
  if (length(args.hints)) {
    res.hints <- requirements(args.hints)
  } else {
    res.hints <- requirements()
  }

  # requirements
  if (length(args.requirements)) {
    res.req <- requirements(args.requirements)
  } else {
    res.req <- requirements()
  }

  # stdin
  if (length(args.stdin)) {
    if (is.character(args.stdin)) {
      res.stdin <- args.stdin
    } else {
      res.stdin <- do.call(Expression, args.stdin)
    }
  } else {
    res.stdin <- NULL
  }

  # stdout
  if (length(args.stdout)) {
    if (is.character(args.stdout)) {
      res.stdout <- args.stdout
    } else {
      res.stdout <- do.call(Expression, args.stdout)
    }
  } else {
    res.stdout <- NULL
  }

  nms <- names(obj)

  # extra fields not defined, drop them with warning
  .obj.def <- names(Tool$fields())
  .obj.extra <- setdiff(nms, .obj.def)
  if (length(.obj.extra)) {
    warning(
      "Extra fields dropped before conversion: ",
      paste(.obj.extra, collapse = " ")
    )
  }

  .obj.nms <- setdiff(nms, c(.diy, .obj.extra))

  res <- do.call("Tool", obj[.obj.nms])
  res$field("inputs", res.in)
  res$field("outputs", res.out)
  res$field("hints", res.hints)
  res$field("requirements", res.req)
  res$field("stdin", res.stdin)
  res$field("stdout", res.stdout)
  # for the reason you convert from App, do not add "#"
  res$id <- gsub("^#", "", res$id)
  res
}

.asFlow <- function(obj) {
  args.inputs <- obj$inputs
  args.outputs <- obj$outputs
  args.requirements <- obj$requirements
  args.hints <- obj$hints

  .diy <- c("inputs", "outputs", "requirements", "hints", "steps")

  # inputs
  if (length(args.inputs)) {
    res.in <- input(args.inputs)
  } else {
    res.in <- IPList()
  }

  # outputs
  if (length(args.outputs)) {
    lst <- lapply(args.outputs, function(o) {
      .t <- o$type
      lst <- lapply(.t, function(x) {
        if (("type" %in% names(x)) && x$type == "array") {
          do.call(ItemArray, x)
        } else {
          x
        }
      })
      .type <- do.call(DSCList, lst)
      lst <- c(
        o[!names(o) %in% c(
          "sbg:x",
          "sbg:y",
          "sbg:includeInPorts",
          "type",
          "source"
        )],
        list(
          "x" = o$"sbg:x",
          "y" = o$"sbg:y",
          "type" = .type,
          "source" = list(as.character(o$source)),
          "includeInPorts" = o$"sbg:includeInPorts"
        )
      )
      do.call(SBGWorkflowOutputParameter, lst)
    })

    res.out <- do.call(SBGWorkflowOutputParameterList, lst)
  } else {
    res.out <- SBGWorkflowOutputParameterList()
  }

  # hints
  if (length(args.hints)) {
    res.hints <- requirements(args.hints)
  } else {
    res.hints <- requirements()
  }

  # requirements
  if (length(args.requirements)) {
    res.req <- requirements(args.requirements)
  } else {
    res.req <- requirements()
  }

  # steps
  slst <- get_steplist_item(obj)
  # if (length(steplst)) {
  #     lst <- lapply(steplst, function(x) {
  #         .convert_app(x$run)
  #     })
  #     slst <- lst[[1]]
  #     for (i in 1:(length(lst) -1)) {
  #         slst <- slst + lst[[i + 1]]
  #     }
  # } else {
  #     slst <- SBGStepList()
  # }

  nms <- names(obj)

  # extra fields not defined, drop them with warning
  .obj.def <- names(SBGWorkflow$fields())
  .obj.extra <- setdiff(nms, .obj.def)
  if (length(.obj.extra)) {
    warning(
      "Extra fields dropped before conversion: ",
      paste(.obj.extra, collapse = " ")
    )
  }

  .obj.nms <- setdiff(nms, c(.diy, .obj.extra))

  res <- do.call(
    "Flow",
    c(
      obj[.obj.nms],
      list(
        steps = slst,
        inputs = res.in,
        outputs = res.out,
        hints = res.hints,
        requirements = res.req
      )
    )
  )

  res
}

#' @rdname convert_app
#' @aliases appType
#' @export appType
#' @param x a App object
#' @section appType:
#' \describe{
#'  this function return class of a App object.}
appType <- function(x) {
  obj <- x$raw
  if (is.null(obj)) {
    x$cwl()
    obj <- x$raw
  }
  obj$class
}

get_sbg_item <- function(x) {
  lst <- fromJSON(x, FALSE)
  nms <- names(lst)
  nms[grep("sbg:", nms)]
}

get_nonsbg_item <- function(x, remove = c("inputs", "outputs", "hints", "requirements")) {
  lst <- fromJSON(x, FALSE)
  nms <- setdiff(names(lst), remove)
  nms[!grepl("sbg:", nms)]
}

get_input_item <- function(x) {
  lst <- fromJSON(x, FALSE)
  input(lst$inputs)
}

get_output_item <- function(x) {
  lst <- fromJSON(x, FALSE)
  output(lst$outputs)
}

# Step and StepList
get_stepinputlist_item <- function(x) {
  # x is a step
  lst <- lapply(x$inputs, function(i) {
    do.call(WorkflowStepInput, i)
  })
  WorkflowStepInputList(lst)
}

get_stepoutputlist_item <- function(x) {
  # x is a step
  lst <- lapply(x$outputs, function(i) {
    do.call(WorkflowStepOutput, i)
  })
  WorkflowStepOutputList(lst)
}

get_step_item <- function(x) {
  # x is a step list
  .run <- .convert_app(x$run)
  SBGStep(
    id = x$id,
    run = .run,
    outputs = get_stepoutputlist_item(x),
    inputs = get_stepinputlist_item(x)
  )
}

get_steplist_item <- function(input) {
  if (is.character(input) && file.exists(input)) {
    obj <- fromJSON(input, FALSE)
  } else if (is.list(input) && "steps" %in% names(input)) {
    obj <- input
  } else {
    stop("input has to be a json file or steplist parsed from app")
  }
  ss <- obj$steps
  do.call(SBGStepList, lapply(ss, get_step_item))
}
