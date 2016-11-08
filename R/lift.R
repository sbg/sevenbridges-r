# # TODO
# #' lift a docopt string
# #'
# #' lift a docopt string used for command line
# #'
# #' parse Rmarkdown header from rabix field
# #'
# #' @param input input Rmarkdown file or a function name (character)
# #' @aliases lift_docopt
# #' @return a string used for docopt
# #' @examples
# #' \dontrun{
# #' fl = system.file("examples/runif.Rmd", package = "liftr")
# #' opts = lift_docopt(fl)
# #' require(docopt)
# #' docopt(opts)
# #' docopt(lift_docopt("mean.default"))
# #' }
# lift_docopt = function(input) {
#
#     if (file.exists(input)) {
#         res = lift_docopt_from_header(input)
#     } else {
#         message("file doesn't exist, try to try this as a function")
#         res = lift_docopt_from_function(input)
#     }
#     res
# }

# # TODO
# lift_docopt_from_header = function(input) {
#
#     opt_all_list = parse_rmd(input)
#     ol <- opt_all_list$rabix
#     .in <- ol$inputs
#     txt <- paste("usage:", ol$baseCommand, "[options]")
#     txt <- c(txt, "options:")
#
#     ol <- lapply(.in, function(x) {
#         .nm <- x$prefix
#         .t <- x$type
#         .type <- paste0('<', deType(.t), '>')
#         .o <- paste(.nm, .type, sep = "=")
#         .des <- x$description
#         .default <- x$default
#         if (!is.null(.default)) {
#             .des <- paste0(.des, " [default: ", .default, "]")
#         }
#         list(name = .o, description = .des)
#     })
#
#     for (i in 1:length(ol)) {
#         txt <- c(txt, paste(" ", ol[[i]]$name, ol[[i]]$description))
#     }
#
#     paste(txt, collapse = "\n")
#
# }

# # TODO
# lift_docopt_from_function = function(input) {
#
#     ol = opt_all_list = rdarg(input)
#
#     txt <- paste0("usage: ", input, ".R",  " [options]")
#
#     nms <- names(ol)
#     lst <- NULL
#
#     for (nm in nms) {
#         .nm = paste0("--", nm)
#         .t = guess_type(nm, input)
#         .type = paste0('<', deType(.t), '>')
#         .o = paste(.nm, .type, sep = "=")
#         .des = ol[[nm]]
#         .def  = guess_default(nm, input)
#         if (!is.null(.def)) .des <- paste0(.des, " [default: ", .def, "]")
#         lst = c(lst, list(list(name = .o, description = .des)))
#     }
#
#     for (i in 1:length(lst)) {
#         txt <- c(txt, paste(" ", lst[[i]]$name, lst[[i]]$description))
#     }
#
#     # FIXME:
#     paste(txt, collapse = "\n")
#
# }

# # TODO
# lift_cmd = function(input, output_dir = NULL,
#                     shebang = "#!/usr/local/bin/Rscript",
#                     docker_root = "/") {
#
#     if (file.exists(input)) {
#         opt_all_list = parse_rmd(input)
#         if (is.null(output_dir))
#             output_dir = dirname(normalizePath(input))
#
#         tmp = file.path(output_dir, opt_all_list$rabix$baseCommand)
#         message("command line file: ", tmp)
#         con = file(tmp)
#         txt = lift_docopt(input)
#         txt = c(shebang, "'", paste0(txt, " ' -> doc"))
#         paste("library(docopt)\n opts <- docopt(doc) \n
#               rmarkdown::render('",
#               docker_root, basename(input),
#               "', rmarkdown::html_document(toc = TRUE), output_dir = '.', params = lst)" ) -> .final
#         txt <- c(txt, .final)
#         writeLines(txt, con = con)
#         close(con)
#     } else {
#         message("consider you passed a function name (character)")
#         if (is.null(output_dir))
#             output_dir = getwd()
#         .baseCommand <- paste0(input, ".R")
#         tmp = file.path(output_dir, .baseCommand)
#         message("command line file: ", tmp)
#         con = file(tmp)
#         txt = lift_docopt(input)
#         txt = c(shebang, "'", paste0(txt, " ' -> doc"))
#         txt = c(txt, "library(docopt)\n opts <- docopt(doc)")
#         .final = gen_list(input)
#         txt <- c(txt, .final)
#         writeLines(txt, con = con)
#         close(con)
#     }
#     Sys.chmod(tmp)
#     tmp
# }

con_fun = function(type) {
    res = switch(deType(type),
                 int     = "as.integer",
                 float   = "as.numeric",
                 boolean = "as.logical",
                 NULL)
    res
}

# # TODO
# gen_list = function(fun) {
#     lst = rdarg(fun)
#     lst = lst[names(lst) != "..."]
#     nms = names(lst)
#     txt = NULL
#     for (nm in nms) {
#         .t = con_fun(guess_type(nm, fun))
#         if (!is.null(.t)) {
#             txt = c(txt, paste0(nm, " = ", .t, "(", "opts$", nm, ")"))
#         } else {
#             txt = c(txt, paste0(nm, " = ", "opts$", nm))
#         }
#     }
#     txt = paste("list(", paste(txt, collapse = ","), ")")
#     paste("do.call(", fun, ",", txt, ")")
#
# }

guess_type = function(nm, fun) {
    dl = formals(fun)
    if (!is.null(dl[[nm]])) {
        .c <- class(dl[[nm]])
        if (.c == "name") {
            return("string")
        } else {
            return(deType(.c))
        }
    } else {
        return("string")
    }
}

guess_default = function(nm, fun) {
    dl = formals(fun)
    if (!is.null(dl[[nm]])) {
        .c <- class(dl[[nm]])
        if (.c == "name") {
            return(NULL)
        } else {
            return(dl[[nm]])
        }
    } else {
        return(NULL)
    }
}

parse_rmd = function(input) {

    # locate YAML metadata block
    doc_content = readLines(normalizePath(input))
    header_pos = which(doc_content == '---')

    # handling YAML blocks ending with three dots
    if (length(header_pos) == 1L) {
        header_dot_pos = which(doc_content == '...')
        if (length(header_dot_pos) == 0L) {
            stop('Cannot correctly locate YAML metadata block.
                 Please use three hyphens (---) as start line & end line,
                 or three hyphens (---) as start line with three dots (...)
                 as end line.')
        } else {
            header_pos[2L] = header_dot_pos[1L]
        }
    }

    doc_yaml = paste(doc_content[(header_pos[1L] + 1L):
                                     (header_pos[2L] - 1L)],
                     collapse = '\n')

    yaml.load(doc_yaml)

}
