.onLoad <- function(libname, pkgname) {
    lst <- list(offset = NULL,
                limit = NULL,
                auth = suppressMessages(.parseToken()),
                taskhook = TaskHook())
    options(sevenbridges = lst)
}
