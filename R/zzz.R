.onLoad <- function(libname, pkgname) {
    lst <- list(offset = NULL,
                limit = NULL,
                taskhook = TaskHook())
    options(sevenbridges = lst)
}
