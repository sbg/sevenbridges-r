.onLoad <- function(libname, pkgname) {
    lst <- list(offset = 0,
                limit = 100,
                input_check = TRUE, 
                auth = suppressMessages(.parseToken()),
                taskhook = TaskHook())
    options(sevenbridges = lst)
}
