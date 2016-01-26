
.response_app <- c("href", "id", "name", "project", "revision", "raw")
App <- setRefClass("App", contains = "Item",
                   fields = list(id = "characterORNULL",
                       project = "characterORNULL",
                       name = "characterORNULL",
                       revision = "characterORNULL",
                       raw = "ANY"),
                   methods = list(
                       copyTo = function(project = NULL, name = NULL){
                           auth$copyApp(id, project = project, name = name)
                       },
                       ## getLatestRevision = function(){
                       ##     app(id = .id, detail = TRUE)$revision
                       ## },
                       cwl = function(revision = NULL, ...){
                           if(!is.null(revision)){
                               .id <- .update_revision(id, revision)
                           }else{
                               .id <- id
                           }
                           auth$api(path = paste0("apps/", .id, "/raw"),
                                    methods = "GET", ...)
                       },
                       show = function(){
                           .showFields(.self, "== App ==", .response_app)
                       }
                   ))

.asApp <- function(x){
    if(!is.null(x$revision)){
        r <- as.character(x$revision)
    }else{
        r <- x$revision
    }
    App(id = x$id,
        name = x$name,
        project = x$project,
        revision = r,
        raw = x$raw,
        response = response(x))
}

AppList <- setListClass("App", contains = "Item0")

.asAppList <- function(x){
    obj <- AppList(lapply(x$items, .asApp))
    obj@href <- x$href
    obj@response <- response(x)
    obj
}






