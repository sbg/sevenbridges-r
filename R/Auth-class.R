##' Class Auth
##'
##' Auth token object
##'
##' Every object could be requested from this Auth object and any action
##' could start from this object using cascading style. Please check vignette
##' 'easy-api' for more information.
##'
##' @field auth_token [character] your auth token.
##' @field url [character] basic url used for API, by default
##' it's \url{https://api.sbgenomics.com/1.1/}
##'
##' @param auth_token [character] your auth token.
##' @param platform [character %in% 'CGC', 'US'] which platform you are
##'  using, by default it is sbg us platform.
##' @param url [chracter] a URL for the API, default is \code{NULL},
##'  will use \code{api} parameter to switch to the right one.
##' @param version [character] default: 1.1 version used for api.
##'
##' @importFrom stringr str_match 
##'
##' @export Auth
##' @exportClass Auth
##' @examples
##' ## replace it with real token
##' token <- "aef7e9e3f6c54fb1b338ac4ecddf1a56"
##' a <- Auth(token)
Auth <- setRefClass("Auth", fields = list(auth_token = "character",
                                url = "character",
                                version = "character"),
                    methods = list(
                        initialize = function(
                            auth_token = NULL,
                            platform = c("CGC", "US"),
                            url = NULL,
                            version = c("1.1", "v2"), ...){

                            platform <- match.arg(platform)
                            .v <- match.arg(version)
                        
                            if(is.null(auth_token)){
                                stop("missing token")
                            }
                            
                            auth_token <<- auth_token

                            stopifnot(is.null(url) | is.character(url))

                            if(is.null(url)){
                                url <<- switch(platform,
                                               'US' =
                                                   paste0('https://api.sbgenomics.com/', .v, '/'),
                                               'CGC' =
                                                   paste0('https://cgc-api.sbgenomics.com/', "v2", '/')
                                               )
                                ## if(platform %in% c("cgc")){
                                ##     ## V2 only platform?
                                ##     version <<- "v2"
                                ## }else{
                                ##     version <<- .v
                                ## }                                
                            }else{
                                url <<- url
                                ## platform 
                            }
                            version <<- .ver(.self$url)

                        },
                        project_owner = function(owner = NULL, ...){
                            'List the projects owned by and accessible to a particular user.
                             Each project\'s ID and URL will be returned.'
                            
                            if(is.null(owner)){
                                stop("owner must be provided. For example, Nate. ")
                            }
                            v2Check(version)
                            req <- api(auth_token = auth_token,
                                          base_url = url, 
                                          path = paste0('projects/', owner),
                                          method = 'GET', ...)
                            res <- status_check(req)
                            if(hasItems(res)){
                                rp <- parseItem(res)
                                obj <- .asProjectList(rp)
                            }else{
                                message("not found")
                                obj <- res
                            }
                             obj <- setAuth(obj, .self, "Project")
                            
                        }, 
                        project_new = function(name = NULL,
                            billing_group_id = NULL,                            
                            description = name,
                            tags = list(), type = "v2", ...){
                            
                            'Create new projects, required parameters: name, billing_group_id,
optional parameteres: tags and description, type. '

                            if(version == "1.1"){
                                res <- sevenbridges::project_new(auth_token, name = name,
                                                         base_url = url,
                                                         description = description,
                                                         billing_group_id = billing_group_id)
                            }
                            if(version == "v2"){
                                if (is.null(name) || is.null(billing_group_id))
                                    stop('name, description, and billing_group_id must be provided')

                                body = list('name' = name,
                                    'type' = type, 
                                    'description' = description,
                                    'tags' = tags,
                                    'billing_group' = billing_group_id)


                                res <- api(path = 'projects', body = body,
                                    method = 'POST', ...)
                            }

                            res <- .asProject(res)
                            res <- setAuth(res, .self, "Project")
                        },
                        ## Project call                        
                        project = function(name = NULL, id = NULL,
                            index = NULL, ignore.case = TRUE,
                            exact = FALSE, owner = NULL, detail = FALSE, ...){
                            
                            'If no id or name provided, this call returns a list of all projects you are a member of. Each project\'s project_id and URL on the CGC will be returned. If name or id provided, we did a match search the list'

                            if(version == "1.1"){
                                req <- api(path = 'project', method = 'GET', ...) 
                                pl <- .asProjectList(req[[1]])
                                res <- m.match(pl, id = id, name = name, exact = exact,
                                               ignore.case = ignore.case)
                            }
                            if(version == "v2"){
                                ## check owner
                                if(is.null(owner)){
                                    ## show all projects
                                    req <- api(path = "projects", method = "GET", ...)
                                    res <- .asProjectList(req)
                                }else{
                                    message("Owner: ", owner)
                                    req <- api(path = paste0("projects/", owner),
                                               method = "GET", ...)
                                    res <- .asProjectList(req)
                                }

                                res <- m.match(res, id = id, name = name, exact = exact,
                                               ignore.case = ignore.case)

                                if(length(res) == 1){
                                    .id <- res$id
                                    req <- api(path = paste0("projects/", .id), method = "GET",  ...)
                                    res <- .asProject(req)
                                    res <- setAuth(res, .self, "Project")
                                    return(res)                                
                                }
                                
                                if(detail && length(res)){
                                    if(is(res, "SimpleList")){
                                        ids <- sapply(res, function(x){ x$id })
                                    }else{
                                        ids <- res$id
                                    }

                                    lst <- lapply(ids, function(id){
                                        req <- api(path = paste0("projects/", id), method = "GET", ...)
                                        .asProject(req)
                                    })
                                    res <- ProjectList(lst)
                                }
                                

                            }
                            res <- setAuth(res, .self, "Project")
                            res
                               
                        },
                        billing = function(id = NULL, breakdown = FALSE, ...){
                            'if no id provided, This call returns a list of paths used to access billing
information via the API. else, This call lists all your billing groups, including groups that are pending or have been disabled.
if breakdown = TRUE, This call returns a breakdown of spending per-project for the billing group specified by billing_group. For each project that the billing group is associated with, information is shown on the tasks run, including their initiating user (the runner), start and end times, and cost.
'
                            if(is.null(id)){
                                if(version == "1.1"){
                                    req <- api(path = 'billing', method = 'GET', ...)
                                    res <- .asBillingList(req[[1]])
                                    lapply(res, function(x){
                                        x$auth <- .self
                                        x
                                    })
                                    return(res)
                                }
                                if(version == "v2"){
                                    ## show api
                                    req <- api(path = 'billing/groups', method = 'GET', ...)
                                    req <- .asBillingList(req)
                                    return(req)
                                    
                                }}else{
                                    
                                    v2Check(version)

                                    if(breakdown){
                                        req = api(path = paste0('billing/groups/', id, "/breakdown"),
                                            method = 'GET', ...)
                                    }else{
                                        req = api(path = paste0('billing/groups/', id), method = 'GET', ...)

                                    }
                                        req <- .asBilling(req)
                                        
                                    return(req)
                                }
                        },
                        invoice = function(id = NULL, ...){
                            'no id provided, This call returns a list of invoices, with information about each, including whether or not the invoice is pending and the billing period it covers.

The call returns information about all your available invoices, unless you use the query parameter bg_id to specify the ID of a particular billing group, in which case it will return the invoice incurred by that billing group only.

if id provided, This call retrieves information about a selected invoice, including the costs for analysis and storage, and the invoice period.
'
                            v2Check(version)

                            if(is.null(id)){
                                req = api(path = 'billing/invoices', method = 'GET', ...)    
                            }else{
                                req = api(path = paste0('billing/invoices/', id), method = 'GET', ...)    
                            }
                            req

                        },
                        api = function(...){
                            'This call returns all API paths, and pass arguments to api() function and input token and url automatically'
                            v2Check(version)
                            req <- sevenbridges::api(auth_token, base_url = url, ...)
                            status_check(req)
                        },
                        show = function(){
                            .showFields(.self, "== Auth ==",
                                        values = c("auth_token", "url"))
                        },
                        ## v2 only feature
                        rate_limit = function(...){
                            'This call returns information about your current 
                            rate limit. This is the number of API calls you can 
                            make in one hour.'
                            
                            v2Check(version)
                          
                            
                            req <- api(path = "rate_limit", method = "GET", ...)
                                                  
                            .asRate(req)
                        
                            
                        },
                        user = function(username = NULL, ...){
                            'This call returns a list of the resources, such as projects, 
                            billing groups, and organizations, that are accessible to you.
                            
                            If you are not an administrator, this call will only return a 
                            successful response if {username} is replaced with your own 
                            username. If you are an administrator, you can replace 
                            {username} with the username of any CGC user, to return 
                            information on their resources.'
                            
                            v2Check(version)
                            
                            if(is.null(username)){
                                req <- api(auth_token = auth_token, 
                                       path = "user/",
                                       method = "GET", ...)
                                message("username is not provided, show run user information instead")
                            }else{
                                
                                req <- api(auth_token = auth_token, 
                                           path = paste0("users/", username), 
                                           method = "GET", ...)
                            }

                            .asUser(req)
                        },
                        ## File API
                        file = function(name = NULL, id = NULL, project = NULL,
                            exact = FALSE, detail = FALSE, ...){
                            'This call returns a list of all files in a specified project that you can access. For each file, the call returns: 1) Its ID 2) Its filename The project is specified as a query parameter in the call.'


                            if(is.null(id) && is.null(project)){
                                stop("When file id is not provided, Porject id need to be provided.")
                            }

                            ## list all files
                            req <- api(path = 'files',  method = 'GET', query = list(project = project), ...)
                            res <- .asFilesList(req)

                            ## matching
                            res <- m.match(res, id = id, name = name, exact = exact)
                            
                            if(length(res)){
                                if(detail){
                                    if(is(res, "FilesList")){
                                        ids <- sapply(res, function(x){ x$id })
                                    }else{
                                        ids <- res$id
                                    }
                                    lst <- lapply(ids, function(id){
                                        req <- api(path = paste0("files/", id), method = "GET", ...)
                                        .asFiles(req)
                                    })
                                    res <- FilesList(lst)
                                }
                            }else{
                                return(NULL)
                            }

                            
                            res <- setAuth(res, .self, "Files")
                            res
                        },
                        copyFile = function(id, project = NULL, name = ""){
                            if(is.null(project))
                                stop("project ID need to be provided, to which the file is copied to")


                            ## iteratively
                            if(length(id) > 1){
                                ids <- as.character(id)
                                for(i in ids){
                                    message("copying: ", i)
                                    copyFile(i, project = project, name = name)
                                }
                            }else{
                                
                                body = list(project = project,
                                    name = name)
                                
                                res <- api(path = paste0("files/", id, "/actions/copy"),
                                           body = body, method = "POST")

                                res <- .asFiles(res)
                                setAuth(res, .self, "Files")
                            }
                        },
                        ## App API
                        app = function(name = NULL,
                            id = NULL,
                            exact = FALSE,
                            ignore.case = TRUE,
                            detail = FALSE,
                            project = NULL,
                            query = NULL,
                            visibility = c("project", "public"),
                            revision = NULL, ...){

                            visibility <- match.arg(visibility)

                            if(visibility == "public"){
                                message("ignore project id, showing public apps")
                                query <- c(query, list(visibility = "public"))
                            }
                            
                            if(!is.null(id)){
                                
                                req <- api(path = paste0("apps/", .update_revision(id, revision)),
                                           method = "GET", query = query, ...)
                                
                                return(.asApp(req))
                            }

                            ## list all apps first
                            if(is.null(project)){
                                req <- api(path = "apps", method = "GET", query = query, ...)                                
                            }else{
                                req <- api(path = "apps", method = "GET",
                                           query = c(list(project = project), query),
                                           ...)                                
                            }
                            
                            res <- .asAppList(req)
                            ## match
                            res <- m.match(res, id = id, name = name, exact = exact,
                                           ignore.case = ignore.case)

                            if(length(res) == 1){
                                .id <- res$id
                                req <- api(path = paste0("apps/", .update_revision(.id, revision)),
                                           method = "GET", query = query, ...)
                                res <- .asApp(req)
                                return(setAuth(res, .self, "App"))
                            }
                                                            
                            if(detail && length(res)){
                                if(is(res, "AppList")){
                                    ids <- sapply(res, function(x){ x$id })
                                }else{
                                    ids <- res$id
                                }

                                lst <- lapply(ids, function(id){
                                    if(is.null(project)){
                                        req <- api(path = paste0("apps/", id), 
                                                   query = query,
                                                   method = "GET", ...)
                                    }else{
                                        req <- api(path = paste0("apps/", id), method = "GET", 
                                                   query = c(list(project = project), query),
                                                   ...)                                
                                    }

                                    .asApp(req)
                                })
                                res <- AppList(lst)
                            }


                            if(!length(res)) return(NULL)
                            
                            setAuth(res, .self, "App")
                        },
                        copyApp = function(id, project = NULL, name = ""){
                            if(is.null(project))
                                stop("project ID need to be provided, to which the file is copied to")


                            ## iteratively
                            if(length(id) > 1){
                                ids <- as.character(id)
                                for(i in ids){
                                    message("copying: ", i)
                                    copyApp(i, project = project, name = name)
                                }
                            }else{
                                
                                body = list(project = project,
                                    name = name)
                                
                                res <- api(path = paste0("apps/", id, "/actions/copy"),
                                           body = body, method = "POST")

                                res <- .asApp(res)
                                setAuth(res, .self, "App")
                            }
                        },
                        task = function(name = NULL,
                            id = NULL, project = NULL,
                            exact = FALSE, detail = FALSE,
                            status = c("all", "queued", "draft", "running", "completed", "aborted", "failed"),...){

                            status <- match.arg(status)

                            if(is.null(project)){
                                ## list all files
                                if(status == "all"){
                                    req <- api(path = 'tasks',  method = 'GET', ...)
                                }else{
                                    req <- api(path = 'tasks',  method = 'GET', query = list(status = status), ...)
                                }
                            }else{
                                ## list all files
                                if(status == "all"){
                                    req <- api(path = paste0("projects/", project, "/tasks"),
                                               method = 'GET', , ...)
                                    ## req <- api(path = 'tasks',  method = 'GET', query = list(project = project), ...)
                                }else{
                                    req <- api(path = paste0("projects/", project, "/tasks"),
                                               method = 'GET',
                                               query = list(status = status), ...)

                                    ## req <- api(path = 'tasks',  method = 'GET',
                                    ##            query = list(status = status, project = project), ...)
                                }
                            }

                            res <- .asTaskList(req)

                            ## matching
                            res <- m.match(res, id = id, name = name, exact = exact)

                            if(length(res) == 1){
                                .id <- res$id
                                req <- api(path = paste0("tasks/", .id), method = "GET",  ...)
                                res <- .asTask(req)
                                res <- setAuth(res, .self, "Task")
                                return(res)                                
                            }

                            if(length(res)){
                                if(detail){
                                    if(is(res, "TaskList")){
                                        ids <- sapply(res, function(x){ x$id })
                                    }else{
                                        ids <- res$id
                                    }
                                    lst <- lapply(ids, function(id){
                                        req <- api(path = paste0("taskss/", id), method = "GET", ...)
                                        .asTask(req)
                                    })
                                    res <- TasksList(lst)
                                }
                            }else{
                                return(NULL)
                            }

                            
                            res <- setAuth(res, .self, "Task")
                            res
                        }
                    ))


setClassUnion("AuthORNULL", c("Auth", "NULL"))

                                
                               
