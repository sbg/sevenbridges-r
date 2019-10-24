Permission <- setRefClass(
  "Permission",
  contains = "Item",

  fields = list(
    write = "logicalORNULL",
    copy_permission = "logicalORNULL", # cannot use copy
    execute = "logicalORNULL",
    admin = "logicalORNULL",
    read = "logicalORNULL"
  ),

  methods = list(
    initialize = function(write = NULL, copy_permission = NULL, execute = NULL, admin = NULL, read = NULL, ...) {
      write <<- write
      copy_permission <<- copy_permission
      execute <<- execute
      admin <<- admin
      read <<- read

      callSuper(...)
    },

    show = function() {
      .showFields(
        .self, "-- Permission --",
        c("read", "write", "copy_permission", "execute", "admin")
      )
    }
  )
)

Member <- setRefClass(
  "Member",
  contains = "Item",

  fields = list(
    pid = "characterORNULL",
    id = "characterORNULL",
    username = "characterORNULL",
    invitation_pending = "logicalORNULL",
    permissions = "Permission"
  ),

  methods = list(
    update = function(write = NULL, copy = NULL, execute = NULL, admin = NULL, read = NULL, ...) {
      if (is.null(pid)) stop("cannot find project id")

      body <- list("write" = write, "copy" = copy, "execute" = execute, "read" = read, "admin" = admin)
      body <- body[!sapply(body, is.null)]

      if (length(body) == 0) stop("please provide updated information")

      req <- api(
        token = auth$token,
        base_url = auth$url,
        path = paste0(
          "projects/", pid,
          "/members/", username,
          "/permissions"
        ),
        body = body, method = "PATCH",
        authorization = auth$authorization, ...
      )

      res <- status_check(req)

      # check new updated info

      # update self
      lst <- res
      names(lst)[names(lst) == "copy"] <- "copy_permission"
      nms <- names(lst)

      # update object
      for (nm in nms) {
        .self$permissions$field(nm, lst[[nm]])
      }

      .self
    },

    delete = function(...) {
      stopifnot(!is.null(auth$version))

      req <- api(
        token = auth$token,
        base_url = auth$url,
        path = paste0(
          "projects/", pid,
          "/members/", username
        ),
        method = "DELETE",
        authorization = auth$authorization, ...
      )
      res <- status_check(req)
    },

    show = function() {
      .showFields(.self, "== Member ==",
        values = c(
          "id", "username",
          "invitation_pending"
        )
      )
      .self$permissions$show()
    }
  )
)

.asMember <- function(x, pid = NULL) {
  Member(
    id = x$id,
    pid = pid,
    username = x$username,
    invitation_pending = x$invitation_pending,
    permissions = do.call(Permission, x$permissions),
    response = response(x)
  )
}

MemberList <- setListClass("Member", contains = "Item0")

.asMemberList <- function(x, pid = NULL) {
  obj <- MemberList(lapply(x$items, .asMember, pid = pid))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}
