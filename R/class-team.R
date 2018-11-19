# Team class -------------------------------------------------------------------
Team <- setRefClass(
  "Team",
  contains = "Item",

  fields = list(id = "characterORNULL", name = "characterORNULL", role = "characterORNULL"),

  methods = list(

    # initialize ---------------------------------------------------------------
    initialize = function(id = NULL, name = NULL, role = NULL, ...) {
      id <<- id
      name <<- name
      role <<- role

      callSuper(...)
    },

    # add a team member --------------------------------------------------------
    add_member = function(username = NULL, ...) {
      if (is.null(username)) {
        stop("Please provide the username to add to the team")
      }

      if (!grepl("/", username)) {
        stop("The username should follow the format `division_id/username`, instead of `username`")
      }

      req <- auth$api(
        path = paste0("teams/", .self$id, "/members"), method = "POST",
        body = list("id" = username), ...
      )

      res <- .asTeamMember(req)
      res$auth <- .self$auth

      res
    },

    # list team members --------------------------------------------------------
    member = function(...) {
      req <- auth$api(
        path = paste0("teams/", .self$id, "/members"),
        method = "GET", ...
      )

      # no team members
      if ((length(req$items) == 0L)) {
        return(NULL)
      }

      # one or multiple members
      if (length(req$items) != 0L) {
        res <- .asTeamMemberList(req)
        setAuth(res, .self$auth, "TeamMember")
      }

      res
    },

    # remove a team member -----------------------------------------------------
    remove_member = function(username = NULL, ...) {
      if (is.null(username)) {
        stop("Please provide the username to delete from the team")
      }

      auth$api(
        path = paste0("teams/", .self$id, "/members/", username),
        method = "DELETE", ...
      )
    },

    # rename a team ------------------------------------------------------------
    rename = function(name, ...) {
      if (is.null(name)) {
        stop("Please provide the new name for the team")
      }

      req <- auth$api(
        path = paste0("teams/", .self$id),
        body = list("name" = name),
        method = "PATCH", ...
      )

      res <- .asTeam(req)
      res$auth <- .self$auth

      res
    },

    # delete a team ------------------------------------------------------------
    delete = function(...) {
      auth$api(
        path = paste0("teams/", .self$id),
        method = "DELETE", ...
      )
    },

    # show ---------------------------------------------------------------------
    show = function() {
      .showFields(.self, "== Team ==", c("id", "name", "role"))
    }
  )
)

# .asTeam ----------------------------------------------------------------------
.asTeam <- function(x) {
  Team(id = x$id, name = x$name, role = x$role, href = x$href)
}

# TeamList class ---------------------------------------------------------------
TeamList <- setListClass("Team", contains = "Item0")

# .asTeamList ------------------------------------------------------------------
.asTeamList <- function(x) {
  obj <- TeamList(lapply(x$items, .asTeam))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}
