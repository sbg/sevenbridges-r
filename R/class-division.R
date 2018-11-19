# Division class ---------------------------------------------------------------
Division <- setRefClass(
  "Division",
  contains = "Item",

  fields = list(id = "characterORNULL", name = "characterORNULL"),

  methods = list(

    # initialize ---------------------------------------------------------------
    initialize = function(id = NULL, name = NULL, ...) {
      id <<- id
      name <<- name

      callSuper(...)
    },

    # list all teams or get details of a team ----------------------------------
    team = function(id = NULL, ...) {
      "List all teams or get details of a team."
      if (is.null(id)) {
        req <- auth$api(
          path = paste0("divisions/", .self$id, "/teams"),
          method = "GET", ...
        )
      } else {
        req <- auth$api(
          path = paste0("teams/", id),
          method = "GET", ...
        )
      }

      # no teams
      if ((length(req$items) == 0L) & is.null(req$id)) {
        return(NULL)
      }

      # one team
      if (length(req$items) != 0L | !is.null(req$id)) {
        res <- .asTeam(req)
        res$auth <- .self$auth
      }

      # multiple teams
      if (length(req$items) != 0L & is.null(req$id)) {
        res <- .asTeamList(req)
      }

      res
    },

    # create a team ------------------------------------------------------------
    create_team = function(name = NULL, ...) {
      "Create a team."
      if (is.null(name)) {
        stop("Please provide the team name")
      }

      req <- auth$api(
        path = "teams", method = "POST",
        body = list("division" = .self$id, "name" = name), ...
      )

      res <- .asTeam(req)
      res$auth <- .self$auth

      res
    },

    # show ---------------------------------------------------------------------
    show = function() {
      .showFields(.self, "== Division ==", c("id", "name"))
    }
  )
)

# .asDivision ------------------------------------------------------------------
.asDivision <- function(x) {
  Division(id = x$id, name = x$name, href = x$href, response = response(x))
}

# DivisionList class -----------------------------------------------------------
DivisionList <- setListClass("Division", contains = "Item0")

# .asDivisionList --------------------------------------------------------------
.asDivisionList <- function(x) {
  obj <- DivisionList(lapply(x$items, .asDivision))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}
