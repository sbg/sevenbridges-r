# Team Member class ------------------------------------------------------------
TeamMember <- setRefClass(
  "TeamMember",
  contains = "Item",

  fields = list(username = "characterORNULL", role = "characterORNULL"),

  methods = list(

    # initialize ---------------------------------------------------------------
    initialize = function(username = NULL, role = NULL, ...) {
      username <<- username
      role <<- role

      callSuper(...)
    },

    # show ---------------------------------------------------------------------
    show = function() {
      .showFields(.self, "== Team Member ==", c("username", "role"))
    }
  )
)

# .asTeamMember ----------------------------------------------------------------
.asTeamMember <- function(x) {
  TeamMember(username = x$username, role = x$role, href = x$href)
}

# TeamMemberList class ---------------------------------------------------------
TeamMemberList <- setListClass("TeamMember", contains = "Item0")

# .asTeamMemberList ------------------------------------------------------------
.asTeamMemberList <- function(x) {
  obj <- TeamMemberList(lapply(x$items, .asTeamMember))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}
