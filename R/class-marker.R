# Marker class -----------------------------------------------------------------
Marker <- setRefClass(
  "Marker",
  contains = "Item",

  fields = list(
    id = "characterORNULL",
    name = "characterORNULL",
    file = "characterORNULL",
    chromosome = "characterORNULL",
    position = "listORNULL",
    created_time = "characterORNULL",
    created_by = "characterORNULL",
    private = "logicalORNULL"
  ),

  methods = list(

    # initialize ---------------------------------------------------------------
    initialize = function(id = NULL, name = "", file = NULL,
                              chromosome = "", position = list(),
                              created_time = "", created_by = NULL,
                              private = TRUE, ...) {
      id <<- id
      name <<- name
      file <<- file
      chromosome <<- chromosome
      position <<- position
      created_time <<- created_time
      created_by <<- created_by
      private <<- private

      callSuper(...)
    },

    # modify a marker ----------------------------------------------------------
    modify = function() {
      "Modify a marker."
      NULL
    },

    # delete a marker ----------------------------------------------------------
    delete = function() {
      "Delete a marker."
      NULL
    },

    # show ---------------------------------------------------------------------
    show = function() {
      .showFields(
        .self,
        "== Marker ==",
        c("id", "name", "file", "chromosome", "position", "created_time", "created_by", "private")
      )
    }
  )
)

# .asMarker --------------------------------------------------------------------
.asMarker <- function(x) {
  Marker(
    id = x$id,
    name = x$name,
    file = x$file,
    chromosome = x$chromosome,
    position = x$position,
    created_time = x$created_time,
    created_by = x$created_by,
    private = x$private,
    href = x$href,
    response = response(x)
  )
}

# MarkerList class -------------------------------------------------------------
MarkerList <- setListClass("Marker", contains = "Item0")

# .asMarkerList ----------------------------------------------------------------
.asMarkerList <- function(x) {
  obj <- MarkerList(lapply(x$items, .asMarker))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}
