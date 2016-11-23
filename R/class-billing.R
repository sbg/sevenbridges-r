# TODO: when it got stable, define billing object
# PrivilegesEnum <- setSingleEnum("Privileges", c(""))
# Billing
# to make it simple to update, return a list, not an object,
# because no action defined an this object
Billing <- setRefClass("Billing", contains = "Item",
                       fields = list(
                           id                = "characterORNULL",
                           name              = "characterORNULL",
                           owner             = "characterORNULL",
                           privileges        = "listORNULL",
                           type              = "characterORNULL",
                           pending           = "logicalORNULL",
                           disabled          = "logicalORNULL",
                           active            = "logicalORNULL",
                           balance           = "listORNULL",
                           project_breakdown = "listORNULL",
                           total_spending    = "listORNULL"),  # 1.1

                       methods = list(

                           initialize = function(
                               id                = NULL,
                               name              = NULL,
                               owner             = NULL,
                               privileges        = list(),
                               type              = NULL,
                               pending           = NULL,
                               disabled          = NULL,
                               active            = NULL,
                               balance           = list(),
                               project_breakdown = list(),
                               total_spending    = list(), ...) {

                               id         <<- id
                               name       <<- name
                               owner      <<- owner
                               privileges <<- privileges
                               type       <<- type
                               disabled   <<- disabled
                               active     <<- active
                               balance    <<- balance

                               # for breakdown
                               project_breakdown <<- project_breakdown
                               total_spending    <<- total_spending

                               callSuper(...)

                           },

                           show = function() {
                               .showFields(
                                   .self, "== Billing ==",
                                   values = c("id", "href", "name",
                                              "owner", "privileges", "type",
                                              "disabled", "active", "balance",
                                              "project_breakdown", "total_spending"))
                           }

                       ))

.asBilling <- function(x) {

    Billing(id         = x$id,
            href       = x$href,
            name       = x$name,
            owner      = x$owner,
            privileges = x$privileges,
            type       = x$type,
            disabled   = x$disabled,
            active     = x$active,
            balance    = x$balance,
            response   = response(x),
            # for breakdown
            project_breakdown = x$project_breakdown,
            total_spending    = x$total_spending)

}

BillingList <- setListClass("Billing", contains = "Item0")

.asBillingList <- function(x) {

    obj <- BillingList(lapply(x$items, .asBilling))
    obj@href <- x$href
    obj@response <- response(x)
    obj

}
