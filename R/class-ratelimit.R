# Rate limit
Rate <- setRefClass(
    "Rate", contains = "Item",

    fields = list(
        limit     = "numeric",
        remaining = "numeric",
        reset     = "numeric"),

    methods = list(

        show = function() {
            .showFields(
                .self, "== Rate Limit ==",
                values = c("limit", "remaining", "reset"))
        }

    ))

.asRate <- function(x) {
    Rate(limit     = x$rate$limit,
         remaining = x$rate$remaining,
         reset     = x$rate$reset,
         response  = response(x))
}
