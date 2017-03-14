User <- setRefClass(

    "User",

    contains = "Item",

    fields = c(
        "username", "email",
        "first_name", "last_name",
        "affiliation", "phone", "address",
        "city", "state", "country", "zip_code",
        "projects", "billing_groups", "tasks"),

    methods = list(

        initialize = function(
            username       = "",
            email          = "",
            first_name     = "",
            last_name      = "",
            affiliation    = "",
            phone          = "",
            address        = "",
            city           = "",
            state          = "",
            country        = "",
            zip_code       = "",
            projects       = "",
            billing_groups = "",
            tasks          = "", ...) {

            username       <<- username
            email          <<- email
            first_name     <<- first_name
            last_name      <<- last_name
            affiliation    <<- affiliation
            phone          <<- phone
            address        <<- address
            city           <<- city
            state          <<- state
            country        <<- country
            zip_code       <<- zip_code
            projects       <<- projects
            billing_groups <<- billing_groups
            tasks          <<- tasks

            callSuper(...)

        },

        show = function() {
            .showFields(
                .self,
                "== User ==",
                values = c(
                    "href",
                    "username",
                    "email",
                    "first_name",
                    "last_name",
                    "affiliation",
                    "phone",
                    "address",
                    "city",
                    "state",
                    "country",
                    "zip_code",
                    "projects",
                    "billing_groups",
                    "tasks"))
        }

    ))

.asUser <- function(x) {

    User(href           = x$href,
         username       = x$username,
         email          = x$email,
         first_name     = x$first_name,
         last_name      = x$last_name,
         affiliation    = x$affiliation,
         phone          = x$phone,
         address        = x$addrss,
         city           = x$city,
         state          = x$state,
         country        = x$country,
         zip_code       = x$zip_code,
         projects       = x$projects,
         billing_groups = x$billing_groups,
         tasks          = x$tasks,
         response       = response(x))

}
