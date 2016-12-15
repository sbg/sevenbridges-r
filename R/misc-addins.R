# From rstudio/addinexamples::R/utils.R
stableColumnLayout = function(...) {
    dots  = list(...)
    n     = length(dots)
    width = 12 / n
    class = sprintf("col-xs-%s col-md-%s", width, width)
    fluidRow(
        lapply(dots, function(el) {
            div(class = class, el)
        })
    )
}

#' Create Rabix tool object
#'
#' Call this as an addin to create an Rabix tool object.
#'
#' @return An RStudio addin
#'
#' @export ToolUI
#'
#' @examples
#' NULL
ToolUI = function() {

    # Get the document context
    context <- getActiveDocumentContext()

    # Set the default data to use based on the selection
    text <- context$selection[[1]]$text
    defaultData <- text

    # Generate UI for the gadget
    ui <- miniPage(
        gadgetTitleBar("Describe your tool"),
        miniContentPanel(
            stableColumnLayout(
                textInput("data", "Data", value = defaultData),
                textInput("subset", "Subset Expression")
            ),
            uiOutput("pending")
        )
    )

    # Server code for the gadget
    server <- function(input, output, session) {

        # TODO

    }

}
