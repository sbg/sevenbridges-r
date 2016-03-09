ToolUI <- function(){
    ## Get the document context.
    context <- getActiveDocumentContext()
    
    # Set the default data to use based on the selection.
    text <- context$selection[[1]]$text
    defaultData <- text
    
    # Generate UI for the gadget.
    ui <- miniPage(
        gadgetTitleBar("Describe your tool"),
        miniCotentPanel(
            stableColumnLayout(
                textInput("data", "Data", value = defaultData),
                textInput("subset", "Subset Expression")
            ),
            uiOutput("pending")
        )
    )
    
    
    # Server code for the gadget.
    server <- function(input, output, session) {
        
    }
}
