#This is script uploads .csv and .rds files (in order to restore the state of the questionnaire)

# Module UI function
questionnaireUploadUI <- function(id, label = "upload") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label, accept=c("text/csv", "text/comma-separated-values", ".csv", ".rds")),
    htmlOutput(ns("msg"))
   )
}

# Module server function
questionnaireUploadServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        extension<-tools::file_ext(input$file$datapath)
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        # If not .rds or .csv, print error message (shown in the textoutput)
        validate(need(extension %in% c("rds","csv"), message =  "Invalid file. Please upload a .csv or .rds file"))
        input$file
      })
      
      # Read and save list of inputs  
      state <- reactive({
        #if .rds file
        if(tools::file_ext(userFile()$datapath)=="rds"){
          readRDS(userFile()$datapath)  
        #if .csv file
        } else {
          df <- read.csv(userFile()$datapath, header = TRUE, sep = ",", stringsAsFactors = stringsAsFactors, colClasses="character", na.strings = NULL)
          setNames(as.list(df$Value),df$Question)
        } 
        })

      # Some output is required to allow the display of the error msg in case of wrong file format
      # This can also display a message upon succesful upload (or could check data format and send people to next page)
      output$msg<-renderUI(
        HTML(       # renderUI with HTML, to allow for correct display of text with new lines
          paste(paste0("File ",userFile()$name, " was successfully uploaded."),
                   "If the uploaded questionnaire is complete, please visit the Results page for visualisation.",
                   "If the uploaded questionnaire is only partially completed, please visit the Dimension-specific pages to complete your submission.",
                   sep="<br/>"
                )))

      # Return the reactive that yields the rds state / data frame
      return(state)
    }
  )    
}