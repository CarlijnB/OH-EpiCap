questionnaireDownloadUI <- function(id, label = "download") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("saveRds"), "Save questionnaire answers as .rds"),
    downloadButton(ns("saveCsv"), "Save questionnaire answers as .csv")
  )
}

# Module server function
questionnaireDownloadServer <- function(id, stringsAsFactors, inputlist) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #save as .Rds
      output$saveRds <- downloadHandler(
        filename = "questionnaire_answers.rds", #can make this depend on prelim questions?
        content = function(file){
          saveRDS(inputlist(), file)
        }
      )
      
      #save as .csv
      output$saveCsv <- downloadHandler(
        filename = "questionnaire_answers.csv", #can make this depend on prelim questions?
        content = function(file){
          write.table(
            matrix(c(names(inputlist()),inputlist()),ncol=2)[order(as.numeric(substring(names(inputlist()),2)),rev(names(inputlist())),decreasing=FALSE),], 
            row.names=FALSE,
            col.names=c("Question","Value"),
            sep=",",
            file=file)
        }
      )
    }
  )    
}