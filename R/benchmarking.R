#Helper function: create reference data file from multiple EU-Epicap profiles
#createRefDataset <- function(files, outputname){
#  
#  #if .rds file
#  if(tools::file_ext(userFile()$datapath)=="rds"){
#    readRDS(userFile()$datapath)  
#    #if .csv file
#  } else {
#    df <- read.csv(userFile()$datapath, header = TRUE, sep = ",", stringsAsFactors = stringsAsFactors, colClasses="character", na.strings = NULL)
#    setNames(as.list(df$Value),df$Question)
#  } 
#}

#Helper function: formats a reference data file into scoring table format
formatRefDataset <- function(refdatafile){
  
  source("R/scoringHelpers.R")
  
  #Check if questionnaire exists (in the global env), and if not read in questionnaire (within function)
  if(!exists("questionnaire")){
    source("R/questionnaireHelpers.R")
    qdata <- readQuestionnaire("data/EU-EpiCap_Questionnaire_21_11_30.xlsx") #Need to keep this updated or make more generic
    questionnaire <- qdata$questionnaire
  }
  #Read in ref data (read.csv or readRDS)
  #refdatafile<-"Data/reference_datasets/example_benchmark_data.csv"
  refdata <- read.csv(refdatafile)
  
  #Attach ref data values to questionnaire
  qwv<- cbind(
    questionnaire,
    Chosen_value = refdata$Value, #needs refdata to be read (i.e. read.csv or readRDS somewhere)
    low = refdata$Low,
    high = refdata$High
  )
  
  #Create scoring table and save to file
  for (level in c("targets","indicators")){
    write.csv(scoringTable(qwv,level,reference=TRUE), #this creates scoring table with sample tooltips (change in scoringHelpers as appropriate)
              file=paste0("data/reference_datasets/",basename(tools::file_path_sans_ext(refdatafile)),"_ST",level,".csv"),
              row.names=FALSE
    )
  }
}

# Module UI function
benchmarkUI <- function(id, label = "benchmark", ref_datasets) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    selectInput(inputId = ns("selected_ref"), label = "Select a reference dataset",choices=c("Select a reference dataset", ref_datasets), selectize=TRUE),
    girafeOutput(ns("benchmark_all")),
    girafeOutput(ns("benchmark_1")),
    girafeOutput(ns("benchmark_2")),
    girafeOutput(ns("benchmark_3"))
  )
}

# Module server function
benchmarkServer <- function(id, scores_targets=scores_targets, scores_indicators=scores_indicators, ref_files=ref_files, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #read in reference scoring tables
      selected_ref_files<-reactive({ref_files[which(str_detect(ref_files,input$selected_ref))]})
      ref_targets<-reactive({read.csv(paste0("data/reference_datasets/",as.character(selected_ref_files()[which(str_detect(ref_files,"STtargets"))])))})
      ref_indic<-reactive({read.csv(paste0("data/reference_datasets/",as.character(selected_ref_files()[which(str_detect(ref_files,"STindicators"))])))})
      #generate radarcharts
      output$"benchmark_all" <- renderGirafe(makeRadarPlot_benchmark(scores_targets(),3,ref_targets()))
      output$"benchmark_1" <- renderGirafe(makeRadarPlot_benchmark(scores_indicators()[1:20,],4,ref_indic()[1:16,]))
      output$"benchmark_2" <- renderGirafe(makeRadarPlot_benchmark(scores_indicators()[21:40,],4,ref_indic()[17:32,]))
      output$"benchmark_3" <- renderGirafe(makeRadarPlot_benchmark(scores_indicators()[41:60,],4,ref_indic()[33:48,]))
      }
  )    
}