#Helper function: create reference data file from multiple OH-Epicap profiles
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
    h2("Compare your OH-EpiCap profile with a reference dataset"),
    fluidRow(
      box(width=12,
          p("This page allows the user to visually compare the completed or uploaded OH-EpiCap profile to a selected reference dataset")
      )),
    fluidRow(
      box(width=12,
          title="Reference dataset",
          solidHeader=TRUE, status="danger",
          collapsible=TRUE, collapsed=FALSE,
          fluidRow(column(6, selectInput(inputId = ns("selected_ref"), label = "Please select a reference dataset",choices=c("Select a reference dataset", ref_datasets), selectize=TRUE)),
                   column(6, textOutput(ns("bmtxt_refdata"))))
      )),
    fluidRow(
      box(width=12,
          title="Targets",
          solidHeader=TRUE, status="info",
          collapsible=TRUE, collapsed=FALSE,
          fluidRow(
            column(6,
                   p("This section shows the comparison between the completed/uploaded OH-EpiCap profile and the selected reference dataset, across the twelve targets."),
                   p("The lightly coloured area depicts the interquartile range (IQR) of the relevant target score in the reference dataset, with the + symbol indicating the median."),
                   p("If a data point (filled circle) falls within the coloured area, the target score from the OH-EpiCap profile is within the range of the benchmark target."),
                   uiOutput(ns("bmtxt_targets"))),
                   column(6, girafeOutput(ns("benchmark_all"))))
      )),
    fluidRow(
      box(width=12,
          title="Dimension 1: Organization",
          solidHeader=TRUE, status="warning",
          collapsible=TRUE, collapsed=TRUE,
          fluidRow(
            column(6, 
                   p("This section shows the comparison between the completed/uploaded OH-EpiCap profile and the selected reference dataset, across the indicators of Dimension 1 (Organization)."),
                   p("The lightly coloured area depicts the interquartile range (IQR) of the relevant indicator score in the reference dataset, with the + symbol indicating the median."),
                   p("If a data point (filled circle) falls within the coloured area, the indicator score from the OH-EpiCap profile is within the range of the benchmark target."),
                   uiOutput(ns("bmtxt_dim1"))),
            column(6, girafeOutput(ns("benchmark_1"))))
      )),
    fluidRow(
      box(width=12,
          title="Dimension 2: Operations",
          solidHeader=TRUE, status="primary",
          collapsible=TRUE, collapsed=TRUE,
          fluidRow(
            column(6, 
                   p("This section shows the comparison between the completed/uploaded OH-EpiCap profile and the selected reference dataset, across the indicators of Dimension 2 (Operations)."),
                   p("The lightly coloured area depicts the interquartile range (IQR) of the relevant indicator score in the reference dataset, with the + symbol indicating the median."),
                   p("If a data point (filled circle) falls within the coloured area, the indicator score from the OH-EpiCap profile is within the range of the benchmark target."),                   
                   uiOutput(ns("bmtxt_dim2"))),
            column(6, girafeOutput(ns("benchmark_2"))))
      )),
    fluidRow(
      box(width=12,
          title="Dimension 3: Impact",
          solidHeader=TRUE,
          collapsible=TRUE, collapsed=TRUE,
          fluidRow(
            column(6,
                   p("This section shows the comparison between the completed/uploaded OH-EpiCap profile and the selected reference dataset, across the indicators of Dimension 3 (Impact)."),
                   p("The lightly coloured area depicts the interquartile range (IQR) of the relevant indicator score in the reference dataset, with the + symbol indicating the median."),
                   p("If a data point (filled circle) falls within the coloured area, the indicator score from the OH-EpiCap profile is within the range of the benchmark target."),                   
                   uiOutput(ns("bmtxt_dim3"))),
            column(6, girafeOutput(ns("benchmark_3"))))
      )),
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
      output$benchmark_all <- renderGirafe(makeRadarPlot_benchmark(scores_targets(),3,ref_targets()))
      output$benchmark_1 <- renderGirafe(makeRadarPlot_benchmark(scores_indicators()[1:20,],4,ref_indic()[1:16,]))
      output$benchmark_2 <- renderGirafe(makeRadarPlot_benchmark(scores_indicators()[21:40,],4,ref_indic()[17:32,]))
      output$benchmark_3 <- renderGirafe(makeRadarPlot_benchmark(scores_indicators()[41:60,],4,ref_indic()[33:48,]))
      #lists of targets/indicators with low scores
      source("R/scoringHelpers.R")
      targets_low<-id_low_scores(scores_targets(),ref_targets()$low)
      targets_high<-id_high_scores(scores_targets(),ref_targets()$high)
      dim1_low<-id_low_scores(scores_indicators()[1:20,],ref_indic()[1:16,]$low)
      dim1_high<-id_high_scores(scores_indicators()[1:20,],ref_indic()[1:16,]$high)
      dim2_low<-id_low_scores(scores_indicators()[21:40,],ref_indic()[17:32,]$low)
      dim2_high<-id_high_scores(scores_indicators()[21:40,],ref_indic()[17:32,]$high)
      dim3_low<-id_low_scores(scores_indicators()[41:60,],ref_indic()[33:48,]$low)
      dim3_high<-id_high_scores(scores_indicators()[41:60,],ref_indic()[33:48,]$high)
      #generate benchmark texts
      output$bmtxt_refdata <- renderText({"Sample text describing the reference dataset"})
      output$bmtxt_targets <- renderUI({HTML(paste0("Targets exceeding the benchmark range, are: ",targets_high(),".<br><br>Targets falling below the benchmark range, are: ",targets_low(),"."))})
      output$bmtxt_dim1 <- renderUI({HTML(paste0("Indicators exceeding the benchmark range, are: ",dim1_high(),".<br><br>Indicators falling below the benchmark range, are: ",dim1_low(),"."))})
      output$bmtxt_dim2 <- renderUI({HTML(paste0("Indicators exceeding the benchmark range, are: ",dim2_high(),".<br><br>Indicators falling below the benchmark range, are: ",dim2_low(),"."))})
      output$bmtxt_dim3 <- renderUI({HTML(paste0("Indicators exceeding the benchmark range, are: ",dim3_high(),".<br><br>Indicators falling below the benchmark range, are: ",dim3_low(),"."))})
      }
  )    
}