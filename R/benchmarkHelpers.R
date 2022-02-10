
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