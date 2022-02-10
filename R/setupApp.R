setupApp <- function(questionnaire_file){
  
  # Loading of EU-EpiCap questionnaire  -----------------------------------------

  #Read in questionnaire from file and turn into df
  questionnaire <<- readQuestionnaire(questionnaire_file)
  #Turn questionnaire df into commands that will build questionnaire pages in App
  commands <<- questionnaire2Commands(questionnaire) 
 
  # Setup for plotting of results ----------------------------------------------------------------
  
  setupRadarPlot()

  # Setup for benchmarking ------------------------------------------------------------
  
  #Identify available reference datasets (for use in benchmarking dropdown menu)
  ref_files<<-list.files("data/reference_datasets", pattern=".+_ST.+")
  ref_datasets<<-unique(sub(pattern = "_ST.+",replacement="",ref_files))
  
}