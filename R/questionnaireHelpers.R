library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)
library(Rcpp)

#This file contains 3 functions that build the questionnaire pages:
#- readQuestionnaire() reads an Excel-format questionnaire into a usable df
#- questionnaire2Commands() turns the questionnaire df into a df with commands to build the content for the Questionnaire pages on the EU-EpiCap app.
#- buildQuestionnaireUI() runs the commands df, and thus builds the Questionnaire pages


# readQuestionnaire -------------------------------------------------------

# This function turns the Excel-format questionnaire (single sheet) into a usable dataframe using
# code ideas from https://nacnudus.github.io/spreadsheet-munging-strategies/

readQuestionnaire <- function(datafile) {

  #Read the xlsx data from questionnaire sheets into an R tibble / dataframe
  questionnaire <- xlsx_cells(datafile, sheets=(2:length(xlsx_sheet_names(datafile)))) %>% #skipping sheet with preliminary questions
    dplyr::filter(!is_blank) %>%
    select(sheet,row,col,data_type,character)
  
  #remove leading and trailing whitespaces
  questionnaire$character<-trimws(questionnaire$character)
  
  #extract generic dimension data, and remove dimension text from questionnaire
  questionnaire<-questionnaire[!(questionnaire$row==2 & questionnaire$col==1),]
  
  #If "Dimension"/"Target" stated in 2nd column:    
  #change the column number of cells containing the word "Dimension" (or "Target") to 1 - so can "behead"
  questionnaire[grep('Dimension',x=questionnaire$character,fixed=TRUE), ]$col<-1
  questionnaire[grep('Target',x=questionnaire$character,fixed=TRUE), ]$col<-1
  
  pageprep <- function(sheet){

    #If numeric indicator ID not given a header, add this post-hoc:
    #Add row for numeric ID header 'ID'
    sheet<-rbind(sheet[1,],c(3,1,'character','ID'),sheet[-1,])
    sheet$row<-as.integer(sheet$row)
    sheet$col<-as.integer(sheet$col)
    
    #Extract "Dimension" and "Header"
    sheet <- sheet %>%
      behead("up-left",Dimension) %>%
      behead("up",Header)
    
    #Filter for "Target" headers and assign them to separate variable
    Target <-
      sheet %>%
      dplyr::filter(grepl("Target", character, fixed=TRUE)) %>%
      select(row, col, Target = character)
    
    #Take out Target rows and assign as separate column, then adds columns for each Header item
    sheet <- sheet %>% 
      dplyr::filter(!grepl("Target", character, fixed=TRUE)) %>% 
      enhead(Target, "left-up") %>%
      select(-col) %>%
      spatter(Header) %>%
      select(-row)

  }
  
  #Split up data into a tibble for each sheet (i.e. Dimension), run pageprep function on each, and merge
  questionnaire <- questionnaire %>% 
    nest(-sheet) %>% 
    mutate(data = map(data,pageprep)) %>% 
    unnest(cols=c(data)) %>%
    select(-sheet)
  
  #Split up "Possible Answers" (one long string) into character vector "Options" with separate options, based on new line character
  questionnaire$Options <- strsplit(questionnaire$`Possible answers`,"(\r)*\n")
  questionnaire <- select(questionnaire,-`Possible answers`)
  
  #Split up "Questions" into "Question" and "Notes"
  questionnaire <- questionnaire %>% separate(Questions, c("Question","Notes"),sep="\\?",fill="right")
  questionnaire$Question<-paste0(questionnaire$Question,"?")
  questionnaire$Notes<-trimws(questionnaire$Notes)
  questionnaire$Colour <- c(rep("#F47931",16),rep("#00679C",16),rep("#CECECE",16))
  questionnaire$Transparency <- rep(c(rep(1,4),rep(0.9,4),rep(0.8,4),rep(0.7,4)),3)


  return(questionnaire)
}


# questionnaire2Commands --------------------------------------------------

# This function turns the questionnaire df into a commands df for the Shiny app.
# The new df alternates commands to generate:
#   (1) Indicator name in bold text  -  command: strong(), arguments: indicator
#   (2) Indicator question + notes in normal text  -  command: p(), arguments: question
#   (3) Radiobuttons  -  command: radioButtons(), arguments: rb_args

questionnaire2Commands <- function(questionnaire) {
  
  #Add column with values of the options (NA,1,2,3,4 - as strings)
  questionnaire$Values<-lapply(questionnaire$Options,function(x) {sub(pattern = "\\..+$","",x,fixed=FALSE)})
  #Combine "ID", "Options", and "Values" columns into a list of arguments for the radioButtons command
  questionnaire <- within(questionnaire,Rb_args<-paste0('list(inputId = "Q',ID,'", label = "", selected = character(0), width = "100%", choiceNames=',Options,', choiceValues=',Values,')'))
  #Add "list()" to Indicators, Question, Notes, Target to make argument lists for do.call
  questionnaire <- within(questionnaire,Indicator<-paste0('list("',Indicators,'")'))
  questionnaire <- within(questionnaire,Question<-paste0('list("',Question,'")'))
  questionnaire <- within(questionnaire,Notes<-paste0('list(tags$i("',gsub("\r\n",'",br(),"',Notes),'"))'))
  questionnaire <- within(questionnaire,Target<-paste0('list(id=paste0("T","',str_match(Target,"\\d\\.\\d"),'"),h3("',Target,'"))'))
  #Add option to comment (textInput) for each indicator
  questionnaire <- within(questionnaire,Comment_box<-paste0('list(inputId = "C',ID,'", label = NULL, placeholder = "Type here any comments to supplement your answer", width = "100%")'))
  
  #Make commands df: Extract Indicator, Question and Rb_args columns; rename cols; turn into "long matrix" format
  #Also extracts Dimension column, but keeps this out of the longmatrix format, to facilitate filtering
  commands <-
    questionnaire[c("Dimension","Target","Indicator","Question","Notes","Rb_args","Comment_box")] %>%
    `colnames<-`(c("Dimension","div","strong","p","tagList","radioButtons","textInput")) %>%  
    pivot_longer(cols=c(2,3,4,5,6,7),names_to="Command",values_to="Arguments") %>%
    unique()
  
  return(commands)
}



# buildQuestionnaireUI ----------------------------------------------------

# This function builds the questionnaire UI (text and input devices) from the 'commands' dataframe

buildQuestionnaireUI <- function(commands) {
     apply(commands,1,function(x){do.call(x[['Command']],args=eval(parse(text=x[['Arguments']])))})
}

