library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)
library(Rcpp)

#This file contains 2 functions that together turn an Excel-format questionnaire, 
#into a df with commands to build the content for the Questionnaire pages on the EU-EpiCap app

readQuestionnaire <- function(datafile) {
  
  # This function turns the Excel-format questionnaire (single sheet) into a usable dataframe using
  # code ideas from https://nacnudus.github.io/spreadsheet-munging-strategies/
  
  #Read the xlsx data from 1st sheet into an R tibble / dataframe
  questionnaire <- xlsx_cells(datafile, sheets=1) %>% #1st sheet only
    dplyr::filter(!is_blank) %>%
    select(row,col,data_type,character)
  
  #If "Dimension"/"Target" stated in 2nd column:    
  #change the column number of cells containing the word "Dimension" (or "Target") to 1 - so can "behead"
  questionnaire[grep('Dimension',x=questionnaire$character,fixed=TRUE), ]$col<-1
  questionnaire[grep('Target',x=questionnaire$character,fixed=TRUE), ]$col<-1
  
  #If numeric indicator ID not given a header, add this post-hoc:
  #Add row for numeric ID header 'ID'
  questionnaire<-rbind(questionnaire[1,],c(2,1,'character','ID'),questionnaire[-1,])
  questionnaire$row<-as.integer(questionnaire$row)
  questionnaire$col<-as.integer(questionnaire$col)
  
  #Extract "Dimension"
  questionnaire <- questionnaire %>%
    behead("up-left",Dimension) %>%
    behead("up",Header)
  
  #Filter for "Target" headers and assign them to separate variable
  targets <-
    questionnaire %>%
    dplyr::filter(grepl("Target", character, fixed=TRUE)) %>%
    select(row, col, Target = character)
  
  #partition the data by Target (identifiying the starting cell of each target)
  partitions<-partition(questionnaire,targets[,c(1,2)])
  
  #Function to be applied to each partition: takes out Target header and assigns as separate column, then adds columns for each Header item
  unpivot <- function(cells) {
    cells %>%
      behead("up-left", Target) %>%
      select(-col) %>%
      spatter(Header) %>%
      select(-row)
  }
  
  #Apply unpivot function to each partition
  questionnaire <-
    partitions %>%
    mutate(cells = map(cells,unpivot)) %>%
    unnest() %>%
    select(-corner_row,-corner_col)
  
  #Split up "Possible Answers" (one long string) into character vector "Options" with separate options, based on new line character
  questionnaire$Options <- strsplit(questionnaire$`Possible answers `,"\r\n")
  questionnaire <- select(questionnaire,-`Possible answers `)
  
  return(questionnaire)
}

questionnaire2Commands <- function(questionnaire) {
  
# This function turns the questionnaire df into a commands df for the Shiny app.
# The new df alternates commands to generate:
#   (1) Indicator number + name in bold text  -  command: strong(), arguments: paste0(id, indicator)
#   (2) Indicator question + notes in normal text  -  command: p(), arguments: question
#   (3) Radiobuttons  -  command: radioButtons(), arguments: rb_args

  #Combine "ID" and "Options" columns into a list of arguments for the radioButtons command
  questionnaire <- within(questionnaire,Rb_args<-paste0('list(inputId = "Q',ID,'", label = NULL, width = "100%", choices=',Options,')'))
  #Add "list()" to ID+Indicators, and to Questions, to make argument lists for do.call
  questionnaire <- within(questionnaire,Indicator<-paste0('list("',ID,' ',Indicators,'")'))
  questionnaire <- within(questionnaire,Question<-paste0('list("',`Questions `,'")'))
  
  #Make commands df: Extract Indicator, Question and Rb_args columns; rename cols; turn into "long matrix" format
  commands <-
    questionnaire[c("Indicator","Question","Rb_args")] %>%
    `colnames<-`(c("strong","p","radioButtons")) %>%
    pivot_longer(cols=c(1,2,3),names_to="Command",values_to="Arguments")
  
  return(commands)
}