#This file contains functions that score the completed questionnaire and link it to the visualisations
#- addScores2Questionnaire() extracts the values selected in the questionnaire and adds these to a reactive version of the questionnaire df
#- scoringTable() creates scoring tables (summarises questionnaire_w_values) for use in plotting

# To test these functions, start out with these lines:
questionnaire_w_values <- readQuestionnaire("data/Questionnaire_db_test.xlsx")
questionnaire_w_values$Chosen_value <- as.character(sample(1:4,16,replace=TRUE))
questionnaire_w_values$Comment <- (sample(c("a","b",""),16,replace=TRUE))


dupl_questionnaire <- function(questionnaire,n){
  questionnaire2 <- questionnaire
  questionnaire2$Dimension <- gsub("1",n,questionnaire2$Dimension,fixed=TRUE)
  questionnaire2$Target <- gsub("1.",paste(n,"."),questionnaire2$Target,fixed=TRUE)
  questionnaire2$ID <- gsub("1.",paste(n,"."),questionnaire2$ID,fixed=TRUE)
  return(questionnaire2)
}

questionnaire_w_values<-rbind(questionnaire_w_values,dupl_questionnaire(questionnaire_w_values,2),dupl_questionnaire(questionnaire_w_values,3))



# addScores2Questionnaire -------------------------------------------------

# This function extracts questionnaire choices, and adds them to the questionnaire dataframe as column "Chosen_value" 

addScores2Questionnaire <- function(input, questionnaire) {
  
  q_values <- reactive({
    sapply(grep(pattern="Q[[:digit:]]", x=names(input), value=TRUE), function(x) input[[x]])
  })
  
  comments <- reactive({
    sapply(grep(pattern="C[[:digit:]]", x=names(input), value=TRUE), function(x) input[[x]]) %>%
      gsub(pattern="^(.+)$",replacement="\n Added comment(s):\n'\\1'")
  })
  
  reactive({
    cbind(
      questionnaire,
      Chosen_value = q_values()[sort(names(q_values()))], #N.B. Chosen_value contains characters!
      Comment = comments()[sort(names(comments()))]
    )
  })
}


# scoringTable ------------------------------------------------------------

# Need to fix so generates either a plot or an informative error message when questions not completed!

# This function creates scoring tables for use in plotting.
# If dimension = "all", it summarises the questionnaire df by target: it sums the scores, and generates tooltip texts with score breakdowns
# If dimension = 1,2,3 or character equivalents, it filters the indicator scores that are part of the selected dimension,and generates tooltip texts that correspond to the selected questionnaire option + any comments

scoringTable <- function(questionnaire_w_values, dimension) {
  
  origin_points <- list(x=0,variable=NA,value=0,tooltip="") # a dataframe row to represent (0,0) points with no tooltip or label
  
  if(dimension == "all"){
    target_scores <- summarise(group_by(questionnaire_w_values,
                                        variable=Target),
                               value = sum(as.numeric(Chosen_value)),
                               tooltip = paste(ID,Indicators,"-",Chosen_value,collapse="\n")) %>%
      mutate(x=seq(15,345,30))
    
    scores_df <- rbind(origin_points,target_scores[1:4,], origin_points,target_scores[5:8,], origin_points,target_scores[9:12,])                  

  }
  else if(dimension %in% list(1,2,3,"1","2","3")) { #if dimension given as a number
    target_scores <- questionnaire_w_values %>%
      filter(grepl(as.character(dimension), Dimension, fixed=TRUE)) %>% #filters rows for one particular dimension
      mutate(variable = paste(ID,Indicators),
             value = as.numeric(Chosen_value),
             tooltip = paste0(str_match(Options,paste(Chosen_value,'\\. *([^"]+)\\"',sep=''))[,2],
                             '\n',
                             Comment)) %>%
      select(variable, value, tooltip) %>%
      mutate(x=c(seq(9,81,24),seq(99,171,24),seq(189,261,24),seq(279,351,24)))
    
    scores_df <- rbind(origin_points,target_scores[1:4,], origin_points,target_scores[5:8,], origin_points,target_scores[9:12,], origin_points,target_scores[13:16,])     
  }
  scores_df$x <- as.numeric(scores_df$x)
  scores_df$value <- as.numeric(scores_df$value)
  return(scores_df)

}


