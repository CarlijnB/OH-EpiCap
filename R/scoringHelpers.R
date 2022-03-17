#This file contains functions that score the completed questionnaire and link it to the visualisations
#- addScores2Questionnaire() extracts the values selected in the questionnaire and adds these to a reactive version of the questionnaire df
#- scoringTable() creates scoring tables (summarises questionnaire_w_values) for use in plotting

# To test these functions, start out with these lines:
#questionnaire_w_values <- readQuestionnaire("data/EU-EpiCap_Questionnaire_21_11_30.xlsx")
#questionnaire_w_values$Chosen_value <- sample(1:4,48,replace=TRUE)
#questionnaire_w_values$Comment <- (sample(c("a","b",""),48,replace=TRUE))


# addScores2Questionnaire -------------------------------------------------

# This function extracts questionnaire choices, and adds them to the questionnaire dataframe as column "Chosen_value" 

addScores2Questionnaire <- function(input, questionnaire) {
  
  q_values <- reactive({
    sapply(grep(pattern="Q[[:digit:]]", x=names(input), value=TRUE), function(x) as.numeric(input[[x]]))
  })
  
  comments <- reactive({
    sapply(grep(pattern="C[[:digit:]]", x=names(input), value=TRUE), function(x) input[[x]]) %>%
      gsub(pattern="^(.+)$",replacement="\n Added comment(s):\n'\\1'")
  })
  
  reactive({
    cbind(
      questionnaire,
      Chosen_value = q_values()[sort(names(q_values()))], 
      Comment = comments()[sort(names(comments()))]
    )
  })
}


# scoringTable ------------------------------------------------------------

# Need to fix so generates either a plot or an informative error message when questions not completed!

# This function creates scoring tables for use in plotting.
# If level = "targets", it summarises the questionnaire df by target: it sums the scores, and generates tooltip texts with score breakdowns
# If level = "indicators", it shows indicator values, and generates tooltip texts that correspond to the selected questionnaire option + any comments

# The reference argument indicates use of the function to create scoring tables for a benchmark reference dataset:
# If reference = TRUE, comments are not included, and tooltips show ... 

scoringTable <- function(questionnaire_w_values, level, reference = FALSE) {
  
  origin_points <- list(x=0,variable=NA,value=0,tooltip="",colour=NA, transparency = NA) # a dataframe row to represent (0,0) points with no tooltip or label

  if(level == "indicators") {
    scores_df <- questionnaire_w_values %>%
      mutate(variable = Indicators,
             value = Chosen_value,
             x=rep(c(seq(9,81,24),seq(99,171,24),seq(189,261,24),seq(279,351,24)),3),
             tooltip = paste0(str_match(Options,paste(Chosen_value,'\\. *([^"]+)\\"',sep=''))[,2],'\n',Comment),
             colour = Colour,
             transparency = Transparency
      )
    if(reference == FALSE){
      scores_df <- scores_df %>%
        group_by(Dimension,Target) %>%
        group_modify(~ add_row(.x,as_tibble(origin_points),.before=0)) %>%
        ungroup() %>%
        select(x, variable, value, tooltip, colour, transparency)
    }else{
      scores_df <- scores_df %>%  select(x, variable, value, low, high, tooltip, colour, transparency)
    }
  }
    
  else{
    target_scores <- summarise(group_by(questionnaire_w_values,
                                        variable_t =Target,
                                        dimension = Dimension),
                               value_t = mean(Chosen_value,na.rm=TRUE),
                               low = ifelse(reference== FALSE, NA, mean(low,na.rm=TRUE)), #review when clearer what values 
                               high = ifelse(reference== FALSE, NA, mean(high,na.rm=TRUE)), #low and high should display
                               tooltip = ifelse(reference == FALSE,
                                                paste("Target average:",value_t,"\n",paste(Indicators,"-",Chosen_value,collapse="\n")),
                                                "sample tooltip for benchmark ref dataset"),
                               colour = unique(Colour),
                               transparency = unique(Transparency)
                               )
    if(level == "targets"){
      target_scores <- target_scores %>% ungroup() %>% mutate(variable = variable_t, value = value_t, x=seq(15,345,30), transparency = 1)
      if(reference == FALSE){
        target_scores <- target_scores %>% select(x, variable, value, tooltip, colour, transparency)
        scores_df <- rbind(origin_points,target_scores[1:4,], origin_points,target_scores[5:8,], origin_points,target_scores[9:12,],make.row.names=FALSE)
      }else{
        scores_df <- target_scores %>% select(x, variable, value, low, high, tooltip, colour, transparency)
      }
    }
    else if(level == "dimensions"){
      scores_df <- summarise(group_by(target_scores,
                                      variable=dimension),
                             value = mean(value_t,na.rm=TRUE),
                             #low = ifelse(reference== FALSE, NA, mean(low,na.rm=TRUE)), #review when clearer what values 
                             #high = ifelse(reference== FALSE, NA, mean(high,na.rm=TRUE)), #low and high should display
                             tooltip = ifelse(reference == FALSE,
                                              paste(variable_t,"-",value_t,collapse="\n"),
                                              "sample tooltip for benchmark ref dataset"),
                             colour = unique(colour),
                             transparency = 1
                             )
    }
  }

  if("x" %in% colnames(scores_df)){scores_df$x <- as.numeric(scores_df$x)}
  scores_df$value <- as.numeric(scores_df$value)
  if(reference==TRUE){
    scores_df$high <- as.numeric(scores_df$high)
    scores_df$low <- as.numeric(scores_df$low)    
  }
  
  return(scores_df)
}


# Identify low and high scores --------------------------------------------

# Used in creating dynamic text for Results and Benchmarking pages.
# Input data = scoring tables

id_low_scores<-function(data,threshold_low=2){
  reactive({ifelse(length(data$variable[which(data$value<threshold_low & data$value !=0 & !is.na(data$value))])>0,
                   toString(data$variable[which(data$value<threshold_low & data$value !=0 & !is.na(data$value))]),
                   "None")})
}
id_high_scores<-function(data,threshold_high=3){
  reactive({ifelse(length(data$variable[which(data$value>threshold_high & !is.na(data$value))])>0,
                   toString(data$variable[which(data$value>threshold_high & !is.na(data$value))]),
                   "None")})
}
