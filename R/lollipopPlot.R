library(tidyverse)
library(ggiraph)

#Function setting up a generic plot for a lollipop plot (in global env)
#Run only once at app setup
setupLollipopPlot<-function(max_score=4){
  lp <<- ggplot() +
    theme_light() +
    coord_flip() +
    theme(
      #panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab("EU-EpiCap score") +
    scale_y_continuous(breaks=seq(1,max_score,1),minor_breaks=NULL,limits=c(1,max_score))
}

#Main function making a lollipop plot from a scoring table
makeLollipopPlot<-function(scoring_table, level){
  #rearrange data: order by score
  mutated_data <- scoring_table %>% arrange(value) %>% mutate(variable=factor(variable,levels=variable)) %>% drop_na()
  #add data to lollipop plot
  lp <- lp + #accesses rp from global env, but returns updated value within main function env
    geom_segment(data=mutated_data, aes(x=variable, xend=variable, y=1, yend=value), color=mutated_data$colour, alpha=mutated_data$transparency) +
    geom_point_interactive(data=mutated_data, mapping = aes(x=variable, y=value, tooltip = tooltip), color=mutated_data$colour, alpha=mutated_data$transparency, size=4) +
    #xlab(level)
    xlab(NULL)
  if(level == "Dimensions"){girafe(code = print(lp),height_svg = 2.5)}else{girafe(code = print(lp))}
}




    
    
    