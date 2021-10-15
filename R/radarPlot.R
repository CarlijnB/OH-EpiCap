#This script creates a radarplot with a customisable tooltip. 
#Script (rewrite of ggRadar) mostly by Laurens - don't forget to acknowledge! 
# Check rewrite from where? might need to add ref/acknowledgement


library(tidyverse)
library(ggiraph)
library(ggmulti)

#Create the shapes for background colouring, from origin to outer edge of the plot, for x partitions
createBgpolygons <- function(n_partitions,max_score){
  degrees_per_partition <- 360/(n_partitions+1) #90 for 3; 72 for 4
  degrees_break <- degrees_per_partition/n_partitions #in between partitions
  degrees_start <- c(seq(degrees_break/2,360,360/n_partitions)) #start partitions so that 0 falls in middle between them
  return(
    map(degrees_start,function(v){
      tibble(
        x=c(v,seq(v,v+degrees_per_partition),v+degrees_per_partition),
        value=c(0,rep(max_score,1+degrees_per_partition),0))
    }))
}

makeRadarPlot <- function(scoring_table, n_partitions){
  
  if(n_partitions==3){
    max_score <- 16}
  else {max_score <- 4}
  
  bgPolygons<-createBgpolygons(n_partitions,max_score)
  
  p <- ggplot(
    data = scoring_table,
    mapping = aes(x = x, y = value, group = 1)
  ) +
    geom_polygon(data=bgPolygons[[1]],linetype=0,fill="#F47931",alpha=0.2)+
    geom_polygon(data=bgPolygons[[2]],linetype=0,fill="#00679C",alpha=0.2)+
    geom_polygon(data=bgPolygons[[3]],linetype=0,fill="#CECECE",alpha=0.2)+
    geom_polygon_interactive(data=scoring_table[1:5,],colour = "#F47931", fill="#F47931", alpha = 0.7) +
    geom_polygon_interactive(data=scoring_table[6:10,],colour = "#00679C", fill="#00679C", alpha = 0.7) + 
    geom_polygon_interactive(data=scoring_table[11:15,],colour = "#CECECE", fill="#CECECE", alpha = 0.7) +
    geom_point_interactive(
      data=scoring_table[2:5,],
      mapping = aes(tooltip = tooltip),
      colour = "#F47931",
      size = 3
    ) +
    geom_point_interactive(
      data=scoring_table[7:10,],
      mapping = aes(tooltip = tooltip),
      colour = "#00679C",
      size = 3
    ) +
    geom_point_interactive(
      data=scoring_table[12:15,],
      mapping = aes(tooltip = tooltip),
      colour = "#CECECE",
      size = 3
    ) +
    
    xlab("") +
    ylab("") +
    ggmulti::coord_radial(clip = "off") +     # Was coord_radar. Comment this line out to get "original" x-y plot, to better understand underlying code 
    expand_limits(x=c(0,360),y = c(0, max_score)) +
    theme_bw() +        # Use ggplot theme_bw to remove greys
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      #panel.background = element_rect(fill="lightblue"),
      #plot.background = element_rect(fill="darkseagreen") #to make margins visible; REMOVE!
    ) +
    scale_y_continuous(breaks = seq(0, max_score, by=2),limits=c(0,max_score),expand=c(0,0)) +
    scale_x_continuous(breaks = seq(15, 355, by=30),
                       minor_breaks = NULL,
                       labels=str_wrap(c(scoring_table$variable[2:5],scoring_table$variable[7:10],scoring_table$variable[12:15]),width=15)) +
    annotate(
      "text",
      x = 0,
      y = seq(0, max_score, 2),
      label = seq(0, max_score, 2),
      size = 3,
      colour = "grey"
    )
    
    if(n_partitions ==4){
      p <- p +
        geom_polygon(data=bgPolygons[[4]],linetype=0,fill="#63913E",alpha=0.2) +
        geom_polygon_interactive(data=scoring_table[16:20,],colour = "#63913E", fill="#63913E", alpha = 0.7) +
        geom_point_interactive(
          data=scoring_table[17:20,],
          mapping = aes(tooltip = tooltip),
          colour = "#63913E",
          size = 3
        ) +
        scale_y_continuous(breaks = seq(0, max_score, by=1),limits=c(0,max_score),expand=c(0,0)) +
        scale_x_continuous(breaks = c(seq(9,81,by=24),seq(99,171,by=24),seq(189,261,by=24),seq(279,351,by=24)),
                           minor_breaks = NULL,
                           labels=str_wrap(c(scoring_table$variable[2:5],
                                             scoring_table$variable[7:10],
                                             scoring_table$variable[12:15],
                                             scoring_table$variable[17:20]),width=15))+
        annotate(
          "text",
          x = 0,
          y = seq(0, max_score, 1),
          label = seq(0, max_score, 1),
          size = 3,
          colour = "grey"
        )
    }

  interactive <- FALSE
  if (interactive) {
    tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
    hover_css = "r:4px;cursor:pointer;stroke-width:6px;"
    selected_css = "fill:#FF3333;stroke:black;"
    p <- girafe(ggobj = p)
    p <- girafe_options(
      p,
      opts_hover(css = hover_css),
      opts_tooltip(css = tooltip_css, opacity = 0.75),
      opts_selection(css = selected_css),
      opts_zoom(min = 1, max = 10)
    )
  }
  
  ggiraph(code = print(p))

}