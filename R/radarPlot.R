#This script creates a radarplot with a customisable tooltip. 
#Script (rewrite of ggRadar) mostly by Laurens - don't forget to acknowledge! 
# Check rewrite from where? might need to add ref/acknowledgement

#Script slow due to rewrite of radar_coord. does it make a difference if this is called elsewhere?

library(tidyverse)
library(ggiraph)
library(ggmulti)

#to test script for 4 partitions, generate dummy scoring table as follows
#scores<-data.frame(x=c(seq(9,81,24),seq(99,171,24),seq(189,261,24),seq(279,351,24)), variable="foo",value=sample(1:4,16,replace=TRUE),tooltip="bar")
#origin_points <- list(x=0,variable=NA,value=0,tooltip="")
#scoring_table <- rbind(origin_points,scores[1:4,],origin_points,scores[5:8,],origin_points,scores[9:12,],origin_points,scores[13:16,],make.row.names=FALSE)  
#scoring_table[2,'value']<-NA

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

#Rewrite of radar_coord to remove the extra outer circle
#from: https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co
coord_radar <- function (theta = "x", start = 0, direction = 1){
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}

#Main function, to create a radar chart from a scoring table
makeRadarPlot <- function(scoring_table, n_partitions){
  
  max_score <- 4
  
  bgPolygons<-createBgpolygons(n_partitions,max_score)
  
  p <- ggplot(
    data = scoring_table,
    mapping = aes(x = x, y = value, group = 1)
  ) +
    geom_polygon(data=bgPolygons[[1]],linetype=0,fill="#F47931",alpha=0.2)+
    geom_polygon(data=bgPolygons[[2]],linetype=0,fill="#00679C",alpha=0.2)+
    geom_polygon(data=bgPolygons[[3]],linetype=0,fill="#CECECE",alpha=0.2)+
    geom_polygon_interactive(data=scoring_table[1:5,][!is.na(scoring_table$value[1:5]),],colour = "#F47931", fill="#F47931", alpha = 0.7) +
    geom_polygon_interactive(data=scoring_table[6:10,][!is.na(scoring_table$value[6:10]),],colour = "#00679C", fill="#00679C", alpha = 0.7) + 
    geom_polygon_interactive(data=scoring_table[11:15,][!is.na(scoring_table$value[11:15]),],colour = "#CECECE", fill="#CECECE", alpha = 0.7) +
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
    coord_radar()+
    expand_limits(x=c(0,360),y = c(0, max_score)) +
    theme_bw() +        # Use ggplot theme_bw to remove greys
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(), #gets rid of the vertical lines of the grid
      #panel.background = element_rect(fill="lightblue"),  #to make panel visible; keep commented-out unless testing
      #plot.background = element_rect(fill="darkseagreen") #to make margins visible; keep commented-out unless testing
    ) +
    scale_y_continuous(breaks = seq(0, max_score, by=1), limits=c(0,max_score+1),
                       expand=expansion(add=c(0,0))
                       ) +
    scale_x_continuous(breaks = seq(15, 355, by=30),
                       minor_breaks = NULL,
                       labels = NULL
                       ) +
    annotate( #these are the axis labels (levels 1-4) in grey
      "text",
      x = 0,
      y = seq(1, max_score, 1),
      label = seq(1, max_score, 1),
      size = 3,
      colour = "grey"
    )+
    annotate( #these are the target/indicator labels in black, when not NA
      "text",
      x = scoring_table$x[(!is.na(scoring_table$value) & scoring_table$value !=0)],
      y = 5,
      label = str_wrap(scoring_table$variable[(!is.na(scoring_table$value) & scoring_table$value !=0)], width=15),
      size = 3,
      colour = "black"
    )+
    annotate( #these are the indicator labels in grey, for NA values
      "text",
      x = scoring_table$x[is.na(scoring_table$value)],
      y = 5,
      label = str_wrap(scoring_table$variable[is.na(scoring_table$value)], width=15),
      size = 3,
      colour = "grey"
    )
    
    if(n_partitions ==4){
      p <- p +
        geom_polygon(data=bgPolygons[[4]],linetype=0,fill="#63913E",alpha=0.2) +
        geom_polygon_interactive(data=scoring_table[16:20,][!is.na(scoring_table$value[16:20]),],colour = "#63913E", fill="#63913E", alpha = 0.7) +
        geom_point_interactive(
          data=scoring_table[17:20,],
          mapping = aes(tooltip = tooltip),
          colour = "#63913E",
          size = 3
        ) +
        scale_x_continuous(breaks = c(seq(9,81,by=24),seq(99,171,by=24),seq(189,261,by=24),seq(279,351,by=24)),
                           minor_breaks = NULL,
                           labels = NULL
                           )
    }

  girafe(code = print(p))
  
}