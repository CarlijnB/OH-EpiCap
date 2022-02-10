# Module UI function
resultsOutput <- function(id, label = "results") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    girafeOutput(ns("radar_all")),
    girafeOutput(ns("radar_1")),
    girafeOutput(ns("radar_2")),
    girafeOutput(ns("radar_3"))
  )
}

resultsServer <- function(id, scores_targets=scores_targets(), scores_indicators=scores_indicators(), stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #generate radarcharts
      output$"radar_all" <- renderGirafe(makeRadarPlot_results(scores_targets,3))
      output$"radar_1" <- renderGirafe(makeRadarPlot_results(scores_indicators[1:20,],4))
      output$"radar_2" <- renderGirafe(makeRadarPlot_results(scores_indicators[21:40,],4))
      output$"radar_3" <- renderGirafe(makeRadarPlot_results(scores_indicators[41:60,],4))
    }
  )    
}
