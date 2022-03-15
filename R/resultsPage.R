
# Module UI function
resultsOutput <- function(id, label = "results") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    h2("Visualise your EU-EpiCap profile"),
    fluidRow(
      box(width=10,
          p("This page visually summarises the interactively completed, or uploaded, EU-EpiCap profile.")
       ),
      downloadButton(ns("report"), "Download report",width=2)
      ),
    div(id="foo",
      fluidRow(
       box(width=12,
           title="EU-EpiCap Index and Dimensions",
           solidHeader=TRUE, status="success",
           collapsible=TRUE, collapsed=FALSE,
           p("EU-EpiCap and Dimension indices represent mean scores over all component questions, expressed as percentages."),
           textOutput(ns("restxt_overall")),
           fluidRow(column(4),
                    gaugeOutput(ns("indexGauge")),
                    column(4)),
           fluidRow(column(4,gaugeOutput(ns("organizationGauge"))),
                    column(4,gaugeOutput(ns("operationsGauge"))),
                    column(4,gaugeOutput(ns("impactGauge"))))
           )),
      fluidRow(
        box(width=12,
            title="Targets",
            solidHeader=TRUE, status="info",
            collapsible=TRUE, collapsed=FALSE,
            p("Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be required. Users are encouraged to hover over plotted data points to view breakdowns of scores."),
            textOutput(ns("restxt_targets")),
            fluidRow(column(6, girafeOutput(ns("lollipop_tar"))),
                     column(6, girafeOutput(ns("radar_all"))))
            )),
      fluidRow(
        box(width=12,
            title="Dimension 1: Organization",
            solidHeader=TRUE, status="warning",
            collapsible=TRUE, collapsed=TRUE,
            fluidRow(
              column(6, 
                     p("Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be required. Greyed-out Indicators labels indicate a question was answered with NA. Users are encouraged to hover over plotted data points to view the wording of the chosen indicator level, and any comments that may have been added in connection with particular questions."),
                     textOutput(ns("restxt_dim1"))),
              column(6, girafeOutput(ns("radar_1"))))
            )),
      fluidRow(
        box(width=12,
            title="Dimension 2: Operations",
            solidHeader=TRUE, status="primary",
            collapsible=TRUE, collapsed=TRUE,
            fluidRow(
              column(6, 
                     p("Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be required. Greyed-out Indicators labels indicate a question was answered with NA. Users are encouraged to hover over plotted data points to view the wording of the chosen indicator level, and any comments that may have been added in connection with particular questions."),
                     textOutput(ns("restxt_dim2"))),
              column(6, girafeOutput(ns("radar_2"))))
            )),
      fluidRow(
        box(width=12,
            title="Dimension 3: Impact",
            solidHeader=TRUE,
            collapsible=TRUE, collapsed=TRUE,
            fluidRow(
              column(6,
                     p("Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be required. Greyed-out Indicators labels indicate a question was answered with NA. Users are encouraged to hover over plotted data points to view the wording of the chosen indicator level, and any comments that may have been added in connection with particular questions."),
                     textOutput(ns("restxt_dim3"))),
              column(6, girafeOutput(ns("radar_3"))))
            ))
    )
  )
}

resultsServer <- function(id, scores_targets=scores_targets, scores_indicators=scores_indicators, scores_dimensions=scores_dimensions, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #gauges
      EUEpiCap_index<-reactive({(mean(scores_dimensions()$value, na.rm=TRUE)-1)*100/3})
      organization_index<-reactive({(scores_dimensions()$value[1]-1)*100/3})
      operations_index<-reactive({(scores_dimensions()$value[2]-1)*100/3})
      impact_index<-reactive({(scores_dimensions()$value[3]-1)*100/3})
      output$indexGauge <- renderGauge({gauge(round(EUEpiCap_index(), 0), 0, 100, symbol="%", "EU-EpiCap Index", sectors = gaugeSectors(colors = "green"))})
      output$organizationGauge <- renderGauge({gauge(round(organization_index(), 0), 0, 100, symbol="%", "Dimension 1: Organization", sectors = gaugeSectors(colors = "#F47931"))})
      output$operationsGauge <- renderGauge({gauge(round(operations_index(), 0), 0, 100, symbol="%", "Dimension 2: Operations", sectors = gaugeSectors(colors = "#00679C"))})
      output$impactGauge <- renderGauge({gauge(round(impact_index(), 0), 0, 100,symbol="%", "Dimension 3: Impact", sectors = gaugeSectors(colors = "#CECECE"))})
      #generate plots
      #output$lollipop_dim <- renderGirafe(makeLollipopPlot(scores_dimensions(),"Dimensions"))
      lollipopreactive_tar <- reactive({makeLollipopPlot(scores_targets(),"Targets")})
      output$lollipop_tar <- renderGirafe(lollipopreactive_tar())
      #output$lollipop_1 <- renderGirafe(makeLollipopPlot(scores_indicators()[1:20,],"Indicators")) #Dimension-specific lollipop plots need labels based on target + short indicator name
      radreactive_all <- reactive({makeRadarPlot_results(scores_targets(),3)})
      output$radar_all <- renderGirafe(radreactive_all())
      radreactive_1 <- reactive({makeRadarPlot_results(scores_indicators()[1:20,],4)})
      output$radar_1 <- renderGirafe(radreactive_1())
      radreactive_2 <- reactive({makeRadarPlot_results(scores_indicators()[21:40,],4)})
      output$radar_2 <- renderGirafe(radreactive_2())
      radreactive_3 <- reactive({makeRadarPlot_results(scores_indicators()[41:60,],4)})
      output$radar_3 <- renderGirafe(radreactive_3())

      #generate result texts
      output$restxt_overall <- renderText(restxt_overall_reactive())
      restxt_overall_reactive <- reactive({"Sample text. Foo"})
      output$restxt_targets <- renderText({"Sample text. Best Dim score is"})
      output$restxt_dim1 <- renderText({"Sample text. Best Target score is "})
      output$restxt_dim2 <- renderText({"Sample text. Best Target score is "})
      output$restxt_dim3 <- renderText({"Sample text. Best Target score is "})
 
      #generate automatic report , from https://shiny.rstudio.com/articles/generating-reports.html
      output$report <- downloadHandler(
        filename = "EUEpiCap-report.html",  # For PDF output, change this to .pdf
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.rmd")
          file.copy("data/report-template.rmd", tempReport, overwrite = TRUE)
          # Knit the document, and eval it in the current environment
          rmarkdown::render(tempReport, output_file = file)
        }
      )
    }
  )    
}
