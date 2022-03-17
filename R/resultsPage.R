
# Module UI function
resultsOutput <- function(id, label = "results") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    h2("Results: Visualise your OH-EpiCap profile"),
    fluidRow(
      box(width=10,
          p("This page visually summarises an interactively completed, or uploaded, OH-EpiCap profile.",
            br(),
            "This page can also be downloaded in report form by clicking the 'Download report' button to the right.",
            br(),
            "The report is in HTML format, and includes interactive visualisations.")
       ),
      downloadButton(ns("report"), "Download report",width=2)
      ),
    div(id="foo",
      fluidRow(
       box(width=12,
           title="OH-EpiCap Index and Dimensions",
           solidHeader=TRUE, status="success",
           collapsible=TRUE, collapsed=FALSE,
           p("OH-EpiCap and Dimension indices represent mean scores over all questions, expressed as a percentage."),
           #textOutput(ns("restxt_overall")),
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
            p("This section shows the results of the twelve targets, divided across the three dimensions.",
              br(),
              "Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be beneficial.",
              br(),
              "Users are encouraged to hover over data points to view a breakdown of each target score."),
            fluidRow(column(6, girafeOutput(ns("lollipop_tar"))),
                     column(6, girafeOutput(ns("radar_all")))),
            uiOutput(ns("restxt_targets")),
            )),
      fluidRow(
        box(width=12,
            title="Dimension 1: Organization",
            solidHeader=TRUE, status="warning",
            collapsible=TRUE, collapsed=TRUE,
            fluidRow(
              column(6,
                     p("This section shows the results across all indicators within the four targets of Dimension 1 (Organization)."),
                     p("Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be beneficial."),
                     p("Indicators labelled in grey indicate a question was answered with NA. Users are encouraged to hover over plotted data points to view the wording of the chosen indicator level, and any comments that may have been added in connection with a particular question."),
                     uiOutput(ns("restxt_dim1"))),
              column(6, girafeOutput(ns("radar_1"))))
            )),
      fluidRow(
        box(width=12,
            title="Dimension 2: Operations",
            solidHeader=TRUE, status="primary",
            collapsible=TRUE, collapsed=TRUE,
            fluidRow(
              column(6,
                     p("This section shows the results across all indicators within the four targets of Dimension 2 (Operations)."),
                     p("Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be beneficial."),
                     p("Indicators labelled in grey indicate a question was answered with NA. Users are encouraged to hover over plotted data points to view the wording of the chosen indicator level, and any comments that may have been added in connection with a particular question."),
                     uiOutput(ns("restxt_dim2"))),
              column(6, girafeOutput(ns("radar_2"))))
            )),
      fluidRow(
        box(width=12,
            title="Dimension 3: Impact",
            solidHeader=TRUE,
            collapsible=TRUE, collapsed=TRUE,
            fluidRow(
              column(6,
                     p("This section shows the results across all indicators within the four targets of Dimension 3 (Impact)."),
                     p("Scores range 1-4, with higher values suggesting better adherence to the One Health principle (better integration of sectors), and lower values suggesting improvements may be beneficial."),
                     p("Indicators labelled in grey indicate a question was answered with NA. Users are encouraged to hover over plotted data points to view the wording of the chosen indicator level, and any comments that may have been added in connection with a particular question."),
                     uiOutput(ns("restxt_dim3"))),
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
      OHEpiCap_index<-reactive({(mean(scores_dimensions()$value, na.rm=TRUE)-1)*100/3})
      organization_index<-reactive({(scores_dimensions()$value[1]-1)*100/3})
      operations_index<-reactive({(scores_dimensions()$value[2]-1)*100/3})
      impact_index<-reactive({(scores_dimensions()$value[3]-1)*100/3})
      g_index <- reactive({gauge(round(OHEpiCap_index(), 0), 0, 100, symbol="%", "OH-EpiCap Index", sectors = gaugeSectors(colors = "green"))})
      output$indexGauge <- renderGauge({g_index()})
      g_org <- reactive({gauge(round(organization_index(), 0), 0, 100, symbol="%", "Dimension 1: Organization", sectors = gaugeSectors(colors = "#F47931"))})
      output$organizationGauge <- renderGauge({g_org()})
      g_ope <- reactive({gauge(round(operations_index(), 0), 0, 100, symbol="%", "Dimension 2: Operations", sectors = gaugeSectors(colors = "#00679C"))})
      output$operationsGauge <- renderGauge({g_ope()})
      g_imp <- reactive({gauge(round(impact_index(), 0), 0, 100,symbol="%", "Dimension 3: Impact", sectors = gaugeSectors(colors = "#CECECE"))})
      output$impactGauge <- renderGauge({g_imp()})
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

      #lists of targets/indicators with low scores
      source("R/scoringHelpers.R")
      targets_low<-id_low_scores(scores_targets())
      targets_high<-id_high_scores(scores_targets())
      dim1_low<-id_low_scores(scores_indicators()[1:20,])
      dim1_high<-id_high_scores(scores_indicators()[1:20,])
      dim2_low<-id_low_scores(scores_indicators()[21:40,])
      dim2_high<-id_high_scores(scores_indicators()[21:40,])
      dim3_low<-id_low_scores(scores_indicators()[41:60,])
      dim3_high<-id_high_scores(scores_indicators()[41:60,])

      #generate result texts
      #restxt_overall_reactive <- reactive({"Sample text. Foo"})
      #output$restxt_overall <- renderText(restxt_overall_reactive())
      restxt_targets_reactive <- reactive({HTML(paste0("Targets demonstrating <b>good adherence</b> to One Health principles are: <b>", targets_high(), "</b>.<br>",
                                                       "Targets that would <b>most benefit from improvement</b> are: <b>",targets_low(), "</b>."
      ))})
      output$restxt_targets <- renderUI({restxt_targets_reactive()})
      restxt_dim1_reactive <- reactive({HTML(paste0("Indicators demonstrating <b>good adherence</b> to One Health principles are: <b>", dim1_high(), "</b>.<br><br>",
                                                    "Indicators that would <b>most benefit from improvement</b> are: <b>",dim1_low(), "</b>."
      ))})
      output$restxt_dim1 <- renderUI({restxt_dim1_reactive()})
      restxt_dim2_reactive <- reactive({HTML(paste0("Indicators demonstrating <b>good adherence</b> to One Health principles are: <b>", dim2_high(), "</b>.<br><br>",
                                                    "Indicators that would <b>most benefit from improvement</b> are: <b>",dim2_low(), "</b>."
      ))})
      output$restxt_dim2 <- renderUI({restxt_dim2_reactive()})
      restxt_dim3_reactive <- reactive({HTML(paste0("Indicators demonstrating <b>good adherence</b> to One Health principles are: <b>", dim3_high(), "</b>.<br><br>",
                                                    "Indicators that would <b>most benefit from improvement</b> are: <b>",dim3_low(), "</b>."
      ))})
      output$restxt_dim3 <- renderUI({restxt_dim3_reactive()})
 
      #generate automatic report , from https://shiny.rstudio.com/articles/generating-reports.html
      output$report <- downloadHandler(
        filename = "OHEpiCap-report.html",  # For PDF output, change this to .pdf
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
