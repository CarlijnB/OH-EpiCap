
# Setup -------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(flexdashboard)


setupApp(questionnaire_file = "data/EU-EpiCap_Questionnaire_21_11_30.xlsx")

# User interface ----------------------------------------------------------

ui <- dashboardPage(
    
    dashboardHeader(
        titleWidth = "100%",
        title = fluidRow(
            column(1, tags$a(href="https://onehealthejp.eu",tags$img(src="OHEJP_logo.png", width="200"))),
            column(4), # this and/or CSS needs changing to have text right of logo, and taking up rest of column (or add more logos)
            column(7, class="title-box", # this and/or CSS needs changing to have text right of logo, and taking up rest of column (or add more logos)
                   tags$h1(class="primary-title", style="margin-top:10px;", "EU-EpiCap Tool"),
                   tags$h2(class="primary-subtitle", style="margin-top:10px", "Evaluation tool for One Health epidemiological surveillance capacities and capabilities")
            )
        )
    ),
    
# UI - sidebar ------------------------------------------------------------
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("About EU-EpiCap", tabName = "about", icon = icon("question-circle")), 
            #menuItem("Your surveillance network", tabName = "network", icon = icon("project-diagram")),
            menuItem("Questionnaire", tabName = "questionnaire", icon = icon("file-alt"),
                     #menuItem("Instructions", tabName = "instructions"),
                     menuItem("Upload answers from file", tabName = "upload"),
                     convertMenuItem(menuItem("Dimension 1: Organization", tabName = "organization",
                              menuSubItem("Target 1.1", href="#T1.1",newtab=FALSE),
                              menuSubItem("Target 1.2", href="#T1.2",newtab=FALSE),
                              menuSubItem("Target 1.3", href="#T1.3",newtab=FALSE),
                              menuSubItem("Target 1.4", href="#T1.4",newtab=FALSE)),"organization"),
                     convertMenuItem(menuItem("Dimension 2: Operations", tabName = "operations",
                              menuSubItem("Target 2.1", href="#T2.1",newtab=FALSE),
                              menuSubItem("Target 2.2", href="#T2.2",newtab=FALSE),
                              menuSubItem("Target 2.3", href="#T2.3",newtab=FALSE),
                              menuSubItem("Target 2.4", href="#T2.4",newtab=FALSE)),"operations"),
                     convertMenuItem(menuItem("Dimension 3: Impact", tabName = "impact",
                              menuSubItem("Target 3.1", href="#T3.1",newtab=FALSE),
                              menuSubItem("Target 3.2", href="#T3.2",newtab=FALSE),
                              menuSubItem("Target 3.3", href="#T3.3",newtab=FALSE),
                              menuSubItem("Target 3.4", href="#T3.4",newtab=FALSE)),"impact"),
                     menuItem("Download answers or report", tabName = "download")
            ),
            menuItem("Results", tabName = "results", icon = icon("chart-pie")
                     ),
            menuItem("Benchmark", tabName = "benchmark", icon = icon("copy")
                     )
        )
    ),
    
# UI - Header CSS ---------------------------------------------------------------
    
# From https://jonkatz2.github.io/2018/06/22/Image-In-Shinydashboard-Header - needs changing to have figure to left and text taking up rest of column

    dashboardBody(
        
        tags$style(type="text/css", "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }
/*    Format the title/subtitle text */
    .title-box {
        position: absolute;
        text-align: left;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: left;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.1em;
        }
        .primary-subtitle {
            font-size: 1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 125px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
        ),
        
# UI - tabs ---------------------------------------------------------------
        
        tabItems(
            tabItem(tabName = "about",
                    h2("About the EU-EpiCap tool"),
                    p("The MATRIX project aims to advance the implementation of One health (OH) Surveillance in practice by building onto existing resources, adding value to them and creating synergies among the sectors at the national level."),
                    p("Within work package four (WP4), a generic benchmarking tool (EU-EpiCap) is being developed for characterizing, monitoring and evaluating epidemiological surveillance capacities, which directly contribute to OHS. The tool aims to identify and describe the collaborations among actors involved in the surveillance of a hazard and to characterize the OH-ness of the surveillance system. The tool will support identification of areas that could lead to improvements in existing OH surveillance capacities.")
            ),
            #tabItem(tabName = "network",
            #        h2("Explore your surveillance network here")
            #),
            tabItem(tabName = "questionnaire",
                    h2("Complete the EU-EpiCap questionnaire here"),
            ),
            #tabItem(tabName = "instructions",
            #        h2("How to complete the EU-EpiCap tool"),
            #),
            tabItem(tabName = "upload",
                    h2("Upload answers from a previously (partially) completed questionnaire"),
                    questionnaireUploadUI("datafile", "Upload EU-EpiCap profile (.csv or .rds format)"),
            ),       
            tabItem(tabName = "organization",
                    h2("Dimension 1: Organization"),
                    box(width=12,
                        p(tags$b("Dimension 1"), "deals with different aspects related to the ", tags$b("organization of the OH surveillance system.")),
                        p("It comprises four Target areas:"),
                        tags$ul(
                          tags$li(tags$b("Target 1.1 Formalization"),"includes questions about the objectives, supporting documentation, coordination roles, and leadership in the OH surveillance system."),
                          tags$li(tags$b("Target 1.2 Coverage"), "addresses whether the surveillance covers all relevant actors, disciplines, sectors, geography, populations, and hazards."),
                          tags$li(tags$b("Target 1.3 Resources"), "addresses questions linked to the availability of financial and human resources, training, and sharing of the available operational resources."),
                          tags$li(tags$b("Target 1.4 Evaluation and resilience"), "focuses on internal and external evaluation, implementation of corrective measures, and the capacity of the OH surveillance system to adapt to changes.")
                        ),
                    ),
                    buildQuestionnaireUI(commands[commands$Dimension=='Dimension 1: Organization',])
            ),
            tabItem(tabName = "operations",
                    h2("Dimension 2: Operations"),
                    box(width=12,
                        p(tags$b("Dimension 2"), "deals with different aspects related to OH-ness in ", tags$b("operational activities of the OH surveillance system.")),
                        p("It comprises four Target areas:"),
                        tags$ul(
                        tags$li(tags$b("Target 2.1 Data collection and methods sharing"),"concerns the level of multisectoral collaboration in the design of surveillance protocols, data collection, harmonization of laboratory techniques and data warehousing."),
                        tags$li(tags$b("Target 2.2 Data sharing"), "addresses data sharing agreements, evaluation of data quality, use of shared data, and the compliance of data with the FAIR principle."),
                        tags$li(tags$b("Target 2.3 Data analysis and interpretation"), "addresses multisectoral integration for data analysis, sharing of statistical analysis techniques, sharing of scientific expertise, and harmonization of indicators."),
                        tags$li(tags$b("Target 2.4 Communication"), "focuses on both internal and external communication processes, dissemination to decision-makers, and information sharing in case of suspicion.")
                        ),
                    ),
                    buildQuestionnaireUI(commands[commands$Dimension=='Dimension 2: Operations',])
            ),
            tabItem(tabName = "impact",
                    h2("Dimension 3: Impact"),
                    box(width=12,
                        p(tags$b("Dimension 3"), "deals with the", tags$b("impact of the OH surveillance system.")),
                        p("It comprises four Target areas:"),
                        tags$ul(
                          tags$li(tags$b("Target 3.1 Technical outputs"),"concerns the timely detection of emergence, knowledge improvement on hazard epidemiological situations, increased effectiveness of surveillance, and reduction of operational costs."),
                          tags$li(tags$b("Target 3.2 Collaborative added value"), "addresses strengthening of the OH team and network, international collaboration and common strategy (road map) design."),
                          tags$li(tags$b("Target 3.3 Immediate and intermediate outcomes"), "addresses advocacy, awareness, preparedness and interventions based on the information generated by the OH surveillance system."),
                          tags$li(tags$b("Target 3.4 Ultimate outcomes"), "focuses on research opportunities, policy changes, behavioral changes and better health outcomes that are attributed to the OH surveillance system.")
                        ),
                    ),
                    buildQuestionnaireUI(commands[commands$Dimension=='Dimension 3: Impact',])
            ),
            tabItem(tabName = "download",
                    h2("Download selected questionnaire answers"),
                    questionnaireDownloadUI("downloadedAnswers", "Download EU-EpiCap profile (.csv or .rds format)")
            ),       
            tabItem(tabName = "results", resultsOutput("resultsPage")),
            tabItem(tabName = "benchmark", benchmarkUI("benchmarkPage",ref_datasets=ref_datasets))
        )    
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    #lists of input names associated with questions and comments
    question_inputs <- reactive(grep(pattern="Q[[:digit:]]", x=names(input), value=TRUE))
    comment_inputs <- reactive(grep(pattern="C[[:digit:]]", x=names(input), value=TRUE))
    
    ### tabName "download" --- download completed questionnaire as rds or csv file
    inputlist <- reactive({
        sapply(c(question_inputs(),comment_inputs()), function(x) input[[x]], USE.NAMES = TRUE)
    })
    downloadedAnswers <- questionnaireDownloadServer("downloadedAnswers", stringsAsFactors = FALSE,inputlist=inputlist)
    
    ### restoring questionnaire state
    state<-questionnaireUploadServer("datafile", stringsAsFactors = FALSE)
    observeEvent(state(),{
        sapply(question_inputs(), function(x){updateRadioButtons(session,inputId=x,selected=state()[[x]])})
        sapply(comment_inputs(), function(x){updateTextInput(session,inputId=x,value=state()[[x]])})
        })

    ### scoring the questionnaire 
    # extracting questionnaire choices, and adding them to the questionnaire dataframe
    questionnaire_w_values <- addScores2Questionnaire(input,questionnaire) #by sticking everything into one df, all wrangling + plotting code has to be rerun as soon as I change 1 value 
    # summarising scores by target for all dimensions, and by indicator for each dimension separately
    scores_dimensions<-reactive(scoringTable(questionnaire_w_values(),"dimensions"))
    scores_targets<-reactive(scoringTable(questionnaire_w_values(),"targets"))
    scores_indicators<-reactive(scoringTable(questionnaire_w_values(),"indicators"))
    
    ### creating the Results page (with gauges, plots, and associated texts)
    resultsServer("resultsPage", scores_targets=scores_targets, scores_indicators=scores_indicators, scores_dimensions=scores_dimensions,stringsAsFactors = FALSE)
    
    ### creating the Benchmarking page (with plots and associated texts)
    benchmarkServer("benchmarkPage", scores_targets=scores_targets, scores_indicators=scores_indicators, ref_files=ref_files, stringsAsFactors = FALSE)
    }

shinyApp(ui, server)
