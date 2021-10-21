
# Setup -------------------------------------------------------------------
library(shiny)
library(shinydashboard)

# Data - EU-EpiCap questionnaire  -----------------------------------------

#Read in questionnaire and turn into df
questionnaire <- readQuestionnaire("data/Questionnaire_db_test.xlsx")
#Turn questionnaire df into commands that will build questionnaire pages in App
commands <- questionnaire2Commands(questionnaire) 


#function to show content for menuItem when menuSubItems exist (from https://newbedev.com/show-content-for-menuitem-when-menusubitems-exist-in-shiny-dashboard)
convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
        mi$attribs$class=NULL
    }
    mi
}


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
            menuItem("Your surveillance network", tabName = "network", icon = icon("project-diagram")),
            menuItem("Questionnaire", tabName = "questionnaire", icon = icon("file-alt"),
                     menuItem("Instructions", tabName = "instructions"),
                     menuItem("Upload completed questionnaire", tabName = "upload"),
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
                              menuSubItem("Target 3.4", href="#T3.4",newtab=FALSE)),"impact")),
            menuItem("Results", tabName = "results", icon = icon("chart-pie")),
            menuItem("Comparison", tabName = "comparison", icon = icon("copy"))
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
                    p("Blabla")
            ),
            tabItem(tabName = "network",
                    h2("Explore your surveillance network here")
            ),
            tabItem(tabName = "questionnaire",
                    h2("Complete the EU-EpiCap questionnaire here"),
            ),
            tabItem(tabName = "instructions",
                    h2("How to complete the EU-EpiCap tool"),
            ),
            tabItem(tabName = "upload",
                    h2("Upload a previously completed questionnaire"),
                    csvFileUI("datafile", "Upload EU-EpiCap profile (.csv format)"),
                    dataTableOutput("table")
            ),       
            tabItem(tabName = "organization",
                    h2("Dimension 1: Organization"),
                    buildQuestionnaireUI(commands)
            ),
            tabItem(tabName = "operations",
                    h2("Dimension 2: Operations"),
            ),
            tabItem(tabName = "impact",
                    h2("Dimension 3: Impact"),
            ),
            tabItem(tabName = "results",
                    h2("Visualise your EU-EpiCap profile here"),
                    girafeOutput("radar_all"),
                    girafeOutput("radar_1")
            ),
            tabItem(tabName = "comparison",
                    h2("Compare multiple EU-EpiCap profiles here")
            )
        )    
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    ### tabName "upload" --- upload completed questionnaire as csv file, display as table
    
    datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
    output$table <- renderDataTable(datafile())
    
    # Don't display as table (too many fields) - but update all questionnaire inputs to values from file.
    # See https://www.r-bloggers.com/2020/12/bookmarking-a-shiny-app-without-shiny-bookmarking/
    # See https://mastering-shiny.org/action-dynamic.html?q=update#updating-inputs
   
    ### scoring the questionnaire & plotting
    # extracting questionnaire choices, and adding them to the questionnaire dataframe
    questionnaire_w_values <- addScores2Questionnaire(input,questionnaire)  #N.B. Chosen_value contains characters!
    questionnaire_w_values_3<-reactive(rbind(questionnaire_w_values(),dupl_questionnaire(questionnaire_w_values(),2),dupl_questionnaire(questionnaire_w_values(),3)))
    # summarising scores for all dimensions and for specific dimensions
    scores_alldimensions<-reactive(scoringTable(questionnaire_w_values_3(),"all"))
    scores_dim1<-reactive(scoringTable(questionnaire_w_values_3(),"1"))
    scores_dim2<-reactive(scoringTable(questionnaire_w_values_3(),"2"))
    scores_dim3<-reactive(scoringTable(questionnaire_w_values_3(),"3"))
    output$"radar_all" <- renderGirafe(makeRadarPlot(scores_alldimensions(),3))
    output$"radar_1" <- renderGirafe(makeRadarPlot(scores_dim1(),4))
    
}

shinyApp(ui, server)
