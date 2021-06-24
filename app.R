
# Setup -------------------------------------------------------------------
library(shiny)
library(shinydashboard)

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
                     menuSubItem("Instructions", tabName = "instructions"),
                     menuSubItem("Upload completed questionnaire", tabName = "upload"),
                     menuSubItem("Dimension 1: Organization", tabName = "organization"),
                     menuSubItem("Dimension 2: Operations", tabName = "operations"),
                     menuSubItem("Dimension 3: Impact", tabName = "impact")),
            menuItem("Results", tabName = "results", icon = icon("chart-pie")),
            menuItem("Comparison", tabName = "comparison", icon = icon("copy"))
        )
    ),
    
# UI - Header CSS ---------------------------------------------------------------
    
#from https://jonkatz2.github.io/2018/06/22/Image-In-Shinydashboard-Header - needs changing to have figure to left and text taking up rest of column
#also for changing colours of elements, see https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
    
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
            ),
            tabItem(tabName = "operations",
                    h2("Dimension 2: Operations"),
            ),
            tabItem(tabName = "impact",
                    h2("Dimension 3: Impact"),
            ),
            tabItem(tabName = "results",
                    h2("Visualise your EU-EpiCap profile here")
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
}

shinyApp(ui, server)
