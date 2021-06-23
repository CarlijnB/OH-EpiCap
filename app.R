
# Setup -------------------------------------------------------------------
library(shiny)
library(shinydashboard)

# User interface ----------------------------------------------------------

ui <- dashboardPage(
    
    dashboardHeader(title = "EU-EpiCap"),

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

# UI - body ---------------------------------------------------------------

    dashboardBody(
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
    
}

shinyApp(ui, server)
