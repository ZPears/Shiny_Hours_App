library(shiny)
library(shinydashboard)
library(ggplot2)
library(openxlsx)
library(lubridate)

dashboardPage(
  dashboardHeader(title = "Greenough Hours Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName="overview", icon=icon("dashboard")),
      menuItem("By Client", tabName="byclient", icon=icon("th")),
      menuItem("By Consultant", tabName="byconsultant", icon=icon("th"))
    ),
    br(),
    dateRangeInput('dateRange', label = 'Date Range', start = floor_date(Sys.Date(), unit="month"), end = Sys.Date(),  format = "mm/dd/yy"),
    br(), br(),
    fileInput("hoursfile", "Upload Hours:", accept = c("text/csv", ".csv")),
    fileInput("projectionfile", "Upload Projections:", accept = c("application/vnd.ms-excel", '.xlsx')),
    br(), br(),
    downloadButton("downloadData", "Download Spreadsheet")
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      # Overview tab content
      tabItem(tabName = "overview",
              
        fluidRow(
          box(
            title = "Alerts by Client:",
            htmlOutput("clientAlerts")
          ),
          
          box(
            title = "Alerts by Consultant:",
            htmlOutput("consultantAlerts")
          )
        )
        
        #fluidRow(
        #  box(
        #    textOutput("clientAlerts")
        #  )
        #)
        
      ),
      
      # Client tab content
      tabItem(tabName = "byclient",
        
        fluidRow(
          box(
            uiOutput("clientSelector"), width = 6
          ),
          
          valueBoxOutput("totalRetainerBox", width = 6)
        ),
        
        fluidRow(
          
          box(plotOutput("clientPlot", height = 500), width = 12)
          
        ),
      
        fluidRow(
          valueBoxOutput("absRetainerBox", width = 6),
          
          valueBoxOutput("retainerSpentBox", width = 6)    
        ),
        
        fluidRow(
          valueBoxOutput("overserviceAbs", width = 6),
          
          valueBoxOutput("overservicePerc", width = 6)
        )
        
      ),
      
      # Consultant tab content
      tabItem(tabName = "byconsultant",
              
        fluidRow(
          box(
            uiOutput("consultantSelector"), width = 6
          ),
          
          valueBoxOutput("totalHoursBox", width = 6)
        ),
        fluidRow(
          
          box(plotOutput("consultantPlot", height = 500), width = 12)
          
        ),
                
        fluidRow(
          
          valueBoxOutput("billableGoalBox"),
          
          valueBoxOutput("assignedClientHoursBox"),
          
          valueBoxOutput("billabilityBox")
          
        ),
        
        fluidRow(
          
          valueBoxOutput("availabilityBox"),
          
          valueBoxOutput("hoursBilledBox"),
          
          valueBoxOutput("utilizationBox")
          
        )
      )
      
    )
  )
)