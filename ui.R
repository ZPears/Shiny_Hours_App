library(shiny)
library(shinydashboard)
library(ggplot2)

hoursData <- read.csv("data/MyHours.csv")

clientList <- sort(unique(as.character(hoursData$Client)))
consultantList <- sort(unique(as.character(hoursData$Consultant)))

byClient <- aggregate(Total.Hours ~ Client, sum, data = hoursData)
byConsultant <- aggregate(Total.Hours ~ Consultant, sum, data = hoursData)

dashboardPage(
  dashboardHeader(title = "Greenough Hours Dashboard"),
  #Implement some message menus here
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName="overview", icon=icon("dashboard")),
      menuItem("By Client", tabName="byclient", icon=icon("th")),
      menuItem("By Consultant", tabName="byconsultant", icon=icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview tab content
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Alerts by Client:"
          ),
          
          box(
            title = "Alerts by Consultant:"
          )
        )
      ),
      
      # Client tab content
      tabItem(tabName = "byclient",
        
        fluidRow(
          box(
            selectInput("clientSelect", label = "Choose Client:", choices = clientList), width = 6
          ),
          
          valueBoxOutput("totalRetainerBox", width = 6)
        ),
        
        fluidRow(
          
          box(plotOutput("clientPlot", height = 500), width = 12)
          
        ),
      
        fluidRow(
          valueBoxOutput("absRetainerBox", width = 6),
          
          valueBoxOutput("percRetainerBox", width = 6)    
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
            selectInput("consultantSelect", label = "Choose Consultant:", choices = consultantList), width = 6
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