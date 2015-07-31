library(shiny)
library(shinydashboard)

hoursData <- read.csv("data/MyHours.csv")

clientList <- unique(as.character(hoursData$Client))
consultantList <- unique(as.character(hoursData$Consultant))

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
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        )
      ),
      
      # Client tab content
      tabItem(tabName = "byclient",
        fluidRow(
          
          box(plotOutput("plot2", height = 250)),
          
          box(
            selectInput("consultantSelect", label = "Choose Client:", choices = clientList)
          )
      ),
      
        fluidRow(
            valueBoxOutput("absRetainerBox"),
          
            valueBoxOutput("percRetainerBox")    
          )
      ),
      
      # Consultant tab content
      tabItem(tabName = "byconsultant",
        fluidRow(
          
          box(plotOutput("plot3", height = 250)),
          
          box(
            selectInput("consultantSelect", label = "Choose Consultant:", choices = consultantList)
          )
        ),
        
        #NEW FLUID ROW FOR OVERSERVICE
        
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