library(shinydashboard)

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
          
          box(plotOutput("plot1", height = 250)),
          
          box(
            selectInput("clientSelect", label = "Choose Client:", choices = clientList)
          )
      ),
      
        fluidRow(
            valueBoxOutput("percRetainerBox")    
          )
      ),
      
      # Consultant tab content
      tabItem(tabName = "byconsultant",
        h2("Consultant tab content")
      )
    )
  )
)