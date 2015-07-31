source("helpers.R")

hoursData <- read.csv("data/MyHours.csv")

byClient <- aggregate(Total.Hours ~ Client, sum, data = hoursData)
byConsultant <- aggregate(Total.Hours ~ Consultant, sum, data = hoursData)

clientList <- unique(data$Client)

shinyServer(function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$percRetainerBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Retainer Used", icon = icon("credit-card"), color = "blue"
    )
  })
  
})
