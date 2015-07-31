source("helpers.R")

shinyServer(function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  #plot outputs
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot3 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  #client dashboard valueboxes

  output$absRetainerBox <- renderValueBox({
    valueBox(
      paste0("10"), "Hours Used", icon = icon("calendar"), color = "green"
    )
  })

  output$percRetainerBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Retainer Used", icon = icon("dollar"), color = "blue"
    )
  })
  
  #ACTUAL RETAINER
  
  #OVERSERVICE PERCENT
  
  #OVERSERVICE AMOUNT
    
  #consultant dashboard valueboxes
  
  output$billableGoalBox <- renderValueBox({
    valueBox(
      paste0("25"), "Billable Goal", icon = icon("line-chart"), color = "blue"
    )
  })
  
  output$assignedClientHoursBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Assigned Client Hours", icon = icon("calendar"), color = "green"
    )
  })
  
  output$billabilityBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Billability", icon = icon("dollar"), color = "blue"
    )
  })
  
  output$availabilityBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Availability", icon = icon("calendar"), color = "green"
    )
  })
  
  output$hoursBilledBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Hours Billed", icon = icon("line-chart"), color = "blue"
    )
  })
  
  output$utilizationBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Utilization", icon = icon("calendar"), color = "green"
    )
  })
  
})
