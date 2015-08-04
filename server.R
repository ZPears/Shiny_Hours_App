source("helpers.R")

shinyServer(function(input, output) {
  
  clientDataInput <- reactive({
    clientData <- hoursData[hoursData$Client == input$clientSelect, ]
    clientData <- clientData[order(clientData$Project),]
    clientData
  })
  
  consultantDataInput <- reactive({
    consultantData <- hoursData[hoursData$Consultant == input$consultantSelect, ]
    consultantData <- consultantData[order(consultantData$Project),]
    consultantData
  })
  
  #plot outputs  
  output$clientPlot <- renderPlot({
    clientPlot <- ggplot(clientDataInput(), aes(x = Consultant, y = Total.Hours, fill = factor(Project))) + 
      geom_bar(stat = "identity") +
      guides(fill=guide_legend(title="Project")) +
      ylab("Total Hours")
    clientPlot
  })
  
  output$consultantPlot <- renderPlot({
    consultantPlot <- ggplot(consultantDataInput(), aes(x = Client, y = Total.Hours, fill = factor(Project))) + 
      geom_bar(stat = "identity") +
      guides(fill=guide_legend(title="Project")) +
      ylab("Total Hours")
    consultantPlot
  })
  
  #client dashboard valueboxes

  output$totalRetainerBox <- renderValueBox({
    valueBox(
      "$12,500", "Total Retainer", icon = icon("dollar"), color = "blue"
    )
  })
  
  output$absRetainerBox <- renderValueBox({
    valueBox(
      sum(clientDataInput()$Total.Hours), "Hours Used", icon = icon("calendar"), color = "green"
    )
  })

  output$percRetainerBox <- renderValueBox({
    valueBox(
      "25%", "Retainer Used", icon = icon("dollar"), color = "blue"
    )
  })
  
  output$overserviceAbs <- renderValueBox({
    valueBox(
      "-60", "Overservice Hours", icon = icon("calendar"), color = "blue"
    )
  })
  
  output$overservicePerc <- renderValueBox({
    valueBox(
      "-70%", "Overservice Percentage", icon = icon("dollar"), color = "green"
    )
  })
    
  #consultant dashboard valueboxes
  
  output$totalHoursBox <- renderValueBox({
    valueBox(
      sum(consultantDataInput()$Total.Hours), "Total Hours", icon = icon("calendar"), color = "blue"
    )
  })
  
  output$billableGoalBox <- renderValueBox({
    valueBox(
      "100", "Billable Goal", icon = icon("line-chart"), color = "blue"
    )
  })
  
  output$assignedClientHoursBox <- renderValueBox({
    valueBox(
      "100", "Assigned Client Hours", icon = icon("calendar"), color = "green"
    )
  })
  
  output$billabilityBox <- renderValueBox({
    valueBox(
      "90", "Billability", icon = icon("dollar"), color = "blue"
    )
  })
  
  output$availabilityBox <- renderValueBox({
    valueBox(
      "10", "Availability", icon = icon("calendar"), color = "green"
    )
  })
  
  output$hoursBilledBox <- renderValueBox({
    valueBox(
      sum(subset(consultantDataInput(), Client != "Agency")$Total.Hours),
      "Hours Billed", icon = icon("line-chart"), color = "blue"
    )
  })
  
  output$utilizationBox <- renderValueBox({
    valueBox(
      paste0("25%"), "Utilization", icon = icon("calendar"), color = "green"
    )
  })
  
})
