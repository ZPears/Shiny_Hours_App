library(shiny)
library(shinydashboard)
library(ggplot2)
library(openxlsx)

source("helpers.R")

shinyServer(function(input, output) {
  
  #reactive objects
  hoursData <- reactive({
    hrsfile <- input$hoursfile
    if (is.null(hrsfile)) return(NULL)
    read.csv(hrsfile$datapath)
  })
  
  projData <- reactive({
    projfile <- input$projectionfile
    if (is.null(projfile)) return(NULL)
    projData <- read.xlsx(projfile$datapath, skipEmptyRows = FALSE)
    names(projData) <- gsub("\\.", " ", names(projData))
    projData
  })
  
  clientDataInput <- reactive({
    data <- hoursData()
    clientData <- data[data$Client == input$clientSelector, ]
    clientData <- clientData[order(clientData$Project),]
    clientData
  })
  
  retainers <- reactive({
    data <- projData()
    retainers <- data[!is.na(data$STAFF) & data$STAFF == "Actual retainer",][4:(which(colnames(data)=="billable goal")-1)]
    retainers
  })
  
  consultantDataInput <- reactive({
    data <- hoursData()
    consultantData <- data[data$Consultant == input$consultantSelector, ]
    consultantData <- consultantData[order(consultantData$Project),]
    consultantData
  })
  
  finalFile <- reactive({
    projData <- projData()
    if (is.null(projData)) return(NULL)
    hoursData <- hoursData()
    if (is.null(hoursData)) return(NULL)
    finalFile <- buildFinalData(projData, hoursData)
    finalFile
  })
  
  #download handler
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("Actuals", ".xlsx", sep="")
    },
    
    content <- function(file) {
      write.xlsx(finalFile(), file)
    }
  )
  
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
  
  #selectors
  output$clientSelector <- renderUI({
    data <- hoursData()
    if (is.null(data)) return(NULL)
    
    selectInput("clientSelector", label = "Choose Client:", 
                sort(unique(as.character(data$Client)))
                )
  })
  
  output$consultantSelector <- renderUI({
    data <- hoursData()
    if (is.null(data)) return(NULL)
    
    selectInput("consultantSelector", label = "Choose Consultant:", 
                sort(unique(as.character(data$Consultant)))
    )
  })
  
  output$mytable <- renderDataTable({
    finalFile()
  })
  
  #client dashboard valueboxes
  
  output$totalRetainerBox <- renderUI({
    retainers <- retainers()
    if (input$clientSelector == "Agency") {
      ans <- "NA"
    } else {
    ans <- paste0("$", as.character(retainers[,names(retainers)==input$clientSelector]))
    }
    
    valueBox(
      ans, "Total Retainer", icon = icon("dollar"), color = "blue", width=12
    )
  })
  
  output$absRetainerBox <- renderUI({
    data <- finalFile()
    if (input$clientSelector == "Agency") {
      ans <- sum(subset(hoursData(), Client=="Agency")$Total.Hours)
    } else {
      ans <- data[!is.na(data$STAFF) & data$STAFF == "Total hours plan", names(data)==input$clientSelector]
    }
    
    valueBox(
      ans, "Hours Used", icon = icon("calendar"), color = "green", width = 14
    )
  })

  output$retainerSpentBox <- renderUI({
    data <- finalFile()
    if (input$clientSelector == "Agency") {
      ans <- "NA"
    } else {
      ans <- data[!is.na(data$STAFF) & data$STAFF == "Total retainer used", names(data)==input$clientSelector]
      ans <- paste0("$", ans)
    }
    
    valueBox(
      ans, "Retainer Used", icon = icon("dollar"), color = "blue", width = 14
    )
  })
  
  output$overserviceAbs <- renderValueBox({
    data <- finalFile()
    projData <- projData()
    actual <- as.numeric(data[!is.na(data$STAFF) & data$STAFF == "Total hours plan", names(data)==input$clientSelector])
    print(actual)
    expected <- projData[!is.na(projData$STAFF) & projData$STAFF == "Total hours plan", names(projData)==input$clientSelector]
    print(expected)
    if (input$clientSelector == "Agency") {
      ans <- "NA"
    } else {
      if (expected - actual > 0) {
        ans <- 0
      } else {
        ans <- actual - expected
      }
    }
    
    valueBox(
      ans, "Overservice Hours", icon = icon("calendar"), color = "blue", width = 14
    )
  })
  
  output$overservicePerc <- renderValueBox({
    data <- finalFile()
    if (input$clientSelector == "Agency") {
      ans <- "NA"
    } else {
      ans <- data[!is.na(data$STAFF) & data$STAFF == "SERVICE %", names(data)==input$clientSelector]
    }
    
    valueBox(
      ans, "Service Percentage", icon = icon("dollar"), color = "green", width = 14
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
