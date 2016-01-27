library(shiny)
library(shinydashboard)
library(ggplot2)
library(openxlsx)
library(lubridate)

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
  
  dateRange <- reactive({
    (input$dateRange[2] - input$dateRange[1]) / 30
  })
  
  finalFile <- reactive({
    projData <- projData()
    if (is.null(projData)) return(NULL)
    hoursData <- hoursData()
    if (is.null(hoursData)) return(NULL)
    finalFile <- buildFinalData(projData, hoursData)
    finalFile$`Billability -Client Only` <- round(finalFile$`Billability -Client Only` * 100, 2)
    finalFile$`% Utilization` <- round(finalFile$`% Utilization` * 100, 2)
    finalFile
  })
  
  clientAlerts <- reactive({
    if (is.null(finalFile())) return("<strong>Upload your files to begin.</strong>")
    alerts <- findProjOverage(finalFile(), dateRange())
    output <- character(length=0)
    for (i in 1:length(alerts)) {
      output <- append(output, paste0("<div id=alert><strong>", alerts[i], "</strong></div>", " has exceeded its retainer. <strong>(", subset(finalFile(), STAFF == "SERVICE %", names(finalFile()) == alerts[i]),  ")</strong> <br/><br/>"))
    }
    output
  })
  
  consultantAlerts <- reactive({
    if (is.null(finalFile())) return("<strong>Upload your files to begin.</strong>")
    alerts <- findConsultOverage(projData(), finalFile(), dateRange())
    output <- character(length=0)
    uniqueVals <- (unique(as.character(alerts[,1])))
    for (consultant in uniqueVals) {
      overagesByConsult <- subset(alerts, alerts[,1] == consultant)
      print(overagesByConsult)
      formattedOverages <- character(length=0)
      print(1:nrow(overagesByConsult))
      for (i in 1:nrow(overagesByConsult)) {
        formattedOverages <- paste0(formattedOverages, paste0("<strong>", overagesByConsult[i,2], "</strong> by <strong>X%</strong><br/> "))
      }
      print(length(formattedOverages))
      output <- append(output, paste0("<div id=alert><strong>", consultant, "</strong></div>", " has exceeded his/her projection on: <br/>", formattedOverages, "<br/>"))
    }
    output
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
  
  output$dateRange <- renderText(
    dateRange()
  )
  
  output$clientAlerts <- renderUI(
    HTML(
      clientAlerts()
    )
  )
  
  output$consultantAlerts <- renderUI(
    HTML(
      consultantAlerts()
    )
  )
  
  #output$mytable <- renderDataTable({
  #  finalFile()
  #})
  
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
    expected <- projData[!is.na(projData$STAFF) & projData$STAFF == "Total hours plan", names(projData)==input$clientSelector]
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
  ##
  
  output$totalHoursBox <- renderValueBox({
    valueBox(
      sum(consultantDataInput()$Total.Hours), "Total Hours", icon = icon("calendar"), color = "blue"
    )
  })
  
  output$billableGoalBox <- renderUI({
    data <- projData()
    ans <- data[!is.na(data$STAFF) & data$STAFF == input$consultantSelector, names(data)=="billable goal"]
    
    valueBox(
      ans, "Billable Goal", icon = icon("line-chart"), color = "blue", width = 14
    )
  })
  
  output$assignedClientHoursBox <- renderUI({
    data <- projData()
    ans <- data[!is.na(data$STAFF) & data$STAFF == input$consultantSelector, names(data)=="Assigned Client Hours"]
    
    valueBox(
      ans, "Assigned Client Hours", icon = icon("calendar"), color = "green", width = 14
    )
  })
  
  output$billabilityBox <- renderUI({
    data <- finalFile()
    ans <- paste0(data[!is.na(data$STAFF) & data$STAFF == input$consultantSelector, names(data)=="Billability -Client Only"], "%")
    valueBox(
      ans, "Billability", icon = icon("dollar"), color = "blue", width = 14
    )
  })
  
  output$availabilityBox <- renderUI({
    data <- finalFile()
    goal <- data[!is.na(data$STAFF) & data$STAFF == input$consultantSelector, names(data)=="billable goal"]
    assigned <- data[!is.na(data$STAFF) & data$STAFF == input$consultantSelector, names(data)=="Assigned Client Hours"]
    ans <- goal - assigned
    
    valueBox(
      ans, "Availability", icon = icon("calendar"), color = "green", width = 14
    )
  })
  
  output$hoursBilledBox <- renderValueBox({
    valueBox(
      sum(subset(consultantDataInput(), Client != "Agency")$Total.Hours),
      "Hours Billed", icon = icon("line-chart"), color = "blue"
    )
  })
  
  output$utilizationBox <- renderUI({
    data <- finalFile()
    ans <- paste0(data[!is.na(data$STAFF) & data$STAFF == input$consultantSelector, names(data)=="% Utilization"], "%")
    
    valueBox(
      ans, "Utilization", icon = icon("calendar"), color = "green", width = 14
    )
  })
  
})
