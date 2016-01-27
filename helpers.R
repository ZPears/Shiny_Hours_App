uncalcables <- c("Total hours plan", "Total retainer used", "Actual retainer", "OVERSERVICE", "SERVICE %", "EVP", "VP", "Director", "Manager", "Account Supervisor", "Account Executive")

buildFinalData <- function(projData, hoursData) {
  names(projData) <- gsub("\\.", " ", names(projData))
  finalData <- projData[,!(names(projData) == '\"A\" Accounts')]
  for (staff in finalData$STAFF) {
    if (!is.na(staff) & !(staff %in% uncalcables)) {
      hoursVector <- finalData[!is.na(finalData$STAFF) & finalData$STAFF == staff,][(which(colnames(finalData)=="Rate")+1):(which(colnames(finalData)=="billable goal")-1)]
      finalData[!is.na(finalData$STAFF) & finalData$STAFF == staff,][(which(colnames(finalData)=="Rate")+1):(which(colnames(finalData)=="billable goal")-1)] <- calcHours(hoursData, hoursVector, staff)
    }
  }

  clientsLength <- (which(colnames(finalData)=="Rate")+1):(which(colnames(finalData)=="billable goal")-1)
  
  for (colinc in clientsLength) {
    finalData[!is.na(finalData$STAFF) & finalData$STAFF=="Total retainer used", colinc] <- sumCol(finalData, colinc)
    finalData[!is.na(finalData$STAFF) & finalData$STAFF=="OVERSERVICE", colinc] <- calcOverservice(finalData, colinc)
    finalData[!is.na(finalData$STAFF) & finalData$STAFF=="SERVICE %", colinc] <- calcOverPerc(finalData, colinc)
    finalData[!is.na(finalData$STAFF) & finalData$STAFF=="Total hours plan", colinc] <- calcHoursUsed(finalData, colinc)
  }
  
  consultants <- finalData$STAFF[!is.na(finalData$STAFF) & !(finalData$STAFF %in% uncalcables) & finalData$STAFF != c("Phil Greenough", "Jamie Parker")]
  
  for (consultant in consultants) {
    total <- sum(subset(hoursData, Consultant == consultant & Client != "Agency")[,"Total.Hours"])
    finalData[!is.na(finalData$STAFF) & finalData$STAFF == consultant, "hours billed"] <- total
  }
  
  finalData
}

calcHours <- function(hoursData, hoursVector, staff) {
  finalVector <- hoursVector
  newHoursVec <- hoursData[hoursData$Consultant == staff,]
  newHoursVec <- newHoursVec[order(newHoursVec$Client, newHoursVec$Project),]
  
  if (nrow(newHoursVec) == 0) {
    
    finalVector <- rep(0, length(finalVector))
    
  } else {
  
    newHours <- aggregate(Total.Hours ~ Client, sum, data = newHoursVec)
    
    newHours$Client <- as.character(newHours$Client)
  
    for (client in names(finalVector)) {
      if (client %in% newHours$Client) {
        finalVector[client] <- newHours[newHours$Client==client, 2]
      } else {
        finalVector[client] <- 0
      }
    }
    
  }
  
  return(finalVector)
   
}

calcHoursUsed <- function(finalData, columnNumber) {
  sum(as.numeric(finalData[!is.na(finalData$STAFF) & !(finalData$STAFF %in% uncalcables) , columnNumber]), na.rm=TRUE)
}

sumCol <- function(finalData, columnNumber) {
  sum <- 0
  for (rowinc in 1:length(finalData$STAFF)) {
    if (!is.na(finalData[rowinc,1]) & !(finalData[rowinc,1] %in% uncalcables)) {
      sum <- sum + (finalData[rowinc,3] * finalData[rowinc,columnNumber])
    }
  }
  sum
}

calcOverservice <- function(finalData, columnNumber) {
  usage <- finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Total retainer used",columnNumber]
  retainer <- finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Actual retainer",columnNumber]
  return(usage - retainer)
}

calcOverPerc <- function(finalData, columnNumber) {
  usage <- finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Total retainer used",columnNumber]
  retainer <- finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Actual retainer",columnNumber]
  perc <- round(usage / retainer, 2)
  if (!is.finite(perc)) {
    "No Data"
  } else {
    paste0(as.character(100 * perc), "%")
  }
}

findProjOverage <- function(finalData, dateRange) {
  clientsLength <- (which(colnames(finalData)=="Agency")+1):(which(colnames(finalData)=="billable goal")-1)
  warnings <- character(length=0)
  for (colinc in clientsLength) {
    projected <- (as.numeric(finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Actual retainer",colinc]) * as.numeric(dateRange)) * 1.15
    actual <- as.numeric(finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Total retainer used",colinc])
    if (actual > projected) {
      warnings <- append(warnings, names(finalData[,colinc:(colinc+1)][1]))
    }
  }
  warnings
}

findConsultOverage <- function(projData, finalData, dateRange) {
  clientsLength <- (which(colnames(finalData)=="Agency")+1):(which(colnames(finalData)=="billable goal")-1)
  consultants <- finalData$STAFF[!is.na(finalData$STAFF) & !(finalData$STAFF %in% uncalcables)]
  consultWarnings <- character(length=0)
  clientWarnings <- character(length=0)
  percOver <- numeric(length=0)
  for (consultant in consultants) {
    for (i in clientsLength) {
      actual <- as.numeric(finalData[!is.na(finalData$STAFF) & finalData$STAFF == consultant,i])
      threshold <- (projData[!is.na(projData$STAFF) & projData$STAFF == consultant,i] * as.numeric(dateRange))
      if (actual > (threshold * 1.20)) {
        consultWarnings <- append(consultWarnings, consultant)
        clientWarnings <- append(clientWarnings, names(finalData[,i:(i+1)][1]))
        overage <- round((actual/threshold) * 100 - 100, 0)
        if (!is.infinite(overage)) {
          percOver <- append(percOver, paste0(as.character(overage), "%"))
        } else {
          percOver <- append(percOver, "no hours allotted.")
        }
      }
    }
  }
  data.frame(consultWarnings, clientWarnings, percOver)
}

findUnderUtil <- function(projData, finalData, dateRange, hoursData) {
  clientsLength <- (which(colnames(finalData)=="Agency")+1):(which(colnames(finalData)=="billable goal")-1)
  consultants <- finalData$STAFF[!is.na(finalData$STAFF) & !(finalData$STAFF %in% uncalcables) & finalData$STAFF != c("Phil Greenough", "Jamie Parker")]
  consultUnders <- character(length=0)
  percActual <- numeric(length=0)
  billedHours <- numeric(length=0)
  agencyHours <- numeric(length=0)
  for (consultant in consultants) {
    consultantActuals <- hoursData[hoursData$Consultant ==  consultant, ]
    billable <- sum(subset(consultantActuals, Consultant == consultant & Client != "Agency")[,"Total.Hours"])
    agencyHrs <- sum(subset(consultantActuals, Consultant == consultant & Client == "Agency")[,"Total.Hours"])
    goal <- as.numeric(finalData[!is.na(finalData$STAFF) & finalData$STAFF == consultant, "billable goal"] * as.numeric(dateRange))
    threshold <- goal * .85
    if (billable <= threshold) {
      consultUnders <- append(consultUnders, consultant)
      actual <- round((billable/goal) * 100, 0)
      if (!is.infinite(actual) & !is.nan(actual)) {
        percActual <- append(percActual, paste0(as.character(actual), "%"))
        billedHours <- append(billedHours, billable)
        agencyHours <- append(agencyHours, agencyHrs)
      } else {
        percActual <- append(percActual, "0%")
        billedHours <- append(billedHours, "0")
        agencyHours <- append(agencyHours, "0")
      }
    }
  }
  total <- as.numeric(billedHours) + as.numeric(agencyHours)
  data.frame(consultUnders, percActual, billedHours, agencyHours, total)
}