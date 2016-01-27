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
  clientsLength <- (which(colnames(finalData)=="Rate")+1):(which(colnames(finalData)=="billable goal")-1)
  warnings <- character(length=0)
  for (colinc in clientsLength) {
    projected <- (as.numeric(finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Actual retainer",colinc]) * as.numeric(dateRange)) * 1.05
    actual <- as.numeric(finalData[!is.na(finalData$STAFF) & finalData$STAFF == "Total retainer used",colinc])
    if (actual > projected) {
      warnings <- append(warnings, names(finalData[,colinc:(colinc+1)][1]))
    }
  }
  warnings
}

findConsultOverage <- function(projData, finalData, dateRange) {
  clientsLength <- (which(colnames(finalData)=="Rate")+1):(which(colnames(finalData)=="billable goal")-1)
  consultants <- finalData$STAFF[!is.na(finalData$STAFF) & !(finalData$STAFF %in% uncalcables)]
  consultWarnings <- character(length=0)
  clientWarnings <- character(length=0)
  for (consultant in consultants) {
    for (i in clientsLength) {
      if (as.numeric(finalData[!is.na(finalData$STAFF) & finalData$STAFF == consultant,i]) > (projData[!is.na(projData$STAFF) & projData$STAFF == consultant,i] * as.numeric(dateRange) * 1.20)) {
        consultWarnings <- append(consultWarnings, consultant)
        clientWarnings <- append(clientWarnings, names(finalData[,i:(i+1)][1]))
      }
    }
  }
  data.frame(consultWarnings, clientWarnings)
}