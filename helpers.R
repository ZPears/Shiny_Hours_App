uncalcables <- c("Total hours plan", "Total retainer used", "Actual retainer", "OVERSERVICE", "OVERSERVICE %", "EVP", "VP", "Director", "Manager", "Account Supervisor", "Account Executive")

buildFinalData <- function(projData, hoursData) {
  finalData <- projData[,!(names(projData) == '"A" Accounts')]
  for (staff in finalData$STAFF) {
    if (!is.na(staff) & !(staff %in% uncalcables)) {
      hoursVector <- finalData[!is.na(finalData$STAFF) & finalData$STAFF == staff,][4:(which(colnames(finalData)=="billable goal")-1)]
      finalData[!is.na(finalData$STAFF) & finalData$STAFF == staff,][4:(which(colnames(finalData)=="billable goal")-1)] <- calcHours(hoursData, hoursVector, staff)
    }
  }
  
  clientsLength <- 4:(which(colnames(finalData)=="billable goal")-1)
  
  for (colinc in clientsLength) {
    finalData[!is.na(finalData$STAFF) & finalData$STAFF=="Total retainer used", colinc] <- sumCol(finalData, colinc)
    finalData[!is.na(finalData$STAFF) & finalData$STAFF=="OVERSERVICE", colinc] <- calcOverservice(finalData, colinc)
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
  usage <- newFinal[!is.na(newFinal$STAFF) & newFinal$STAFF == "Total retainer used",columnNumber]
  retainer <- newFinal[!is.na(newFinal$STAFF) & newFinal$STAFF == "Actual retainer",columnNumber]
  return(usage - retainer)
}