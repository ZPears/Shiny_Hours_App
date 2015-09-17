buildFinalData <- function(projData, hoursData) {
  finalData <- projData
  for (staff in finalData$STAFF) {
    if (!is.na(staff) & !(staff %in% c("Phil Greenough", "EVP", "VP", "Director", "Manager", "Account Supervisor", "Account Executive"))) {
      hoursVector <- finalData[!is.na(finalData$STAFF) & finalData$STAFF == staff,][6:which(colnames(projData)=="billable goal")-1]
      finalData[!is.na(finalData$STAFF) & finalData$STAFF == staff,][6:which(colnames(projData)=="billable goal")-1] <- calcHours(hoursData, hoursVector, staff)
    }
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