best <- function(state, outcome){
  ##settle work folder as the one with the data
  setwd("C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R projects/programm_assignment3")
  
  ##assign data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Space elimination from the entered outcome
  outcome<-sub(" ",".",outcome)
  
  ##change in uppercase to avoid unwanted mismatch
  disease<-toupper(paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome, sep = ""))
  diseaseslist<-toupper(names(data))
  
  ## Check that state and outcome are valid
  ## see if state exists
  if (any(is.na(table(data$State)[State=state]))) stop("invalid state")
  ##see if outcome exists
  if  (!(disease) %in% diseaseslist) stop("invalid outcome")
  
  
  
  ##keep only researched state data
  data<-data[ which(data$State==state),]
  
  ##useless assign column number with the researched data i.e the outcome
  b<-which(diseaseslist==disease)
  
  ##keep only useful data without na (not available replacement too)
  data<-data.frame(data[["Hospital.Name"]],"Rate"=gsub("Not Available", NA, data[[b]]))
  
  ##transform as numeric for classification
  data$Rate = as.numeric(as.character(data$Rate))
  
  ##sorting by name (should be done on exaequo only)
  ##old data<-na.omit(data[order(data[,1]),])
  ##search for the min in 30.Day.Death..Mortality
  ##old   bb<-which.min(data$Rate)
  ##return result
  ##old   data[bb,1]
  
  ## Taking of NA 
  data<-na.omit(data,data$Rate)

  ## Data with minimum value
  data<-data[which(data$Rate %in% min(data$Rate)),]

  data<-data[order(data[,1]),]  #Alphabetic sort 
  
  #Result
  data[1,1]
}

