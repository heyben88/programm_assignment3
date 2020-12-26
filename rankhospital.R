rankhospital <- function(state, outcome, num = "best") {
  ##settle work folder as the one with the data
  setwd("C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R projects/programm_assignment3")
  
  ##assign data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  nbrow<-nrow(data)
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
  
  ##useless assign column number with the researched data
  b<-which(diseaseslist==disease)
  
  ##keep only researched state data
  data<-data[ which(data$State==state),]
  

  ##keep only useful data without na (not available replacement too)
  data<-na.omit(data.frame(data[["Hospital.Name"]],"Rate"=gsub("Not Available", NA, data[[b]])))
  
  ##transform as numeric
  data$Rate = as.numeric(as.character(data$Rate))

  
  if (num=="worst") 
   {data<-data[order(-data$Rate,data[,1]),] } ##reverse sorting to take the worth, higher first
  else
  ##sorting lower first
  {data<-data[order(data$Rate,data[,1]),] }

  ##Top if best or worst
  if (num=="best" || num=="worst") {num<-1} 
  
  ##return result
  data[num,1]

}