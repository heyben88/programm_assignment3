best <- function(state, outcome){
  ##settle work folder as the one with the data
  setwd("C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R projects/ProgrammingAssignment3")
  
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
  
  ##Split by state
  s<-split(data,data$state)
  lapply(s,function(data){which.min(data)}) 
  
  lapply(s, function(x) {
    +         colMeans(x[, c("Ozone", "Solar.R", "Wind")])
  
}  
  ##useless assign column number with the reserached data
  b<-which(diseaseslist==disease)
  print(b)
  ## Return hospital name in that state with lowest 30-day death## rate
  
  subdata<-data[ which(data$State==state),]
  
  ##newdata <- mydata[c(1,5:10)] 
  bb<-which.min(subdata[,b])
  subdata[bb,1:5]
##}
  