rankall <- function(outcome, num = "best") {
  ##settle work folder as the one with the data
  setwd("C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R projects/programm_assignment3")
  ##assign data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Space elimination from the entered outcome
  outcome<-sub(" ",".",outcome)
  ##change in uppercase to avoid unwanted mismatch
  disease<-toupper(paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome, sep = ""))
  diseaseslist<-toupper(names(data))
  
  ##see if outcome exists
  if  (!(disease) %in% diseaseslist) stop("invalid outcome")
  
  ## shorten data to essential to lighten the space used for memory
  data=data[c(7,2,which(diseaseslist==disease))]
  names(data)[3]="Rate"

  ##data<-data[ which(data$State=="AR"),]
  ##keep only useful data without na (not available replacement too)
  data[3]<-gsub("Not Available", NA, data[[3]])
  data<-na.omit(data,data$Rate)
  data$Rate = as.numeric(as.character(data$Rate))
  #print(names(data))
  #print(head(data,50))
  data<-data[order(data[,1]),]
  print(tail(data))
            rankhospital_for_rankall <- function(data, nums) {
              ##sorting
              if (num=="worst") 
              {data<-data[order(-data$Rate,data[,2]),] } ##reverse sorting to take the worth, higher first
              else
              {data<-data[order(data$Rate,data[,2]),] }  ##sorting lower first
              
              ##Top if best or worst
              if (num=="best" || num=="worst") {num<-1} 
              
              ##return result
              data[num,1:2]
            }  
  
  
  splitdata <- split(data, data$State)
  sol=lapply(splitdata, rankhospital_for_rankall,nums=num)
  
  }
