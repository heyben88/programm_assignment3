## This is a markdown file
setwd("C:/Users/pc/Documents/2 SEIMA/Benas/travail/data analysis/R projects/ProgrammingAssignment3")
outcomesss <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcomesss)
ncol(outcomesss)
nrow(outcomesss)
names (outcomesss)
outcomesss[, 11] <- as.numeric(outcomesss[, 11])
 ## You may get a warning about NAs being introduced; that is okay
hist(outcomesss[, 11])
names(outcomesss)
head(outcomesss[c(State)])