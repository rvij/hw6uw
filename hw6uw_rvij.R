setwd("/Users/rajeevvij/Documents/DOcRep/~DataScienceCisco/Module2")
require(sqldf)

# Reading data from the given files into R
readFromFiles <- function() {
  adultfev <<- read.csv("adultfev.csv", header=T, sep=",")
  adultfevmiss <<- read.csv("adultfevmiss.csv", header=T, sep=",")
}

readFromFiles()
