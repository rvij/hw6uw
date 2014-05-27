setwd("/Users/rajeevvij/Documents/DOcRep/~DataScienceCisco/Module2")
require(sqldf)

# Reading data from the given files into R
readFromFiles <- function() {
  adultfev <<- read.csv("adultfev.csv", header=T, sep=",", colClasses = c("character","integer","numeric", "numeric", "factor","factor"))
  adultfevmiss <<- read.csv("adultfevmiss.csv", header=T, sep=",", colClasses = c("character","integer","numeric", "numeric", "factor","factor"))
}

# Estimate the adjusted effect of smoking on fev. 
# The assumptions of linear regression that we covered in class were linearity, 
# normality of residuals, homoskedasticity of residuals, and independence of residuals.  
# Using qqplots, and boxplots of the residuals, assess these assumptions for the proposed analysis. 
# (note, one of the assumptions does not apply for a binary explanatory variable).

question1 <- function(){
  lm1 <<- lm(fev ~ smoke+height+age+male, data=adultfev)
  summarylm1 <- summary(lm1)
  anovalm1 <- anova(lm1)
  print(summarylm1)
  print(anovalm1)
}

question4 <- function(){
  adultfevmissing_cc <- adultfevmiss[complete.cases(adultfevmiss),]
  lm1cc <<- lm(fev ~ smoke+height+age+male, data=adultfevmissing_cc)
  summarylm1cc <- summary(lm1cc)
  anovalm1cc <- anova(lm1cc)
  print(summarylm1cc)
  print(anovalm1cc)
}

question5 <- function(){
  adultfevmissing_cc <- adultfevmiss[complete.cases(adultfevmiss),]
  lm1cc <<- lm(fev ~ smoke+height+age+male, data=adultfevmissing_cc)
  summarylm1cc <- summary(lm1cc)
  anovalm1cc <- anova(lm1cc)
  print(summarylm1cc)
  print(anovalm1cc)
}

question6 <- function(){
  power08 <- power.t.test(n=300, power=0.8, sd=75)
  power09 <- power.t.test(n=300, power=0.9, sd=75)
  print(power08)
  print(power09)
}

readFromFiles()
summary(adultfev)
summary(adultfevmiss)

question1()
question4()
question6()

anova(lm(fev ~ smoke, data=adultfev))

par(mfrow = c(2,2))
plot(lm1)

require(data.table)
ab <-  fread("/Users/michaelyoung/Dropbox/classes/data science/Content/Lecture2/abaloneTrain.csv")

require(ggplot2)


ab.mar <- copy(ab)
ab.mar[gutweight>.25, allweight:=NA] #you do include the outcome

require(mice)
ab.mar.imputed <- mice(ab.mar)

summary(lm(rings~allweight, data=ab))
summary(lm(rings~allweight, data=ab.mar))


impute.fit <- with(ab.mar.imputed, lm(rings~allweight))
summary(pool(impute.fit))

par(mfrow = c(1,1))
boxplot(lm1$resid ~ adultfev$smoke)

ggplot(adultfev, aes(smoke,fev ,fill=smoke)) + geom_boxplot()

ggplot(adultfev,aes(height, fev, color=smoke)) + geom_point() + geom_smooth(method='lm')
