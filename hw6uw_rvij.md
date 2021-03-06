Homework # 6 UW Data Science course by rvij@cisco.com
========================================================

# Define functions used in the homework 


```r
setwd("/Users/rajeevvij/Documents/DOcRep/~DataScienceCisco/Module2")

# Reading data from the given files into R
readFromFiles <- function() {
    print("Read data files and print summaries")
    adultfev <<- read.csv("/Users/rajeevvij/Documents/DOcRep/~DataScienceCisco/Module2/adultfev.csv", 
        header = T, sep = ",", colClasses = c("character", "integer", "numeric", 
            "numeric", "factor", "factor"))
    adultfevmiss <<- read.csv("/Users/rajeevvij/Documents/DOcRep/~DataScienceCisco/Module2/adultfevmiss.csv", 
        header = T, sep = ",", colClasses = c("character", "integer", "numeric", 
            "numeric", "factor", "factor"))
    print(summary(adultfev))
    print(summary(adultfevmiss))
}

# Estimate the adjusted effect of smoking on fev.  The assumptions of linear
# regression that we covered in class were linearity, normality of
# residuals, homoskedasticity of residuals, and independence of residuals.
# Using qqplots, and boxplots of the residuals, assess these assumptions for
# the proposed analysis.  (note, one of the assumptions does not apply for a
# binary explanatory variable).


question1 <- function() {
    print("Perform lm and plot dignistic charts for the residuals against different variables")
    lm1 <<- lm(fev ~ smoke + height + age + male, data = adultfev)
    summarylm1 <<- summary(lm1)
    anovalm1 <<- anova(lm1)
    par(mfrow = c(2, 2))
    plot(lm1)
    par(mfrow = c(2, 2))
    qqplot(adultfev$smoke, lm1$resid)
    qqplot(adultfev$male, lm1$resid)
    qqplot(adultfev$age, lm1$resid)
    qqplot(adultfev$height, lm1$resid)
}


question2 <- function() {
    print("Print summary of the lm and provide details of the p-value and slopes")
    print(summarylm1)
    print(anovalm1)
    print(confint(lm1))
}

question3 <- function() {
    print("How to address violations of the assumptions in lm")
    print("Use transformation of variables to address linearity and homoskedasticity of residuals")
    print("===========")
}

question4 <- function() {
    print("Perform lm with missing data anf complete cases")
    adultfevmissing_cc <- adultfevmiss[complete.cases(adultfevmiss), ]
    lm1cc <<- lm(fev ~ smoke + height + age + male, data = adultfevmissing_cc)
    summarylm1cc <- summary(lm1cc)
    anovalm1cc <- anova(lm1cc)
    print(summarylm1cc)
    print(anovalm1cc)
}

question5 <- function() {
    print("Impute missing data using mice package and compare with complete lm")
    require(mice)
    adultfevmiss.imputed <- mice(adultfevmiss)
    
    summarylmcc <- summary(lm(fev ~ smoke + height + age + male, data = adultfevmiss))
    
    impute.fit <- with(adultfevmiss.imputed, lm(fev ~ smoke + height + age + 
        male))
    print(summarylmcc)
    print(summary(pool(impute.fit)))
}

question6 <- function() {
    print("Calculate the power of a test")
    power08 <- power.t.test(n = 300, power = 0.8, sd = 75)
    power09 <- power.t.test(n = 300, power = 0.9, sd = 75)
    print(power08)
    print(power09)
}
```


# Call functions used in the homework for different questions

## Read data files and print summaries

* Fitting fev against 4 variables smoke, age, height, male to see the adjustments in the smoke variable
* Ploting the diagnistic graphs, 
** graphs shows that condition of homoskedasticity is violated and also the residual is normally distributed
** Ploting residuals against 4 predictor variables shows residual is not evenly distributed around 0 for smoke and age


```r
readFromFiles()
```

```
## [1] "Read data files and print summaries"
##       X                  age            fev            height     male   
##  Length:725         Min.   :65.0   Min.   :0.408   Min.   :54.5   0:363  
##  Class :character   1st Qu.:71.0   1st Qu.:1.745   1st Qu.:62.0   1:362  
##  Mode  :character   Median :74.0   Median :2.158   Median :65.5          
##                     Mean   :74.6   Mean   :2.207   Mean   :65.3          
##                     3rd Qu.:78.0   3rd Qu.:2.649   3rd Qu.:68.5          
##                     Max.   :99.0   Max.   :4.471   Max.   :75.0          
##  smoke  
##  0:629  
##  1: 96  
##         
##         
##         
##         
##       X                  age            fev            height     male   
##  Length:735         Min.   :65.0   Min.   :0.408   Min.   :57.0   0:369  
##  Class :character   1st Qu.:71.0   1st Qu.:1.745   1st Qu.:63.0   1:366  
##  Mode  :character   Median :74.0   Median :2.158   Median :66.5          
##                     Mean   :74.6   Mean   :2.207   Mean   :66.1          
##                     3rd Qu.:78.0   3rd Qu.:2.649   3rd Qu.:69.0          
##                     Max.   :99.0   Max.   :4.471   Max.   :75.0          
##                                    NA's   :10      NA's   :200           
##  smoke  
##  0:636  
##  1: 99  
##         
##         
##         
##         
## 
```


## Perform lm and plot digonistic plots 

```r
question1()
```

```
## [1] "Perform lm and plot dignistic charts for the residuals against different variables"
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) 


## Print summary of the lm and provide details of the p-value and slopes
* Printing the p-value and coeffcient for the variables from summary(lm1) and anova(lm1)
* Results shows that model is able to explain around 40% of variation in data 
* We can also get confidence intervals for the variables 
* All the p-values are significant

```r
question2()
```

```
## [1] "Print summary of the lm and provide details of the p-value and slopes"
## 
## Call:
## lm(formula = fev ~ smoke + height + age + male, data = adultfev)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.6390 -0.2668  0.0498  0.3145  1.5276 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.68304    0.61795   -1.11     0.27    
## smoke1      -0.35473    0.05872   -6.04  2.5e-09 ***
## height       0.07160    0.00799    8.96  < 2e-16 ***
## age         -0.02515    0.00371   -6.78  2.6e-11 ***
## male1        0.27452    0.06061    4.53  6.9e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.532 on 720 degrees of freedom
## Multiple R-squared:  0.405,	Adjusted R-squared:  0.401 
## F-statistic:  122 on 4 and 720 DF,  p-value: <2e-16
## 
## Analysis of Variance Table
## 
## Response: fev
##            Df Sum Sq Mean Sq F value  Pr(>F)    
## smoke       1   10.9    10.9    38.5 9.1e-10 ***
## height      1  111.2   111.2   393.0 < 2e-16 ***
## age         1   10.6    10.6    37.4 1.6e-09 ***
## male        1    5.8     5.8    20.5 6.9e-06 ***
## Residuals 720  203.7     0.3                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##                2.5 %   97.5 %
## (Intercept) -1.89624  0.53017
## smoke1      -0.47002 -0.23944
## height       0.05591  0.08729
## age         -0.03243 -0.01786
## male1        0.15553  0.39352
```


## Resolving lm residual assumption violations
* lm assumptions violations may be overcome by transforming variables 
* Also designing an experiment and controlling some variability may provide better results.
* Look for other variables that can explain the unexplained variation in data


```r
question3()
```

```
## [1] "How to address violations of the assumptions in lm"
## [1] "Use transformation of variables to address linearity and homoskedasticity of residuals"
## [1] "==========="
```


## Perform lm with missing data on complete cases 
* Fitting lm with missing data, using only complete cases
* Results shows that value of smoke coeffcient has changed to -0.47001592 from earlier -0.35


```r
question4()
```

```
## [1] "Perform lm with missing data anf complete cases"
## 
## Call:
## lm(formula = fev ~ smoke + height + age + male, data = adultfevmissing_cc)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.0852 -0.2663 -0.0017  0.2722  1.3668 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.48445    0.63244   -0.77     0.44    
## smoke1      -0.43330    0.06257   -6.92  1.3e-11 ***
## height       0.06879    0.00806    8.53  < 2e-16 ***
## age         -0.02346    0.00391   -6.00  3.7e-09 ***
## male1        0.30416    0.05946    5.12  4.4e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.447 on 520 degrees of freedom
## Multiple R-squared:  0.459,	Adjusted R-squared:  0.454 
## F-statistic:  110 on 4 and 520 DF,  p-value: <2e-16
## 
## Analysis of Variance Table
## 
## Response: fev
##            Df Sum Sq Mean Sq F value  Pr(>F)    
## smoke       1    8.2     8.2    40.8 3.7e-10 ***
## height      1   69.7    69.7   348.3 < 2e-16 ***
## age         1    5.1     5.1    25.3 6.8e-07 ***
## male        1    5.2     5.2    26.2 4.4e-07 ***
## Residuals 520  104.1     0.2                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Impute missing values using mice package and perform lm with new imputed values and compare with complete
* Imputing missing values using mice package and refitting lm
* Comparing lm results with missing data and imputed data
* Imputed values may have bias towards values being estimated to an average value


```r
question5()
```

```
## [1] "Impute missing data using mice package and compare with complete lm"
```

```
## Loading required package: mice
## Loading required package: Rcpp
## mice 2.21 2014-02-05
```

```
## 
##  iter imp variable
##   1   1  fev  height
##   1   2  fev  height
##   1   3  fev  height
##   1   4  fev  height
##   1   5  fev  height
##   2   1  fev  height
##   2   2  fev  height
##   2   3  fev  height
##   2   4  fev  height
##   2   5  fev  height
##   3   1  fev  height
##   3   2  fev  height
##   3   3  fev  height
##   3   4  fev  height
##   3   5  fev  height
##   4   1  fev  height
##   4   2  fev  height
##   4   3  fev  height
##   4   4  fev  height
##   4   5  fev  height
##   5   1  fev  height
##   5   2  fev  height
##   5   3  fev  height
##   5   4  fev  height
##   5   5  fev  height
## 
## Call:
## lm(formula = fev ~ smoke + height + age + male, data = adultfevmiss)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.0852 -0.2663 -0.0017  0.2722  1.3668 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.48445    0.63244   -0.77     0.44    
## smoke1      -0.43330    0.06257   -6.92  1.3e-11 ***
## height       0.06879    0.00806    8.53  < 2e-16 ***
## age         -0.02346    0.00391   -6.00  3.7e-09 ***
## male1        0.30416    0.05946    5.12  4.4e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.447 on 520 degrees of freedom
##   (210 observations deleted due to missingness)
## Multiple R-squared:  0.459,	Adjusted R-squared:  0.454 
## F-statistic:  110 on 4 and 520 DF,  p-value: <2e-16
## 
##                  est       se      t    df  Pr(>|t|)    lo 95    hi 95
## (Intercept) -1.03363 0.638402 -1.619 339.8 1.064e-01 -2.28934  0.22209
## smoke2      -0.35519 0.059378 -5.982 432.8 4.628e-09 -0.47189 -0.23848
## height       0.07642 0.008375  9.124 248.8 0.000e+00  0.05992  0.09291
## age         -0.02460 0.003760 -6.542 524.5 1.444e-10 -0.03199 -0.01721
## male2        0.25738 0.063649  4.044 172.3 7.925e-05  0.13174  0.38301
##             nmis     fmi  lambda
## (Intercept)   NA 0.08169 0.07630
## smoke2        NA 0.06269 0.05837
## height       200 0.10701 0.09986
## age            0 0.04701 0.04339
## male2         NA 0.13994 0.13001
```

## Estimate the effect size for a given power of a test

* computing power for 0.8 the effect size is ~ 17 and for power of 0.9 the effect size ~ 20

```r
question6()
```

```
## [1] "Calculate the power of a test"
## 
##      Two-sample t test power calculation 
## 
##               n = 300
##           delta = 17.18
##              sd = 75
##       sig.level = 0.05
##           power = 0.8
##     alternative = two.sided
## 
## NOTE: n is number in *each* group
## 
## 
##      Two-sample t test power calculation 
## 
##               n = 300
##           delta = 19.88
##              sd = 75
##       sig.level = 0.05
##           power = 0.9
##     alternative = two.sided
## 
## NOTE: n is number in *each* group
```


