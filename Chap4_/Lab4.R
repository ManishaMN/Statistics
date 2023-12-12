#Question 1 ( estimates of fuel economy)

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(FuelEconomy)
head(cars2010)
head(cars2011)
head(cars2012)

#1correlations and scattter plot
cor(cars2010[, c("FE","EngDispl", "NumCyl", "ExhaustValvesPerCyl" ,"VarValveTiming" )])

pairs(cars2010[, c("FE","EngDispl", "NumCyl", "ExhaustValvesPerCyl" ,"VarValveTiming" )])
#1-1Can linear relationships adequately describe these relationships? not much, to some extent () 
#1-2 Are there any outliers that you should investigate, Yes ( FE Vs ExhaustValves, Varvalve)
#1-3Variables with highest correlation 'for these "EngDispl"
#1-4 What is the p-value for that correlation coefficient? Is it statistically 
#significant at the 0.05 level? What can you conclude p-value: < 2.2e-16
#P is smaller than the significance level of 0.05. 
#This indicates that the coefficient estimate for EngDispl is statistically significant at the 0.05 leve
cor.test(cars2010$FE,cars2010$EngDispl)

#1_B EngDispl Vs EngDispl +0.9060, ExhaustvalvesVsEngDispl- -0.47
cor(cars2010[, c("EngDispl", "NumCyl", "ExhaustValvesPerCyl" ,"VarValveTiming" )])

pairs(cars2010[, c("EngDispl", "NumCyl", "ExhaustValvesPerCyl" ,"VarValveTiming" )])

#1_C Linear regression, Assumptions are close to normal and equal variance

slr <- lm(FE ~ EngDispl, data=cars2010)
par(mfrow=c(2,2)) 
plot(slr)
summary(slr)
# 1803 F value, p-value: < 2.2e-16
#. In this case, the F-statistic is very large, and the associated p-value is extremely small. 
#This indicates strong evidence against the null hypothesis, which assumes that the true coefficients of the model are all zero (i.e., no relationship between the predictor and the response). 
#Therefore, we reject the null hypothesis and conclude that there is a statistically significant relationship between FE and EngDispl
#FE = 50.5632 - 4.5209 * EngDispl
#R-squared value of 0.62 suggests that 62% of the variation in FE can be accounted for by the linear relationship with EngDispl
#-------------------------------
#Question 2 (icecream data set)

df_ic <-read.table("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/icecream.csv")
str(df_ic)
#lm
slr <- lm(Sales ~ Temperature, data=df_ic)
#assumptions ( almost normally distributed)
par(mfrow=c(2,2)) 
plot(slr)
#other way to see normality by plotting histogram
residuals <- residuals(slr)

# Plot a histogram of the residuals
hist(residuals, breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")

#P value yes, there is a evidence of relation P value 1.543e-11
summary(slr)
# parameter estimate is 1.0889 , 
#on average, for every unit increase in temperature, the model predicts an increase of 1.0889 units in sales.

#----------------------------

