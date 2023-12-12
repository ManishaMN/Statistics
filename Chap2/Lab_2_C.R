
# Lab 2

# Question 1

install.packages('UsingR') 
library(UsingR) 
data(normtemp)

head(normtemp)

# a
# Ho: avg_temp = 98.6; Ha:avg_temp !=98.6 

t.test(normtemp$temperature, mu=98.6)

# R: True mean is not 98.6


# b
# Confidence Interval: (98.12200, 98.37646)

# c
# Ho: avg_temp = 98.6; Ha:avg_temp !=98.6 
df1<-normtemp[normtemp$gender==1,]
t.test(df1$temperature, mu=98.6)

# Ho: avg_temp = 98.6; Ha:avg_temp !=98.6 
df2<-normtemp[normtemp$gender==2,]
t.test(df2$temperature, mu=98.6)

# The answer does not change independently of gender.

# d

# i) Checking for normality

library(ggplot2)
normtemp$gender <- as.factor(normtemp$gender)
ggplot(data=normtemp, aes(sample = temperature, color=gender)) + stat_qq() + stat_qq_line()
# Result: according to the QQ-plots, normality is met per gender

# ii) Equal variances
# Ho: variances are equal
# Ha: variances are different

var.test(temperature~gender, data=normtemp)
# Result: variances are equal

t.test(temperature~gender, data=normtemp, var.equal=TRUE)
# Ho: temperature_gender_1 = temperature_Gender_2
# Ha: temperature_gender_1 != temperature_Gender_2

# p-value = 0.02393; we reject H0
# temperatures are different between genders



# Question 2

data(AirPassengers)
install.packages("tseries")
install.packages("forecast")
library(tseries)
library(forecast)
library(dplyr)

cycle(AirPassengers)

air1 = data.frame(AirPassengers)
air2 = air1 %>% mutate(summer=ifelse(cycle(AirPassengers) %in% 6:8,1,0))

# checking for normality
air2$summer <- as.factor(air2$summer)
library(ggplot2)
ggplot(data = air2, aes(sample = AirPassengers, color = summer)) + stat_qq() + stat_qq_line()

# Variables are not normal

# Wilcox test
# Ho: air travel in summer = air travel in other months
# Ha: air travel in summer != air travel in other months

wilcox.test(AirPassengers ~ summer,data = air2 )

# Response: Reject Ho.







