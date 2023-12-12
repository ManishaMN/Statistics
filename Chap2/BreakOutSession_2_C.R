
# BreakOut Session 2

covid <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/coviddata.csv')

# Go to repository
# Select the data set (in csv format) you want to use
# Press "Raw"
# The internet address that appears at the top is the one to use in the "read.csv" command

head(covid)

# Question 1

# a

# i) Verifying Normality

library(ggplot2)
ggplot(data=covid, aes(sample = covidDeathsPerCapita, color=region)) + stat_qq() + stat_qq_line()
# Result: according to the QQ-plots, normality is met in each region

# ii) Check for equality of variances

var.test(covidDeathsPerCapita~region, data=covid)
# Result: variances are equal

# iii) Two-sample t-test

t.test(covidDeathsPerCapita~region, data=covid, var.equal=TRUE)
# Ho: covid_deaths_east = covid_deaths_west
# Ha: covid_deaths_east != covid_deaths_west

# Result: Reject Ho.  

# b
# According to the results obtained from the t-test, covid deaths per 
# capita are different between the East and the West.

# c
# We cannot conclude that covid death rates per capita are greater in one
# region against the other, because there are several factors that can affect 
# death rates (vaccinationn, age, sex, previous illnesses, etc.).  So, we
# would need to make a better sample selection taking into account all these
# factors. 









