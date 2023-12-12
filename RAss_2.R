#libraries
library(car)
library(tidyverse)
library(stats)
library(DescTools)
library(AppliedPredictiveModeling)
library(nortest)
library(lmtest)
library(glmnet)
library(gmodels)
library(vcdExtra)
library(survival)

#load data
energy2 <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/energy2.csv') 

website <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/website_clicks.csv') 

infections <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/infections.csv')

#
summary(website)
website$x1 <- as.factor(website$x1)
website$x2 <- as.factor(website$x2)

table(website$x1, website$Obs)
dim(website)
unique(website$y)


ggplot(website) + geom_bar(aes(x=x1,y= y), position = "dodge", stat = "summary", fun = "mean") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


ggplot(website, aes(y=y, x=x1, fill=x1)) + geom_boxplot()
#10 #11
website %>% 
  group_by(x1) %>% 
  summarize(average = mean(y))
  

table(website$x1, website$x2)  
View(website)
#12
web_aov <- aov(y ~ x1*x2, data = website)

summary(web_aov)
#normality
plot(web_aov, 2)
#variance
leveneTest(web_aov)

#14
website %>% 
  group_by(x1, x2) %>% 
  summarize(average = mean(y))
