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

infections <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/infections.csv') 

summary(infections)
infections$Infection <- as.factor(infections$Infection)

#crosstable

CrossTable(infections$Infection, infections$Location)
(140/287)*100
(136/287)*100
(79/140)*100

#test
CMHtest(table(infections$Infection, infections$Location))$table[1,]

#logistic

logit_inf <- glm(Infection ~ Age + Location, data = infections, family = binomial(link = "logit"))
summary(logit_inf)

100*(exp(cbind(coef(logit_inf), confint(logit_inf)))-1)

survival::concordance(logit_inf)
