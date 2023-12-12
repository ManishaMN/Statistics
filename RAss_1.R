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

#Energy**************************
summary(energy2)

#model
eng_lm <- lm(y ~ x1 + x2 + x3 + x4, data = energy2)

#vif
vif(eng_lm)
round(1.039153, 2)

#residual plots
# residual plots

residuals(eng_lm)
residuals

par(mfrow=c(2,2))
summary(eng_lm)
res <- resid(eng_lm)
plot(energy2$x1, res)
plot(energy2$x2, res)
plot(energy2$x3, res)
plot(energy2$x4, res)

#

# empty full model


full.model <- lm(y ~ x1 + x2 + x3 + x4 + I(x1^2) , data = energy2)

empty.model <- lm(y ~ 1, data = energy2)


# k = log(n) for BIC selection
back.model2 <- step(full.model,
                    scope = list(lower = empty.model,
                                 upper = full.model),
                    direction = "backward", k = log(nrow(energy2))) 



#normality
par(mfrow=c(2,2))
plot(back.model2)

res <- residuals(back.model2)

shapiro.test(x = res)



















