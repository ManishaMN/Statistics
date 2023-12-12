#multiple linear regression

#library
library(tidyverse)
library(dplyr)
library(AmesHousing)

#loading ames
ames <- make_ordinal_ames()
set.seed(123)
ames <- ames %>%
  mutate(id = row_number())

#training ameshousing ddta
train <- ames %>% 
  sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')

#MLR - multiple linear regression
ames_lm2 <- lm(Sale_Price ~ Gr_Liv_Area + TotRms_AbvGrd, data = train)
summary(ames_lm2)
#look at golbal F test p value and individual t test

#Global P value for each categ variable *******
#“car::Anova” function in R on your linear regression object to get the p-values for each categorical variable.
car::Anova(ames_lm2)

#Assumptions - normality variance , linear 
# residual plots
par(mfrow=c(2,2))
plot(ames_lm2)

#coefficient of determination Adj R ^2
summary(ames_lm2)

#categorical - dummy - refernce coding
# central air
ames_lm4 <- lm(Sale_Price ~ Gr_Liv_Area + TotRms_AbvGrd + Central_Air, data = train)

summary(ames_lm4)
#We estimate the average difference in sales price
#between homes with central air and without central air to be $54,513.08.