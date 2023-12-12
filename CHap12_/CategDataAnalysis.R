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

#creating categorical variable
train <- train %>%
  mutate(Bonus = ifelse(Sale_Price > 175000, 1, 0))

#distribution using table , bar plot
table(train$Bonus)
ggplot(data = train) +geom_bar(mapping = aes(x = Bonus))

table(train$Central_Air)
ggplot(data = train) + geom_bar(mapping = aes(x = Central_Air))

# if we want to explore two variables together we look at cross-tabulation tables
table(train$Bonus, train$Central_Air)
prop.table(table(train$Bonus, train$Central_Air)) #proportion

# 2 variables, bar plot
ggplot(data = train) +
  geom_bar(mapping = aes(x = Bonus, fill = Central_Air))

#breakdown cross tables
library(gmodels)
CrossTable(train$Central_Air, train$Bonus)

#chi square
chisq.test(table(train$Central_Air, train$Bonus))

#fisher test
fisher.test(table(train$Central_Air, train$Bonus))

#Mantel-Haenszel  χ2test( 2 ordinal)
library(vcdExtra)

CMHtest(table(train$Central_Air, train$Bonus))$table[1,]

#Odds Ratio****
library(DescTools)
##homes without central air are 22.2 times more likely (in terms of odds) to not be bonus eligible as compared to homes with central air. 
OddsRatio(table(train$Central_Air, train$Bonus))

#Cramers V
assocstats(table(train$Central_Air, train$Bonus))

#Spearman's correlation
cor.test(x = as.numeric(ordered(train$Central_Air)), 
         y = as.numeric(ordered(train$Bonus)), 
         method = "spearman")
x
y
