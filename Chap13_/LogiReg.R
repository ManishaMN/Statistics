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

#logistic regression
ames_logit <- glm(Bonus ~ Gr_Liv_Area, 
                  data = train, family = binomial(link = "logit"))
summary(ames_logit)
#An increase of one unit of greater living area square footage is linearly related to the logit not the probability of bonus eligibility.
100*(exp(cbind(coef(ames_logit), confint(ames_logit)))-1)
#every additional square foot in greater living area in the home leads to an average increase in odds of 0.385% to be bonus eligible.


#addding categorical and continuous to logistic regression
ames_logit2 <- glm(Bonus ~ Gr_Liv_Area + Central_Air + factor(Fireplaces), data = train, family = binomial(link = "logit"))
summary(ames_logit2)

#cofficients are to be transformed to be interpreted
100*(exp(cbind(coef(ames_logit2), confint(ames_logit2)))-1)
#home with one fireplace has, on average, 167.04% higher odds of being bonus eligible as compared to a home with zero fireplaces.

#concordance
library(survival)

survival::concordance(ames_logit)
survival::concordance(ames_logit2)

#----------------------------------------

#variable selection
train_sel_log <- train %>% 
  dplyr::select(Bonus, 
                Lot_Area,
                Street,
                Bldg_Type,
                House_Style,
                Overall_Qual,
                Roof_Style,
                Central_Air,
                First_Flr_SF,
                Second_Flr_SF,
                Full_Bath,
                Half_Bath,
                Fireplaces,
                Garage_Area,
                Gr_Liv_Area, 
                TotRms_AbvGrd) %>%
  replace(is.na(.), 0)

full.model <- glm(Bonus ~ . , data = train_sel_log)

empty.model <- glm(Bonus ~ 1, data = train_sel_log)
#forward
for.model <- step(empty.model,
                  scope = list(lower = formula(empty.model),
                               upper = formula(full.model)),
                  direction = "forward", k = log(dim(train_sel_log)[1]))
#backward
back.model <- step(full.model,
                   scope = list(lower = formula(empty.model),
                                upper = formula(full.model)),
                   direction = "backward", k = log(dim(train_sel_log)[1]))
#concardance - backward selection
survival::concordance(back.model)
