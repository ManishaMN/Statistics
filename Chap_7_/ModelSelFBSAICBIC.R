#model slection
#forward Back Step

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
train_se1 <- ames %>% 
  sample_frac(0.7)
test <- anti_join(ames, train_se1, by = 'id')

# empty full model
full.model <- lm(Sale_Price ~ . , data = train_se1)
empty.model <- lm(Sale_Price ~ 1, data = train_se1)

#forward - AIC K =2
for.model <- step(empty.model,
                  scope = list(lower = empty.model,
                               upper = full.model),
                  direction = "forward", k = 2) 
#forward - BIC K log(nrow(train_sel)
#forward - Pvalue alpha
# k = log(n) for BIC selection
for.model2 <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "forward", k = log(nrow(train_se1))) # k = qchisq(alpha, 1, lower.tail = FALSE) for p-value with alpha selection
alpha.f=0.05
for.model3 <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "forward", k = qchisq(alpha.f, 1, lower.tail = FALSE)) 

#backward - AIC
back.model <- step(full.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "backward", k = 2) 
#backward - BIC , P value alpah

# k = log(n) for BIC selection
back.model2 <- step(full.model,
                    scope = list(lower = empty.model,
                                 upper = full.model),
                    direction = "backward", k = log(nrow(train_sel))) # k = qchisq(alpha, 1, lower.tail = FALSE) for p-value with alpha selection
alpha.f=0.05
back.model3 <- step(full.model,
                    scope = list(lower = empty.model,
                                 upper = full.model),
                    direction = "backward", k = qchisq(alpha.f, 1, lower.tail = FALSE)) 

#step stepwise
# k = 2 for AIC selection
step.model <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "both", k = 2) 