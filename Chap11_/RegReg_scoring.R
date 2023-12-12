#load data
#split
install.packages("AmesHousing")
library(tidyverse)
library(dplyr)
library(AmesHousing)

#loading ames
ames <- make_ordinal_ames()
set.seed(123)
ames <- ames %>%
  mutate(id = row_number())
#
## converting to factors(other)
cars2010$NumCyl <- as.factor(cars2010$NumCyl)

# and training ameshousing ddta

train <- ames %>% 
  sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')


#select variables
train_reg <- train %>% 
  dplyr::select(Sale_Price, 
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
# train x, y
# model matrix
train_x <- model.matrix(Sale_Price ~ ., data = train_reg)[, -1]
train_y <- train_reg$Sale_Price

# repaeat for test
test_reg <- test %>% 
  dplyr::select(Sale_Price, 
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

test_x <- model.matrix(Sale_Price ~ ., data = test_reg)[, -1]
test_y <- test_reg$Sale_Price


#ridge 
library(glmnet)

ames_ridge <- glmnet(x = train_x,  y = train_y,  alpha = 0)

plot(ames_ridge, xvar = "lambda")

#lasso
ames_lasso <- glmnet(x = train_x,  y = train_y,  alpha = 1)

plot(ames_lasso, xvar = "lambda")

#elastic
ames_en <- glmnet(x = train_x,  y = train_y,  alpha = 0.5)

plot(ames_en, xvar = "lambda")

#cross validation - lasso
ames_lasso_cv <- cv.glmnet(x = train_x,  y = train_y,  alpha = 1)

plot(ames_lasso_cv)

#obtain lamda for mse and lse
ames_lasso_cv$lambda.min 
ames_lasso_cv$lambda.1se

#impact on coeff - lasso - for lse mse penality
plot(ames_lasso, xvar = "lambda")
abline(v = log(ames_lasso_cv$lambda.1se), col = "red", lty = "dashed")
abline(v = log(ames_lasso_cv$lambda.min), col = "black", lty = "dashed")


#To investigate which variables are important at a  λvalue, we can view the coefficients using the coef function.
coef(ames_lasso, s = c(ames_lasso_cv$lambda.min, ames_lasso_cv$lambda.1se))


#get predictions
test$pred_lm <- predict(ames_lm, newdata = test)

head(test$pred_lm)

test_reg$pred_lasso <- predict(ames_lasso, s = ames_lasso_cv$lambda.1se, newx = test_x)

head(test_reg$pred_lasso)

#calculating MAPE
test_reg %>%
  mutate(lasso_APE = 100*abs((Sale_Price - pred_lasso)/Sale_Price)) %>%
  dplyr::summarise(MAPE_lasso = mean(lasso_APE))

#MAE
test_reg %>%
  mutate(lasso_AE = abs(Sale_Price - pred_lasso)) %>%
  dplyr::summarise(MAE_lasso = mean(lasso_AE))

#MAE
valid.fit=predict(lm.4,newdata = cars2011.1)
MAE=mean(abs(cars2011.1$FE-valid.fit))