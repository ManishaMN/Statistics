#install.packages('AmesHousing')

# 1. Loading libraries
library(AmesHousing)
library(ggplot2)
library(lmtest) # needed for the Durbin Watson function
library(gridExtra) # needed to put all graphs in the same plane
library(dplyr)

# 2. Loading data
ames <- make_ordinal_ames() 


# 3. Dividing the dataset into train and test components

ames_1 <- ames %>%
    mutate(id = row_number())

set.seed(123)

train <- ames_1 %>% 
    sample_frac(0.7)

test <- anti_join(ames_1, train, by = 'id')


# 4. Data Exploratory Analysis (DEA)

## From Previous Classes
ggplot(ames, aes(x=Sale_Price/1000)) + geom_histogram(fill="blue", aes(y = after_stat(!!str2lang("density"))),alpha=0.2) + geom_density(color="blue") + labs(x="Sale Prices (Thousands of US$)", title="Histogram of Sales Prices", y="Frequency") 
ggplot(ames, aes(x=log(Sale_Price/1000))) + geom_histogram(fill="orange", aes(y = after_stat(!!str2lang("density"))),alpha=0.2) + geom_density(color="orange") + labs(x="Log( Sale Prices (Thousands of US$) )", title="Histogram of log(Sales Prices)", y="Frequency") 

ggplot(ames, aes(sample=Sale_Price/1000)) + stat_qq() + stat_qq_line()
ggplot(ames, aes(sample=log(Sale_Price/1000))) + stat_qq() + stat_qq_line()

### According to the histograms and to the QQ-plots, the natural logarithm of the variable,
### log(Sale_Price/100), is closer to be normally distributed.

ggplot(data = train) + 
    geom_point(mapping = aes(x = Gr_Liv_Area, y = Sale_Price/1000)) +
    labs(y = "Sales Price (Thousands $)", x = "Greater Living Area (Sqft)")

ggplot(data = train, aes(y = Sale_Price/1000, x = `Exter_Qual`, fill = `Exter_Qual`)) +
    geom_boxplot() + 
    labs(y = "Sales Price (Thousands $)", x = "Exterior Quality Category") +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
    scale_fill_brewer(palette="Blues") + theme_classic() + coord_flip()

# NOTE: EVEN IF THE PREVIOUS GRAPHS CAN GIVE US A GOOD IDEA OF THE RELATIONSHIP
# BETWEEN CONTINUOUS AND CATEGORICAL VARIABLES, WE HAVE TO FORMALLY EMPLOY AN
# ANOVA TEST.


# 5. Defining the set of variables to build the model


## Corelation matrix

cor(train[, c('Sale_Price','Gr_Liv_Area','Year_Built','Total_Bsmt_SF','First_Flr_SF')])

# From model selection class, just adding the above aditional variables
train_1 <- train %>%
    select(Sale_Price, Year_Built, Total_Bsmt_SF, Lot_Area, Street, Bldg_Type, House_Style, Overall_Qual,
           Roof_Style, Central_Air, First_Flr_SF, Second_Flr_SF, Full_Bath,
           Half_Bath, Fireplaces, Garage_Area, Gr_Liv_Area, TotRms_AbvGrd) %>%
    replace(is.na(.), 0)



# 6. Testing for Multicolinearity

collin.test = lm((Sale_Price/1000)~., data=train_1)

vif(collin.test)


# 6.1 Eliminating Second_Flr_SF, and testing for Multicollinearity again

train_2 <- train %>%
    select(Sale_Price, Year_Built, Total_Bsmt_SF, Lot_Area, Street, Bldg_Type, House_Style, Overall_Qual,
           Roof_Style, Central_Air, First_Flr_SF, Full_Bath,
           Half_Bath, Fireplaces, Garage_Area, Gr_Liv_Area, TotRms_AbvGrd) %>%
    replace(is.na(.), 0)

collin.test = lm((Sale_Price/1000)~., data=train_2)

vif(collin.test)

# 6.2 Eliminating House_Style, and testing for Multicollinearity again

train_3 <- train %>%
    select(Sale_Price, Year_Built, Total_Bsmt_SF, Lot_Area, Street, Bldg_Type, Overall_Qual,
           Roof_Style, Central_Air, First_Flr_SF, Full_Bath,
           Half_Bath, Fireplaces, Garage_Area, Gr_Liv_Area, TotRms_AbvGrd) %>%
    replace(is.na(.), 0)

collin.test = lm((Sale_Price/1000)~., data=train_3)

vif(collin.test)

## There is no further variable to be eliminated from the data set


# 7. Automatic Selection Process

## Creating ful and empty models

full.model <- lm(Sale_Price ~ . , data = train_3) # A dot after the tilde tell R to include in the regression all other variables, except the dependent variable.
empty.model <- lm(Sale_Price ~ 1, data = train_3) # The one after the tilde tell R to build an intercept only model.

## Forward Selection
### AIC selection
forward_model <- step(empty.model,
                  scope = list(lower = empty.model,
                               upper = full.model),
                  direction = "forward", k = 2) 

## Backward Selection
### AIC selection
backward_model <- step(full.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "backward", k = 2) 

## Stepwise Selection
###AIC
stepwise_model <- step(empty.model,
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "both", k = 2) 

### Alpha Method
alpha.f=0.05
stepwise_model_2 <- step(empty.model,
                    scope = list(lower = empty.model,
                                 upper = full.model),
                    direction = "both", k = qchisq(alpha.f, 1, lower.tail = FALSE)) 

# MODEL WITH 12 EXPLANATORY VARIABLES:
# Step:  AIC=42606.56
# Sale_Price ~ Overall_Qual + Gr_Liv_Area + Year_Built + Bldg_Type + 
#    Fireplaces + Total_Bsmt_SF + Garage_Area + Lot_Area + Roof_Style + 
#    TotRms_AbvGrd + Full_Bath + Central_Air


# 8. Diagnostics

## 8.1 Ploting the model residuals

best_model <- lm(Sale_Price ~ Overall_Qual + Gr_Liv_Area + Year_Built + 
                     Bldg_Type + Fireplaces + Total_Bsmt_SF + Garage_Area + 
                     Lot_Area + Roof_Style + TotRms_AbvGrd + Full_Bath + 
                     Central_Air, data=train_3)
summary(best_model)

ggplot(best_model,aes(x=fitted(best_model),y=resid(best_model)))+geom_point(color="blue",size=3)+labs( x="Fitted Values", y="Residuals")


# 8.2 Variance correction model

best_model <- lm(log(Sale_Price) ~ Overall_Qual + Gr_Liv_Area + Year_Built + 
                     Bldg_Type + Fireplaces + Total_Bsmt_SF + Garage_Area + 
                     Lot_Area + Roof_Style + TotRms_AbvGrd + Full_Bath + 
                     Central_Air, data=train_3)
summary(best_model)

ggplot(best_model,aes(x=fitted(best_model),y=resid(best_model)))+geom_point(color="blue",size=3)+labs( x="Fitted Values", y="Residuals")


# This correction appears to reduce the variability of errors.


# 8.3 Checking for Normality

# 8.3a Graphical analysis
qqnorm(resid(best_model))
qqline(resid(best_model))

hist(resid(best_model))

# Residuals might not be normally distributed, but to be sure, we'll do
# the Shapiro-Wilk and Anderson-Darling Normality Tests

# Ho: Residuals are normally distributed

# 8.3b Shapiro-Wilk Test

shapiro.test(resid(best_model))

# Results:
# data:  resid(best_model)
# W = 0.84902, p-value < 2.2e-16

# Reject Ho.  

## Anderson-Darling Test (good for large data sets)
library(nortest)
ad.test(resid(best_model))

# Results:
# data:  resid(best_model)
# A = 32.955, p-value < 2.2e-16

# Reject Ho.


# 8.4 Detecting Heterocedasticity

## Correlation (Spearman) Test
# Ho: Variance of residuals is homocedastic
# Ha: Variance of residuals is heteroscedastic

cor.test(abs(resid(best_model)), fitted.values(best_model), method="spearman", exact=T)

# Results:
# data:  abs(resid(best_model)) and fitted.values(best_model)

# S = 1503605075, p-value = 0.0387
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho -0.04565437 
# At the 5% of significance we reject Ho.


## 8.5 Detecting Outliers and Influential Points

## 8.5a Detecting Outliers
n.index=seq(1,nrow(train_3))

a = ggplot(best_model,aes(x=n.index,y=rstandard(best_model)))+geom_point(color="red")+geom_line(y=-2)+geom_line(y=2)+labs(title = "Internal Studentized Residuals",x="Observation",y="Residuals")
a

b = ggplot(best_model,aes(x=n.index,y=rstudent(best_model)))+geom_point(color="orange")+geom_line(y=-2)+geom_line(y=2)+labs(title = "External Studentized Residuals",x="Observation",y="Residuals")
b

## 8.5b Detecting Influential Points
c = ggplot(best_model,aes(x=n.index,y=rstandard(best_model)))+geom_point(color="green")+geom_line(y=-2)+geom_line(y=2)+labs(title = "Internal Studentized Residuals",x="Observation",y="Residuals")
c

## 8.5c Cook's D
p <- best_model$rank # Number of explanatory variables from lm_FE regression
D.cut = 4/(nrow(train_3)-p-1)

d = ggplot(best_model,aes(x=n.index,y=cooks.distance(best_model)))+geom_point(color="blue")+geom_line(y=D.cut)+labs(title = "Cook's D",x="Observation",y="Cook's Distance")
d

## 8.5d Dffit
df.cut=2*(sqrt((p)/nrow(train_3)))

e =ggplot(best_model,aes(x=n.index,y=dffits(best_model)))+geom_point(color="gray")+geom_line(y=df.cut)+geom_line(y=-df.cut)+labs(title = "DFFITS",x="Observation",y="DFFITS")
e

## 8.5e Dbetas
db.cut=2/sqrt(nrow(train_3))

f =ggplot(best_model,aes(x=n.index,y=dfbetas(best_model)[,'Gr_Liv_Area']))+geom_point(color="yellow")+geom_line(y=db.cut)+geom_line(y=-db.cut)+labs(title = "DFBETA for WT",x="Observation",y="DFBETAS")
f


## 8.5f Hat
hat.cut=2*(p)/nrow(train_3)

h = ggplot(best_model,aes(x=n.index,y=hatvalues(best_model)))+geom_point(color="red")+geom_line(y=hat.cut)+labs(title = "Hat values",x="Observation",y="Hat Values")
h

## 8.5g Graphing all results
grid.arrange(b,c,d,e,f,h,ncol=2)


# 8.6 Detecting outliers/influencial observations

# creating a new data base with the results of all tests

rstd <- rstandard(best_model)
rstu <- rstudent(best_model)
cook <- cooks.distance(best_model)
dffit <- dffits(best_model)
dfbeta <- dfbetas(best_model)[,'Gr_Liv_Area']
hat <- hatvalues(best_model)
n <- n.index

train_4<-cbind(train_3, rstd, rstu, cook, dffit, dfbeta, hat, n)
head(train_4)


# subsetting outliers/influential points

x <- train_4[(abs(train_4$rstu) >= 3 & train_4$dffit >= df.cut), ]

r <- row.names(x)
r
# Observations 490, 1395, 1661 and 2033 are outliers and 
# influential observations that need to be further analized.

























