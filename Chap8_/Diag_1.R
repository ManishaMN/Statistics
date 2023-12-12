#libraries
library(carData)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(nortest)
library(MASS)
library(TSA)
library(lmtest)
library(car)
#relationship between each pair of variables i
pairs(Salaries)

#examine residuals
# misspecified - modeled linear function of explanatory
#there shoulddn't be any pattern
lm.model=lm(salary~.,data=Salaries)

ggplot(lm.model,aes(x=fitted(lm.model),y=resid(lm.model)))+
  geom_point(color="blue")+labs(x="Predicted Values",y="Residuals")

#variance 
# above residual plot
# variation about line should be constant


#normality
#Anderson-Darling - more weight to tails
set.seed(55402)
x=rnorm(1000,0,4)
ad.test(x)

#Shapiro-Wil - betetr for small data
shapiro.test(x)

# residuals not normal
#Box cox transformation
# Convert 'years' from factor to numeric
str(Salaries$yrs.service)
Salaries$years <- as.numeric(as.character(Salaries$years))

lm.var=lm(salary~yrs.service,data=Salaries)
x =boxcox(lm.var)
summary(x)
