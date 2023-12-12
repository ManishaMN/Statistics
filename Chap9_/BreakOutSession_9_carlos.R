
# Loading libraries and data

library(car)
library(ggplot2)

data(mtcars)


# Model 1 selected:
lm_m1 <- lm(mpg ~ wt + cyl + hp + wt:cyl + cyl:hp, data=mtcars)

# Model 2 selected:
lm_m2 <- lm(mpg ~ cyl + hp + wt + I(wt^2) + cyl*hp, data=mtcars)


# TESTING FOR MULTICOLINEARITY
mtcars_1 <- mtcars[,c(1,2,4,6)]

cor(mtcars_1)

lm_m <- lm(mpg~., data=mtcars_1)

vif(lm_m)

# According to the VIF, we do not remove any variable to run the 
# automatic selection process.


# TESTING FOR AUTOCORRELATION
# Ho: Residuals do not show any autocorrelation
# Ha: Residuals show autocorrelation

dwtest(lm_m2,alternative="greater")

# DW = 2.3252, p-value = 0.7138
# We do not reject Ho


# PLOT FOR OUTLIERS

library(ggplot2)

##Plots of outliers
n.index=seq(1,nrow(mtcars))

# Standardized
a = ggplot(lm_m2,aes(x=n.index,y=rstandard(lm_m2)))+geom_point(color="orange")+geom_line(y=-1.5)+geom_line(y=1.5)+labs(title = "External Studentized Residuals",x="Observation",y="Residuals")
a

# Studentized
b = ggplot(lm_m2,aes(x=n.index,y=rstudent(lm_m2)))+geom_point(color="orange")+geom_line(y=-1.5)+geom_line(y=1.5)+labs(title = "External Studentized Residuals",x="Observation",y="Residuals")
b

# Observations 17, 18, 20, 21, 25 could be outliers.


##Cook's D
# n = nrow(mtcars)
# p = 5

D.cut=4/(nrow(mtcars)-5-1)

d =ggplot(lm_m2,aes(x=n.index,y=cooks.distance(lm_m2)))+geom_point(color="orange")+geom_line(y=D.cut)+labs(title = "Cook's D",x="Observation",y="Cook's Distance")
d

# Observations 17 and 29 might be influential points.


##Dffit
df.cut=2*(sqrt((3+1)/nrow(mtcars)))

e =ggplot(lm_m2,aes(x=n.index,y=dffits(lm_m2)))+geom_point(color="orange")+geom_line(y=df.cut)+geom_line(y=-df.cut)+labs(title = "DFFITS",x="Observation",y="DFFITS")
e

##Dbetas
db.cut=2/sqrt(nrow(mtcars))

f =ggplot(lm_m2,aes(x=n.index,y=dfbetas(lm.model)[,'Climb']))+geom_point(color="orange")+geom_line(y=db.cut)+geom_line(y=-db.cut)+labs(title = "DFBETA for Climb",x="Observation",y="DFBETAS")
f

g =ggplot(lm.model,aes(x=n.index,y=dfbetas(lm.model)[,'Distance']))+geom_point(color="orange")+geom_line(y=db.cut)+geom_line(y=-db.cut)+labs(title = "DFBETA for Distance",x="Observation",y="DFBETAS")
g
















