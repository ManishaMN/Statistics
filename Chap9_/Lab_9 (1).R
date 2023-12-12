# Lab 9

library(AppliedPredictiveModeling)
data(FuelEconomy)


# Instruction: Do not transform variables to factors in this lab.

n.index=seq(1,nrow(cars2010))

lm_FE <- lm(FE~EngDispl+Transmission+AirAspirationMethod+TransLockup+
                TransCreeperGear+DriveDesc+IntakeValvePerCyl+
                CarlineClassDesc+VarValveLift, data=cars2010)


# Ho: There is no residual autocorrelation
# Ha: There is residual autocorrelation

library(lmtest)
dwtest(lm_FE,alternative="greater")
# DW = 1.3354
# Reject H0.  There is serial correlation in the data.


# Cook's D

##Cook's D
n = nrow(cars2010)
p = 9

D.cut=4/(nrow(cars2010)-9-1)

d =ggplot(lm_FE,aes(x=n.index,y=cooks.distance(lm_FE)))+geom_point(color="orange")+geom_line(y=D.cut)+labs(title = "Cook's D",x="Observation",y="Cook's Distance")
d

y = cooks.distance(lm_FE)
y
max(y)

# 0.06


## Studentized Residuals

b = ggplot(lm_FE,aes(x=n.index,y=rstudent(lm_FE)))+geom_point(color="orange")+geom_line(y=-3)+geom_line(y=3)+labs(title = "External Studentized Residuals",x="Observation",y="Residuals")
b

y1=rstudent(lm_FE)

x = 0
for (i in y1) {
    if (abs(i) >= 3) {
        x = x+1
    }
}
x
# x = 18



















