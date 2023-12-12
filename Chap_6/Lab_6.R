# Lab6

library(AppliedPredictiveModeling)
data(FuelEconomy)
str(cars2010)

# 1
#use if statement with apply function instead of below 
cars2010$NumCyl <- as.factor(cars2010$NumCyl)
cars2010$Transmission <- as.factor(cars2010$Transmission)
cars2010$AirAspirationMethod <- as.factor(cars2010$AirAspirationMethod)
cars2010$NumGears <- as.factor(cars2010$NumGears)
cars2010$TransLockup <- as.factor(cars2010$TransLockup)
cars2010$TransCreeperGear <- as.factor(cars2010$TransCreeperGear)
cars2010$DriveDesc <- as.factor(cars2010$DriveDesc)
cars2010$IntakeValvePerCyl <- as.factor(cars2010$IntakeValvePerCyl)
cars2010$ExhaustValvesPerCyl <- as.factor(cars2010$ExhaustValvesPerCyl)
cars2010$CarlineClassDesc <- as.factor(cars2010$CarlineClassDesc)
cars2010$VarValveTiming <- as.factor(cars2010$VarValveTiming)
cars2010$VarValveLift <- as.factor(cars2010$VarValveLift)

class(cars2010$FE)
class(cars2010$EngDispl)

#1a global F test
 
cars2010_lma <- lm(FE~NumCyl+Transmission+AirAspirationMethod+NumGears+TransLockup+TransCreeperGear
                   +DriveDesc+IntakeValvePerCyl+ExhaustValvesPerCyl+CarlineClassDesc+VarValveTiming
                   +VarValveLift+EngDispl , data=cars2010) 
summary(cars2010_lma)
#1bRsquare 0.8333 ( amount can be explained)

#b.a 
car::Anova(cars2010_lma)
#b.b VarValveTiming, IntakeValvePerCyl ,TransCreeperGear


#c.a VarValveTiming


#Note that Adjusted_R_square goes down when we keep removing
# variables. This is because each variable even if it is not
# significant, it adds some value to the prediction of a
# variable.  From p-value we will know the statistic significance,
# and from R_squared we will know the predictive power of a model.

cars2010_lmb <- lm(FE~NumCyl+Transmission+AirAspirationMethod+NumGears+TransLockup+TransCreeperGear
                   +DriveDesc+IntakeValvePerCyl+ExhaustValvesPerCyl+CarlineClassDesc
                   +VarValveLift+EngDispl , data=cars2010) 
summary(cars2010_lmb)

#Did the p-value for the model change notably? p-value: < 2.2e-16,p-value: < 2.2e-16(no)
#  b. Did the R-square and adjusted R-square values change notably?
#Multiple R-squared:  0.8333,	Adjusted R-squared:  0.8247 ,
#Multiple R-squared:  0.8333,	Adjusted R-squared:  0.8246 
#R square didn't change, but Adj R square reduce
#  c. Did the p-values on other variables change notably? no they didn't
car::Anova(cars2010_lmb)

#2 repetition - IntakeValvePerCyl 
cars2010_lmc <- lm(FE~NumCyl+Transmission+AirAspirationMethod+NumGears+TransLockup+TransCreeperGear
                   +DriveDesc+ExhaustValvesPerCyl+CarlineClassDesc
                   +VarValveLift+EngDispl , data=cars2010) 
summary(cars2010_lmc)
car::Anova(cars2010_lmc)
#3 repetition Translockup
cars2010_lmd <- lm(FE~NumCyl+Transmission+AirAspirationMethod+NumGears+TransCreeperGear
                   +DriveDesc+ExhaustValvesPerCyl+CarlineClassDesc
                   +VarValveLift+EngDispl , data=cars2010) 
summary(cars2010_lmd)
car::Anova(cars2010_lmd)
#4 repetition VarValveLift 
cars2010_lme <- lm(FE~NumCyl+Transmission+AirAspirationMethod+NumGears+TransCreeperGear
                   +DriveDesc+ExhaustValvesPerCyl+CarlineClassDesc
                   +EngDispl , data=cars2010) 
summary(cars2010_lme)
car::Anova(cars2010_lme)

#4aDid the R-square and adjusted R-square values change notably?
#4b. How many variables did you have left that were significant at the 0.008 level? 9 variables