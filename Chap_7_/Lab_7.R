# Lab 7

library(AppliedPredictiveModeling)
data(FuelEconomy)

str(cars2010)


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


# a forward with Pvalue


full.model <- lm(FE ~ ., data = cars2010)
empty.model <- lm(FE ~ 1, data = cars2010)

#other criteria P value selection

for.model <- step(empty.model, 
                  scope = list(lower = empty.model,
                               upper = full.model),
                  direction = "forward", k = qchisq(0.10, 1, lower.tail = FALSE))


#first FE ~ EngDispl, last ExhaustValvesPerCyl
#FE ~ EngDispl + CarlineClassDesc + NumCyl + DriveDesc + Transmission + IntakeValvePerCyl + VarValveLift + AirAspirationMethod + NumGears + TransLockup + TransCreeperGear + ExhaustValvesPerCyl

# b setpwise BIC criteria, 8 variables
step.model <- step(empty.model, 
                   scope = list(lower = empty.model,
                                upper = full.model),
                   direction = "both", k = log(nrow(cars2010)))



# FE ~ EngDispl + DriveDesc + CarlineClassDesc + NumCyl + ExhaustValvesPerCyl + NumGears + AirAspirationMethod + TransCreeperGear


