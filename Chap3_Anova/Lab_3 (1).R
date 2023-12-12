# Lab 3

# Question 1 and 2

#Garlic dataset
garlic<-read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/garlic.csv')
head(garlic)

# a
library(ggplot2)
garlic$Fertilizer <- as.factor(garlic$Fertilizer)
ggplot(garlic) + geom_bar(aes(x=Fertilizer,y= BulbWt), position = "dodge", stat = "summary", fun = "mean") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplot(garlic, aes(y=BulbWt, x=Fertilizer, fill=Fertilizer)) + geom_boxplot()

#Ho: No effect of fertilizer on bulb weight
#Ha: Effect of fertilizer on bulb weight

fertilizer_lm <- lm(BulbWt ~ Fertilizer, data = garlic)
anova(fertilizer_lm)

# According to the ANOVA results, we reject H0, so it appears that there is 
# a significant relationship between both variables.

#i)  Normality
par(mfrow=c(1,1))
plot(fertilizer_lm)

# Normality is not rejected according to the QQ-plots
# Residuals are normally distributed.


#ii.1) Fligner-Killeen
#Ho: Variances are equal
#Ha: Variances are different

fligner.test(BulbWt ~ Fertilizer, data = garlic) 
# Result: Null hypothesis is not rejected.  So, variances are equal.
# We could get rid off the category with low values

#ii.2) Levene's Test
#Ho: Variances are equal
#Ha: Variances are different

library(car)
library(stats)
leveneTest(BulbWt ~ Fertilizer, data = garlic) 

# Result: Null hypothesis is not rejected.  So, variances are equal.


# According to the ANOVA results, we reject H0, so it appears that there is 
# a significant relationship between both variables.


# b
combn(4,2)
# The answer is 6

# c
# Experiment Wise Error Rate
ewer <- 1 - (1-0.05)^6
ewer

# d
# Tukey's Test
ames_aov <- aov(BulbWt ~ Fertilizer, data = garlic)
tukey.ames <- TukeyHSD(ames_aov)
print(tukey.ames)

#Ho: F1 = F2
#Ha: F1 != F2

# According to the Tukey Test, we reject the Null Hypothesis in the pairwise
# Comparisons 4-1 and 4-3.

#Question 3
bottle <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bottle.csv")
head(bottle)

bottle$Line <- as.factor(bottle$Line)
ggplot(bottle) + geom_bar(aes(x=Line,y= Units), position = "dodge", stat = "summary", fun = "mean") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplot(bottle, aes(y=Units, x=Line, fill=Line)) + geom_boxplot()


#Ho: No effect of fertilizer on bulb weight
#Ha: Effect of fertilizer on bulb weight

line_lm <- lm(Units ~ Line, data = bottle)
anova(line_lm)

# According to the ANOVA results, we reject H0, so it appears that there is 
# a significant relationship between both variables.

#i)  Normality
par(mfrow=c(1,1))
plot(line_lm)

# Normality is not rejected according to the QQ-plots
# Residuals are normally distributed.


#ii.1) Fligner-Killeen
#Ho: Variances are equal
#Ha: Variances are different

fligner.test(Units ~ Line, data = bottle) 
# Result: Null hypothesis is not rejected.  So, variances are equal.


#ii.2) Levene's Test
#Ho: Variances are equal
#Ha: Variances are different

library(car)
library(stats)
leveneTest(Units ~ Line, data = bottle) 

# Result: Null hypothesis is not rejected.  So, variances are equal.


# According to the ANOVA results, we reject H0, so it appears that there is 
# a significant relationship between both variables.

# Tukey's Test
ames_aov <- aov(Units ~ Line, data = bottle)
tukey.ames <- TukeyHSD(ames_aov)
print(tukey.ames)

#Ho: F1 = F2
#Ha: F1 != F2

# According to the Tukey Test, we reject the Null Hypothesis in the pairwise
# Comparisons 3-1 and 3-2. In other words, the 3rd assembly line is different. 



#Question 4
trials <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/trials.csv")
head(trials)

trials$Treatment <- as.factor(trials$Treatment)
ggplot(trials) + geom_bar(aes(x=Treatment,y= BPChange), position = "dodge", stat = "summary", fun = "mean") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplot(trial, aes(y=BPChange, x=Treatment, fill=Treatment)) + geom_boxplot()


#Ho: No effect of treatments on Blood Pressure Change
#Ha: Effect of treatments on Blood Pressure Change

treatment_lm <- lm(BPChange ~ Treatment, data = trials)
anova(treatment_lm)

# According to the ANOVA results, we reject H0, so it appears that there is 
# a significant relationship between both variables.

#i)  Normality
par(mfrow=c(1,1))
plot(treatment_lm)

# Normality is not rejected according to the QQ-plots
# Residuals are normally distributed.


#ii.1) Fligner-Killeen
#Ho: Variances are equal
#Ha: Variances are different


oneway.test(BPChange ~ Treatment, data = trials, var.equal = FALSE)

# Differences are statistically significant.  Reject Ho
# According to the ANOVA results, we reject H0, so it appears that there is 
# a significant relationship between both variables.

# Tukey's Test
ames_aov <- aov(BPChange ~ Treatment, data = trials)
tukey.ames <- TukeyHSD(ames_aov)
print(tukey.ames)

#Ho: F1 = F2
#Ha: F1 != F2

# According to the Tukey Test, we reject the Null Hypothesis in all pairwise
# Comparisons. 


#library(DescTools)

#DunnettTest(x = trials$BPChange, g = trials$Treatment, control = 'Typical')

