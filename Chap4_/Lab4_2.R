#Question 3 (  MinnTemp data set)

df_MT <- read.table("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/minntemp.csv")
str(df_MT)

#3_1regression

slr <- lm(Temp ~ Time, data=df_MT)

residuals <- residuals(slr)
# Plot a histogram of the residuals
hist(residuals, breaks = "FD", main = "Histogram of Residuals", xlab = "Residuals")

#3_2
#assumptions ( They are  almost normally distributed)
par(mfrow=c(2,2)) 
plot(slr)

#3_3 No, P value is 0.07271.  
summary(slr)

#verifying corr test ( corr is 0.06 and P value is 0.132)
cor.test(df_MT$Temp,df_MT$Time)
