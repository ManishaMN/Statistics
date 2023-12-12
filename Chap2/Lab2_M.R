library(UsingR)

data(normtemp)
str(normtemp)

t.test(normtemp$temperature, mu=98.6)

# it is not the mean, P value is 2.411e-07, It is less the alpha. 
#we reject the null hypothesis and say mean is not equal to 98.6
#95 percent confidence interval: 98.12200 98.37646, it represents range of values within which you are 95% confident that true population mean exists.
#given 98.6 falls outside this 95% CI, it means given mean value not equal to true population mean

df1<-normtemp[normtemp$gender == 1,]
df2<-normtemp[normtemp$gender == 2,]
t.test(df1$temperature, mu=98.6)
t.test(df2$temperature, mu=98.6)

#no it will change with gender

#check normality -yes
library(ggplot2)
normtemp$gender <- as.factor(normtemp$gender)
ggplot(data = normtemp, aes(sample = temperature, color = gender)) + stat_qq() + stat_qq_line()

var.test(temperature ~ gender, data = normtemp) 
#P value 0.6211, so variance are equal not rejecting null hypothesis
# Variance are equal p=0.6211

t.test(temperature ~ gender, data = normtemp, var.equal = TRUE)

#true difference in means between group 1 and group 2 is not equal to 0
