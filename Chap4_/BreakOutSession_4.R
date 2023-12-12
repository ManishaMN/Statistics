#question1

#loading data it comes from a bike sharing company

df_bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")

str(df_bike)

#1.Explore the correlation between the numeric variables temp, atemp, hum, and windspeed with your target cnt.
#a.Calculate the correlation coefficient for each pair of variables

#creating row numbe and training data
#df_bike <- df_bike %>%
#  mutate(id = row_number())
#train <- df_bike %>% 
#  sample_frac(0.7)
#test <- anti_join(df_bike, train, by = 'id')

#a)checking correlation ( 0.4,0.4,-0.32,0.09)
cor(df_bike[, c("temp", "atemp", "hum", "windspeed", "cnt")])

#b)correlation scatter plot 
pairs(df_bike[, c("temp", "atemp", "hum", "windspeed", "cnt")])

#c) not very string relations

#2.Do any of the input variables seem like they might be useful predictors the number - temp
#of riders? Pick one variable that you think might work the best. ()
#2.a0Build a linear regression using that variable to predict the total number of hourly riders (cnt).
slr <- lm(cnt ~ temp, data=df_bike)
par(mfrow=c(2,2)) 
plot(slr)
#B) #What is the Rsq for the model you built? Interpret this number in a sentence
#R-squared value is 0.1638, which means that approximately 16.38% of the variance in the dependent variable(cnt) can be explained by the independent variable.
summary(slr)





#C)What is the value of the slope for the model you built? Interpret this number in a sentence
#for each unit increase in the independent variable (temp), the model predicts an average increase of 381.2949 in the dependent variable(cnt)
#381.2949
summary(slr)