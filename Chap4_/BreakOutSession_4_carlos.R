#question1

#loading data it comes from a bike sharing company

df_bike <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv")

str(df_bike)

#1.Explore the correlation between the numeric variables temp, atemp, hum, and windspeed with your target cnt.
#a.Calculate the correlation coefficient for each pair of variables

#creating row number and training data

#df_bike <- df_bike %>%
#    mutate(id = row_number())



#train <- df_bike %>% 
#    sample_frac(0.7)

#test <- anti_join(df_bike, train, by = 'id')


# a

#checking correlation
cor(df_bike[, c("cnt", "temp", "atemp", "hum", "windspeed")])
# corr of cnt with: temp:0.40, atemp:0.40, hum:-0.32, windspeed:0.09

# b

#correlation scatter plot
pairs(df_bike[, c("cnt", "temp", "atemp", "hum", "windspeed")])

# c
# The stronger linear association is with either the variable temp (0.404)
# or the variable atemp (0.400). The slightly highest is temp (0.404). 


#2.Do any of the input variables seem like they might be useful predictors the number 
#  of riders? Pick one variable that you think might work the best.

# a   Build a linear regression using that variable to predict the total number 
#     of hourly riders (cnt).

slr <- lm(cnt ~ temp, data=df_bike)
par(mfrow=c(2,2)) 
plot(slr)
summary(slr)

# b  
#What is the Rsq for the model you built? Interpret this number in a sentence

# R_square  = 0.1638
# the proportion of changes in bike rides (cnt) are 16.38% explained by changes
# temperature (temp).


# c
#What is the value of the slope for the model you built? Interpret this 
# number in a sentence

# B1 = 381.29
# Increasing temperature (temp) by one unit makes the number of bike rides 
# increase by 381.29.


