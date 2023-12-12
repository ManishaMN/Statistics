# Analytic Foundations
# Lab 1

install.packages('UsingR') 
library(UsingR) 
data(normtemp)

# Question 1
# a
temp <- normtemp$temperature
min_temp <- min(temp)
max_temp <- max(temp)
mean_temp <- mean(temp)
sd_temp <- sd(temp)
median_temp <- median(temp)

min_temp
max_temp
round(mean_temp,3)
round(sd_temp,3)
median_temp

# b
qqnorm(temp)
# According to the QQ-plot, temperature appears to be normally distributed

#c 
boxplot(temp,
        main="Box-Plot of Temperature", 
        ylab="Degrees")
abline(h = 98.6)

# Yes, there are some outliers in the series.
# The median temperature is below 98.6 degrees. Such a temperature 
# is closer to the third quartile.


# Question 2
install.packages('AmesHousing')
library(AmesHousing)

# a
ames <- make_ordinal_ames() 

library(ggplot2)

ggplot(ames, aes(x=Sale_Price/1000)) + geom_histogram(fill="blue", aes(y = after_stat(!!str2lang("density"))),alpha=0.2) + geom_density(color="blue") + labs(x="Sale Prices (Thousands of US$)", title="Histogram of Sales Prices", y="Frequency") 

ggplot(ames, aes(x=log(Sale_Price/1000))) + geom_histogram(fill="orange", aes(y = after_stat(!!str2lang("density"))),alpha=0.2) + geom_density(color="orange") + labs(x="Log( Sale Prices (Thousands of US$) )", title="Histogram of Sales Prices", y="Frequency") 

ggplot(ames, aes(x=Gr_Liv_Area)) + geom_histogram(fill="green", aes(y = after_stat(!!str2lang("density"))),alpha=0.2) + geom_density(color="green") + labs(x="Log( Sale Prices (Thousands of US$) )", title="Histogram of Sales Prices", y="Frequency") 


# b

ggplot(ames, aes(sample=Sale_Price/1000)) + stat_qq() + stat_qq_line()

ggplot(ames, aes(sample=log(Sale_Price/1000))) + stat_qq() + stat_qq_line()

# According to the histograms and to the QQ-plots, the natural logarithm of the variable,
# log(Sale_Price/100), is closer to be normally distributed.


# Question 3

head(ames$Overall_Qual)
head(ames$Lot_Shape)
head(ames$Heating_QC)
head(ames$Lot_Area)

# Overall_Qual: Ordinal Variable
# Lot_Shape: Ordinal Variable
# Heating_QC: Ordinal Variable
# Lot_Area: Continuous, quantitative variable
















