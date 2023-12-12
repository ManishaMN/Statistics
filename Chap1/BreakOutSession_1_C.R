# Break Out Sesion 1

bike <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv')

# This repository contains all the data to be used in the Summer II.
# Go to repository
# Select the data set (in csv format) you want to use
# Press "Raw"
# The internet address that appears at the top is the one to use in the "read.csv" command

str(bike)

table(bike$season)


library(ggplot2)

seas<-data.frame(table(bike$season))
ggplot(seas,aes(x=factor(Var1),y=Freq)) +
    geom_bar(stat = "identity") + labs(x="Season")+
    scale_x_discrete(labels=c("Spring", "Summer","Fall","Winter"))


## HISTOGRAMS

# Original Histogram
ggplot(bike, aes(x=cnt)) + geom_histogram(fill="blue") + labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency") 

# Changing the number of bins (30 does not work)
ggplot(bike, aes(x=cnt)) + geom_histogram(fill="blue") + labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency") + stat_bin(bins = 100)

summary(bike$cnt)

sd(bike$cnt)

quantile(bike$cnt,probs = c(0.10,0.40,0.80))


# Changing the number of bins and the histogram colors:

ggplot(bike, aes(x=cnt)) + geom_histogram(fill="red",binwidth = 50)+labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency")

ggplot(bike, aes(x=cnt)) + geom_histogram(fill="purple",binwidth = 100)+labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency")

ggplot(bike, aes(x=cnt)) + geom_histogram(fill="orange",binwidth = 250)+labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency")


# Overlaying the histogram with a kernel density

ggplot(bike, aes(x=cnt)) + geom_histogram(aes(y = after_stat(!!str2lang("density"))),alpha=0.2) + geom_density()+
    labs(x="Bike Rentals", title="Histogram of Bike Rentals")  

## QQ PLOTS

ggplot(bike, aes(sample = cnt)) + stat_qq() + stat_qq_line()
# It is right skewed!


## ASSOCIATIONS

# Let’s take a look at the number of rentals within each season. 
# Since we have one categorical variable (season) and one quanTitative
# variable (count), we will create side-by-side boxplots.

ggplot(bike, aes(x=factor(season), y=cnt, fill=factor(season))) + geom_boxplot() + scale_x_discrete(labels=c("Spring", "Summer","Fall","Winter")) + labs(x="Season",y="Bike Rentals",fill="Season")

