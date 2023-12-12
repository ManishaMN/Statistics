library(ggplot2)

bike <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/bike.csv')
str(bike)
table(bike$season)

ggplot( bike, aes(x= cnt)) +
  geom_histogram(fill="blue") +
  labs( x = "Bike Rentals" , title ="Histogram of Bike Rentals", y = "Frequency")
  
  
summary(bike$cnt)
sd(bike$cnt)
quantile( bike$cnt, probs= c(0.1,0.4,0.8))


ggplot(bike, aes(x=cnt)) + 
  geom_histogram(fill="red",binwidth = 50)+
  labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency")

ggplot(bike, aes(x=cnt)) + 
  geom_histogram(fill="purple",binwidth = 100)+
  labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency")

ggplot(bike, aes(x=cnt)) + 
  geom_histogram(fill="orange",binwidth = 250)+
  labs(x="Bike Rentals", title="Histogram of Bike Rentals", y="Frequency")

ggplot(bike, aes(x=cnt))+
  geom_histogram(aes(y= after_stat(!!str2lang("density"))), alpha = 0.2)+
  geom_density()+
  labs(x="Bike Rentals", title = "Histogram of Bike Rentals")

ggplot(bike, aes(sample = cnt)) +
  stat_qq() +
  stat_qq_line()

ggplot(bike, aes(x=factor(season), y=cnt, fill = factor(season))) +
  geom_boxplot()+
  scale_x_discrete(labels = c("spring","summer", "Fall", "Winter")) +
  labs(x="Season",y="Bike Rentals",fill="Season")
  