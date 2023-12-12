#boxplot with reference line***

library(UsingR)

data(normtemp)
str(normtemp)

minimum_temp <- min(normtemp$temperature)
minimum_temp

max_temp <- max(normtemp$temperature)
max_temp

mean_temp <- mean(normtemp$temperature)
mean_temp

sd_temp <- sd(normtemp$temperature)
sd_temp


ggplot(normtemp, aes(x=temperature) )+
  geom_histogram(aes(y=..density..), alpha =0.5 ) +
  geom_density( alpha = 0.2)+
  labs(x = "Temperature", y = "Density", title = "Histogram and Density Curve")
  
ggplot(data = normtemp, aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line()

ggplot(normtemp, aes( y=temperature)) +
  geom_boxplot()+
  labs(y="Temperature")+
  geom_hline(yintercept=98.6)


  