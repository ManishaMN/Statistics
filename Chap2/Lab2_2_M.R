data(AirPassengers)
head(AirPassengers)
install.packages("tseries")
install.packages("forecast")
library(tseries)
library(forecast)
library(dplyr)

#cycle() function from the "tseries" package to determine the seasonal cycle of the "AirPassengers" dataset.
cycle(AirPassengers)

air1 = data.frame(AirPassengers)
air2 = air1 %>% mutate(summer=ifelse(cycle(AirPassengers) %in% 6:8,1,0))
air2

#summer <- subset(air2, summer == 1)
#non_summer <- subset(air2, summer == 0)

#summer
#non_summer


library(ggplot2)
air2$summer <- as.factor(air2$summer)
ggplot(data = air2, aes(sample = AirPassengers, color = summer)) + stat_qq() + stat_qq_line()
#not normal

wilcox.test(AirPassengers ~ summer,data = air2 )
#Pvalue is 0.00588, reject null