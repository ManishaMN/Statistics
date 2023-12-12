# Breakout Session 3

# Question 1

temp <- tempfile()
download.file("https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip",temp)
#day <- read.csv(unz(temp, "day.csv"))
hour <- read.csv(unz(temp, "hour.csv"))
unlink(temp)
rm(temp)

# Alternatively: download the zip file, open it, save the files it contains i.e. in C:/BOS_Data/
# day <- read.csv('C:/BOS_Data/day.csv')
# hour <- read.csv('C:/BOS_Data/hour.csv')


library(ggplot2)

hour$weathersit <- as.factor(hour$weathersit)

ggplot(hour) + geom_bar(aes(x=weathersit,y= cnt), position = "dodge", stat = "summary", fun = "mean") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplot(hour, aes(y=cnt, x=weathersit, fill=weathersit)) + geom_boxplot()

hr_lm <- lm(cnt ~ weathersit, data = hour)
anova(hr_lm)

#i)  Normality #not normal
par(mfrow=c(2,2))
plot(hr_lm)
par(mfrow=c(1,1))
#ii) Fligner-Killeen ( checking to see diff in varieance), variance are different ( good to drop level)
fligner.test(cnt ~ weathersit, data = hour)
#iii) significant test as normality failed 
#Kruskal Wallis/Wilcoxon ( p value small)
kruskal.test(cnt ~ weathersit, data = hour)



# Question 2 ( there is difference)
hour$season <- as.factor(hour$season)
ggplot(hour) + geom_bar(aes(x=season,y= cnt), position = "dodge", stat = "summary", fun = "mean") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplot(hour, aes(y=cnt, x=season, fill=season)) + geom_boxplot()

#anova P value is small ( there is statistical significant difference)
season_lm <- lm(cnt ~ season, data = hour)
anova(season_lm)

#i)  Normality not very normal
par(mfrow=c(2,2))
plot(season_lm)
par(mfrow=c(1,1))
#ii) Fligner-Killeen  checking to see diff in varieance), variance are different ( good to drop level)
fligner.test(cnt ~ season, data = hour)
#iii) significant test as normality failed
#Kruskal Wallis/Wilcoxon ( p value small)
kruskal.test(cnt ~ season, data = hour)

# Question 3 ( there is difference)
hour$holiday <- as.factor(hour$holiday)
ggplot(hour) + geom_bar(aes(x=holiday,y= cnt), position = "dodge", stat = "summary", fun = "mean") + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplot(hour, aes(y=cnt, x=holiday, fill=holiday)) + geom_boxplot()
#ANOVA P value is small ( there is statistical significant difference)
holiday_lm <- lm(cnt ~ holiday, data = hour)
anova(holiday_lm)
#i)  Normality Normality not very normal
par(mfrow=c(2,2))
plot(holiday_lm)
par(mfrow=c(1,1))
#ii) Fligner-Killeen  variance are different
fligner.test(cnt ~ holiday, data = hour)
#iii) Kruskal Wallis/Wilcoxon ( p value small)
kruskal.test(cnt ~ holiday, data = hour)



