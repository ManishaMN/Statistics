library(lmtest) # needed for the Durbin Watson function
library(gridExtra) # needed to put all graphs in the same plane
library(dplyr)
library(ggplot2)


safety <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv')

summary(safety)

#1.a
#continuous, 
#nominal , 
#ordinal - Unsafe,type,Region, Weight,Size
unique(safety$Weight)
#1.b 
table(safety$Region, safety$Unsafe)
ggplot(data = safety) +
  geom_bar(mapping = aes(x =Unsafe , fill = Region))

library(gmodels)
CrossTable(safety$Region, safety$Unsafe)
#1.b.a.42.8
#b.b.69.7
#b.c. distribution of safe doesn't change with region: NUll
#Mantel-Haenszel Chi-Square Tes
library(vcdExtra)
CMHtest(table(safety$Region, safety$Unsafe))$table[1,]
#distribution of safe doesn't change with region ( p value is 0.06), no association

#b.d. ( Asia, N), 0, 1
library(DescTools)
flip <- OddsRatio(table(safety$Region, safety$Unsafe)) #0.435
flip
1/flip #2.3
#0.43 = Asia + safe, NA not safe
#2.3 = NA + safe , Asia not safe


#1.cMantel-Haenszel Chi-Square Tes
unique(safety$Size)
unique(safety$Unsafe)
safety$Size <- as.factor(safety$Size)
summary(safety$Size)
CMHtest(table(safety$Size, safety$Unsafe))$table[1,] #1.409484e-07 , rejecting null there is association
ggplot(data = safety) +
  geom_bar(mapping = aes(x =Unsafe , fill = Size))




#1.c.b association

cor.test(x = as.numeric(ordered(safety$Unsafe)), 
         y = as.numeric(ordered(safety$Size)), 
         method = "spearman")
unique(as.numeric(ordered(safety$Unsafe)))
# size 3 to 1, safety 0 to 1: safety is decreasing with decrease size








