##2 way ANova

#library
library(tidyverse)
library(dplyr)
library(AmesHousing)
#loading ames
ames <- make_ordinal_ames()
set.seed(123)
ames <- ames %>%
  mutate(id = row_number())

#training ameshousing ddta

train <- ames %>% 
  sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')


#Data exploration******************
CA_heat <- train %>% 
  group_by(Heating_QC, Central_Air) %>%
  dplyr::summarise(mean = mean(Sale_Price), sd = sd(Sale_Price), max = max(Sale_Price), min = min(Sale_Price))
#bar chart 
#sideby side bar chart ******
#We need to statistically be sure of any differences that exist between average sale price in categories.
ggplot(data = CA_heat, aes(x = Heating_QC, y = mean/1000, fill = Central_Air)) +geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "Sales Price (Thousands $)", x = "Heating Quality Category") +scale_fill_brewer(palette = "Paired") + theme_minimal()


#ANOVA aov
#Inference: Based on these tests, at least one category in each variable is statistically different than the rest.
ames_aov2 <- aov(Sale_Price ~ Heating_QC + Central_Air, data = train)

summary(ames_aov2)

#POst Hoc, TukeyHSD
tukey.ames2 <- TukeyHSD(ames_aov2)
print(tukey.ames2)
plot(tukey.ames2, las = 1)

#interaction
# not significant
ames_aov_int <- aov(Sale_Price ~ Heating_QC*Central_Air, data = train)

summary(ames_aov_int)

#slicing( Post Hoc for interactiona are overwhelming)***************
# one way anova for HQ in each level of central air
#there are statistical differences in average sale price 
#across heating quality within homes that have central air as well as those that don’t have central air.
CA_aov <- train %>% 
  group_by(Central_Air) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(Sale_Price ~ Heating_QC, data = .x))))

print(CA_aov$aov)

#Assumptions
#levene test equal variance
library(car)

leveneTest(Sale_Price ~ Heating_QC*Central_Air, data = train) # no equla variance

# normality QQplot
plot(ames_aov_int, 2) #not normal
# shapiro test
ames_res <- residuals(ames_aov_int)
shapiro.test(x = ames_res)


#Unfortunately, the Kruskal-Wallis approach is not applicable to  n-Way ANOVA where  n>1
.

#randomzied block design

#bulb wt, fertlizer, plot
#explore dat
block <- read_csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/garlic_block.csv")
head(block, 20)

#blocking - same as 2 way ANOVA
block_aov <- aov(BulbWt ~ factor(Fertilizer) + factor(Sector), data = block)
summary(block_aov)
#both the sector (block) and fertilizer variables are significant in the model at the 0.05 level. 

#block post Hoc Tukey HSD
tukey.block <- TukeyHSD(block_aov)
print(tukey.block)
plot(tukey.block, las = 1)
