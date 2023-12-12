drugdose <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/drug.csv")

str(drugdose)
#below graph is not right we need to summarise by mean
#sidebysidebarchart ( diff disearses behaving inn diff way for dose change , same disease have diff BP levels with diff doses)
#looks like disease A has decereased BP with dose increase
ggplot(data = drugdose, aes(x = factor(DrugDose), y = BloodP, fill = factor(Disease))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.9)  +
  ylim(0,150) +
  #geom_text( fontface = "bold", vjust = 1.5,
  #          position = position_dodge(.9), size = 4) +
  labs(x = "\n Drug dose", y = "BloodP\n", title = "\n Drug dose on BP for diff disease \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="red", size = 12),
        axis.title.y = element_text(face="bold", colour="red", size = 12),
        legend.title = element_text(face="bold", size = 10))



#2 way anova with interactions , P value is 3.02e-06; there is interaction btween disease and drugdose

drug_aov_int <- aov(BloodP ~ factor(DrugDose)*factor(Disease), data = drugdose) 
summary(drug_aov_int)

#3 across disease sliced anova ( for disease 3 there is no interaction between drug dose and disease)

DD_Slic <- drugdose %>%
  group_by(factor(Disease)) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(BloodP ~ factor(DrugDose), data = .x))))
print(DD_Slic$aov)