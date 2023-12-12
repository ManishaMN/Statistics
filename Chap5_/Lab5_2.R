disks = read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/disks.csv")

str(disks$Technician)

# 2 way anova with interactions ( technician and brand): there is interaction yes, yes its significant

disk_aov_int <- aov(Time ~ factor(Technician)*factor(Brand), data = disks) 
summary(disk_aov_int)

# no its not appropriate to check main effects


# slicing ( yes there is difference with technician )
DK_Slic <- disks %>%
  group_by(factor(Technician)) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(Time ~ factor(Brand), data = .x))))
print(DK_Slic$aov)

#PostHOC testing tukeyhsd
tukey.disk <- TukeyHSD(disk_aov_int) 
print(tukey.disk)
plot(tukey.disk, las = 1)

factor(disks$Technician)