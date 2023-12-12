df <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/coviddata.csv")
str(df)

ggplot(data = df, aes(sample = covidDeathsPerCapita, color = region)) +
  stat_qq() +
  stat_qq_line()

#They are normally distributed

var.test(covidDeathsPerCapita ~ region, data = df)

# Variance are equal

t.test(covidDeathsPerCapita ~ region, data = df, var.equal = TRUE)

#true difference in means between group East and group West is not equal to 0

# There may be other factors at play that contribute to the observed difference in means. so we can't conclude 

