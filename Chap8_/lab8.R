cafe <- read.csv("https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/cafeteria.csv")
str(cafe)

#linear regression, curved, highdegree or interactions
lm.cafe = lm(Sales ~ Dispensers, data = cafe)
summary(lm.cafe)
ggplot(lm.cafe,aes(x=fitted(lm.cafe),y=resid(lm.cafe)))+geom_point(color="blue",size=3)+labs( x="Fitted Values", y="Residuals")

#forward selection

full.model <- lm(Sales ~ Dispensers , data = cafe) # A dot after the tilde tell R to include in the regression all other variables, except the dependent variable.
empty.model <- lm(Sales ~ 1, data = cafe) # The one after the tilde tell R to build an intercept only model.
AIC(full.model) #126.8844
AIC(empty.model) #176.0696
full.model_2 <- lm(Sales ~ Dispensers + I(Dispensers^2) , data = cafe)

AIC(full.model_2)#101.9835 (best)

full.model_3 <- lm(Sales ~ Dispensers +  I(Dispensers^2) + I(Dispensers^3), data = cafe)

AIC(full.model_3)#102.6002

#final model ,random
lm.cafe_f = lm(Sales ~ Dispensers + I(Dispensers^2), data = cafe)
summary(lm.cafe_f)
ggplot(lm.cafe_f,aes(x=fitted(lm.cafe_f),y=resid(lm.cafe_f)))+geom_point(color="blue",size=3)+labs( x="Fitted Values", y="Residuals")


qqnorm(resid(lm.cafe_f)) # mostly normal
qline(resid(lm.cafe_f))
hist(resid(lm.cafe_f)) # slightly deviated
