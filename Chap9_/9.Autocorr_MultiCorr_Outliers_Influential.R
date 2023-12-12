#autocorrelation
#Durbin-Watson test
library(lmtest) #Durbin-Watson test
library("TSA") #google data set
data(google)
x=seq(1,length(google))
lm.model=lm(google~x)
dwtest(lm.model,alternative="greater")


#data 
url = 'http://www.statsci.org/data/general/hills.txt' 
races_table = read.table(url, header=TRUE, sep='\t')
n.index=seq(1,nrow(races_table))
races.table=cbind(races_table,n.index)
lm.model=lm(Time~Distance+Climb,data=races_table)


#outliers
#studentized
#standartized
library(ggplot2)

##Plots of outliers
a = ggplot(lm.model,aes(x=n.index,y=rstandard(lm.model)))+geom_point(color="orange")+geom_line(y=-3)+geom_line(y=3)+labs(title = "Internal Studentized Residuals",x="Observation",y="Residuals")
a
b = ggplot(lm.model,aes(x=n.index,y=rstudent(lm.model)))+geom_point(color="orange")+geom_line(y=-3)+geom_line(y=3)+labs(title = "External Studentized Residuals",x="Observation",y="Residuals")
b

#influential points
#Cook's D
##Cook's D
D.cut=4/(nrow(races.table)-3-1)
D.cut
D= 4/(length(n.index)-3-1)
D
ggplot(lm.model,aes(x=n.index,y=cooks.distance(lm.model)))+geom_point(color="orange")+geom_line(y=D.cut)+labs(title = "Cook's D",x="Observation",y="Cook's Distance")

#Dffits
df.cut=2*(sqrt((3)/nrow(races.table)))
ggplot(lm.model,aes(x=n.index,y=dffits(lm.model)))+geom_point(color="orange")+geom_line(y=df.cut)+geom_line(y=-df.cut)+labs(title = "DFFITS",x="Observation",y="DFFITS")



#Hat values
hat.cut=2*(3)/nrow(races.table)
ggplot(lm.model,aes(x=n.index,y=hatvalues(lm.model)))+geom_point(color="orange")+geom_line(y=hat.cut)+labs(title = "Hat values",x="Observation",y="Hat Values")

#DFBeta
db.cut=2/sqrt(nrow(races.table))
ggplot(lm.model,aes(x=n.index,y=dfbetas(lm.model)[,'Climb']))+geom_point(color="orange")+geom_line(y=db.cut)+geom_line(y=-db.cut)+labs(title = "DFBETA for Climb",x="Observation",y="DFBETAS")

ggplot(lm.model,aes(x=n.index,y=dfbetas(lm.model)[,'Distance']))+geom_point(color="orange")+geom_line(y=db.cut)+geom_line(y=-db.cut)+labs(title = "DFBETA for Distance",x="Observation",y="DFBETAS")

#studentized residuals versus the hat values
ggplot(lm.model,aes(x=hatvalues(lm.model),y=rstudent(lm.model))) +  geom_point(color="orange")+ labs(x="Hat values",y="Residuals")

#multicollinearity
library(car) #VIF
#correlation  matrix
cor(mtcars)

lm.model=lm(mpg~.,data=mtcars)
vif(lm.model)

#------------------

##Cook's D
D.cut=4/(nrow(cars2010)-lm.model$rank)
##Dffit
df.cut=2*(sqrt(lm.model$rank/nrow(cars2010)))
db.cut=2/sqrt(nrow(cars2010)) 
hat.cut=2*(lm.model$rank)/nrow(cars2010)
##arrange plots
grid.arrange(a,b,c,d,e,h,ncol=2)

library(AppliedPredictiveModeling)
data(FuelEconomy)
lm.model=lm(FE~EngDispl+Transmission+AirAspirationMethod+TransLockup+TransCreeperGear+DriveDesc+IntakeValvePerCyl+CarlineClassDesc+VarValveLift,data=cars2010)

##Are there any observations with a dffits larger than 1 AND studentized residuals larger ********
#than 3 in magnitude? If so, list the observations
newcar2<-cbind(cars2010,R = rstudent(lm.model), D= dffits(lm.model))
str(newcar2)
newcar2[abs(newcar2$R)>=3 & newcar2$D >=1,]
          
          
