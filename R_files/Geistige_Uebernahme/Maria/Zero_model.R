library("pscl")
library("caret")
library("dplyr")
library("purrr")
library("tidyr")
library("splines")


set.seed(2020)
total_rows <- nrow(full_data)
ntrain <- floor(0.8 * total_rows)
ntest <- floor(0.2* total_rows)
index <- seq(1: total_rows)
trainIndex <- sample(index, ntrain)
testIndex <- index[-trainIndex]

train <- full_data[trainIndex,]
test <- full_data[testIndex,]

model1<- zeroinfl(triggerCountTh~ bs(mag)+heatFlow+crustalThick+mantleThick+depth+dip+bs(rake_mod)+strainRate, data=train, dist="negbin")
model2<-zeroinfl(triggerCountTh~ bs(mag)+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=train, dist="negbin")
model3<-zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+bs(rake_mod)+strainRate, data=train, dist="negbin")
model4<-zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=train, dist="negbin")
model5<-zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate+dip*rake_mod, data=train, dist="negbin")
model6<-zeroinfl(triggerCountTh~ mag+I(mag^2)+I(mag^3)+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=train, dist="negbin")


model1.1<- zeroinfl(triggerCountTh~ bs(mag)+heatFlow+crustalThick+mantleThick+depth+dip+bs(rake_mod)+strainRate, data=test, dist="negbin")
model2.1<-zeroinfl(triggerCountTh~ bs(mag)+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=test, dist="negbin")
model3.1<-zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+bs(rake_mod)+strainRate, data=test, dist="negbin")
model4.1<-zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=test, dist="negbin")
model5.1<-zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate+dip*rake_mod, data=test, dist="negbin")
model6.1<-zeroinfl(triggerCountTh~ mag+I(mag^2)+I(mag^3)+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=test, dist="negbin")


AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5)
AIC(model6)

AIC(model1.1)
AIC(model2.1)
AIC(model3.1)
AIC(model4.1)
AIC(model5.1)
AIC(model6.1)

summary(model4)



