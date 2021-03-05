library("pscl")
library("caret")
library("dplyr")
library("purrr")
library("tidyr")
library("splines")

modela<-function(df) zeroinfl(triggerCountTh~ bs(mag)+heatFlow+crustalThick+mantleThick+depth+dip+bs(rake_mod)+strainRate, data=df, dist="negbin")
modelb<-function(df) zeroinfl(triggerCountTh~ bs(mag)+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=df, dist="negbin")
modelc<-function(df) zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+bs(rake_mod)+strainRate, data=df, dist="negbin")
modeld<-function(df) zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=df, dist="negbin")
modele<-function(df) zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate+dip*rake_mod, data=df, dist="negbin")
modelf<-function(df) zeroinfl(triggerCountTh~ mag+I(mag^2)+I(mag^3)+heatFlow+crustalThick+mantleThick+depth+dip+rake_mod+strainRate, data=df, dist="negbin")


# For each fold, extract summary for example
a<-crossVal(full_data,modela,5)
b<-crossVal(full_data,modelb,5)
c<-crossVal(full_data,modelc,5)
d<-crossVal(full_data,modeld,5)
e<-crossVal(full_data,modele,5)
f<-crossVal(full_data,modelf,5)

a[["rmse"]]
b[["rmse"]]
c[["rmse"]]
d[["rmse"]]
e[["rmse"]]
f[["rmse"]]

mean(a[["rmse"]])
mean(b[["rmse"]])# Zweitbestes modelb bzw model2
mean(c[["rmse"]])
mean(d[["rmse"]])
mean(e[["rmse"]])# Bestes Model modele bzw model4
mean(f[["rmse"]])

var(a[["rmse"]])
var(b[["rmse"]])
var(c[["rmse"]])
var(d[["rmse"]])# Zweitbestes modeld bzw model4
var(e[["rmse"]])# Bestes Model modele bzw model5
var(f[["rmse"]])

a[["mod_summary"]]#NA
b[["mod_summary"]]
c[["mod_summary"]]
d[["mod_summary"]]
e[["mod_summary"]]
f[["mod_summary"]]#NA
