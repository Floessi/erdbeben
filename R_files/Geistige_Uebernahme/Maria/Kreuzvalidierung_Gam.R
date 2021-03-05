library("pscl")
library("caret")
library("dplyr")
library("purrr")
library("tidyr")
library("splines")

gam1 <- function(df) gam(formula = triggerCountTh ~ mag + depth + heatFlow +
                           crustalThick + mantleThick + dip + s(rake_mod, bs = "ps") +
                           strainRate,family = nb(), data = df)
gam2<- function(df) gam(formula = triggerCountTh ~ mag + depth + heatFlow +
                          crustalThick +mantleThick + dip + rake_mod + strainRate,
                        family = nb(), data = df)
gam3<- function(df) gam(formula = triggerCountTh ~ s(mag, bs="ps") + depth + heatFlow +
                          crustalThick +mantleThick + dip + rake_mod + strainRate,
                        family = nb(), data = df)
gam4<- function(df) gam(formula = triggerCountTh ~ s(mag, bs="ps") + depth + heatFlow +
                          crustalThick +mantleThick + dip + s(rake_mod, bs="ps") +
                          strainRate,family = nb(), data = df)




gamb<-crossVal(full_data,gam1,5)
gamc<-crossVal(full_data,gam2,5)
gamd<-crossVal(full_data,gam3,5)
game<-crossVal(full_data,gam4,5)


mean(gamb[["rmse"]])
mean(gamc[["rmse"]])
mean(gamd[["rmse"]])

var(gamb[["rmse"]])
var(gamc[["rmse"]])
var(gamd[["rmse"]])
var(game[["rmse"]])
