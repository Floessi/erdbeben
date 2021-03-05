#################################################################################
# Manuelle Kreuzvalidierung #####################################################
#################################################################################

# Notwendige Packages
library(splines)
library(pscl)
library(MASS)
library(dplyr)
library(mgcv)

# Lade die geupdateten Daten (27.02.2021)
load("Daten/data_full_00001.Rda")




# Kreuzvalidierung allgemeines Framework-Variablen ##############################
n <- nrow(full_data)
set.seed(12345)

# Unterteile die Daten für eine 5-fach CV
k1_ind <- sample(x = seq_len(n), size = round(n *0.2))
temp <- setdiff(seq_len(n), k1_ind)
k2_ind <- sample(x = temp, size = round(n *0.2))
temp <- setdiff(temp, k2_ind)
k3_ind <- sample(x = temp, size = round(n *0.2))
temp <- setdiff(temp, k3_ind)
k4_ind <- sample(x = temp, size = round(n *0.2))
k5_ind <- setdiff(temp, k4_ind)

# Wo ist das Tohoku-Hauptbeben drin
full_data[6247, "mag"]
6247 %in% k1_ind # Hier (seed-abhängig)
6247 %in% k2_ind
6247 %in% k3_ind
6247 %in% k4_ind
6247 %in% k5_ind

datenListe <- list()
datenListe[[1]] <- full_data[k1_ind,]
# Alternativ ohne das Tohoku-Hauptbeben
# datenListe[[1]] <- full_data[setdiff(k1_ind, 6247),]
datenListe[[2]] <- full_data[k2_ind,]
datenListe[[3]] <- full_data[k3_ind,]
datenListe[[4]] <- full_data[k4_ind,]
datenListe[[5]] <- full_data[k5_ind,]






# Modelle #######################################################################

# Zeroinflated-Modelle
mod_zeroinf_full <- zeroinfl(triggerCountTh ~ mag + crustalThick + heatFlow + dip +
                            depth + bs(rake_mod) + strainRate, data = full_data,
                         dist = "negbin")
summary(mod_zeroinf_full)

# Zeroinflated-Modell mit ausschließlich signifikanten Effekten
mod_zeroinf <- zeroinfl(triggerCountTh ~ mag + heatFlow +
                          depth + bs(rake_mod) + strainRate, data = full_data,
                        dist = "negbin")
summary(mod_zeroinf)


CV_sum <- numeric()
for (i in seq_len(5)) {
  daten_ind <- setdiff(seq(1, 5), i)
  train_daten <- rbind(datenListe[[daten_ind[1]]], datenListe[[daten_ind[2]]],
                       datenListe[[daten_ind[3]]], datenListe[[daten_ind[4]]])
  temp_model <- zeroinfl(triggerCountTh ~ mag + heatFlow +
                           depth + bs(rake_mod) + strainRate, data = train_daten,
                         dist = "negbin")
  CV_sum[[i]] <- sum((datenListe[[i]]$triggerCountTh -
                       predict(temp_model, newdata = datenListe[[i]]))^2)
}

CV_sum
# Mit Tohoku-Beben
# 145751.130   1860.867   1214.317   1582.064   3947.837
# Ohne
# 1476.290 1983.739 1205.480 1590.805 4214.577
mean(CV_sum)/n
# Mit Tohoku-Beben
# 2.251896
# Ohne
# 0.1527594



# Negatives Binomialmodell
model_negBin <- glm.nb(formula = triggerCountTh ~ mag + depth + crustalThick + mantleThick +
                          dip + rake_mod + strainRate, data = full_data)
summary(model_negBin)


CV_sum <- numeric()
for (i in seq_len(5)) {
  daten_ind <- setdiff(seq(1, 5), i)
  train_daten <- rbind(datenListe[[daten_ind[1]]], datenListe[[daten_ind[2]]],
                       datenListe[[daten_ind[3]]], datenListe[[daten_ind[4]]])
  temp_model <- glm.nb(formula = triggerCountTh ~ mag + depth + crustalThick + mantleThick +
                         dip + rake_mod + strainRate, data = train_daten)
  CV_sum[[i]] <- sum((datenListe[[i]]$triggerCountTh -
                        predict(temp_model, newdata = datenListe[[i]]))^2)
}

CV_sum
# Mit Tohoku-Beben
# 277606.22  28818.14  25498.86  27845.12  32467.05
# Ohne
# 25642.57 28716.52 25419.79 27751.29 32361.45
mean(CV_sum)/n
# Mit Tohoku-Beben
# 5.722305
# Ohne
# 2.040873



# GAM Negatives Binomialmodell
model_negBin_gam <- gam(formula = triggerCountTh ~ mag + depth + crustalThick +
                           mantleThick + s(rake_mod, bs = "ps") + strainRate,
                         family = nb(), data = full_data)
summary(model_negBin_gam)
plot(model_negBin_gam,select = 5)

CV_sum <- numeric()
for (i in seq_len(5)) {
  daten_ind <- setdiff(seq(1, 5), i)
  train_daten <- rbind(datenListe[[daten_ind[1]]], datenListe[[daten_ind[2]]],
                       datenListe[[daten_ind[3]]], datenListe[[daten_ind[4]]])
  temp_model <- gam(formula = triggerCountTh ~ mag + depth + crustalThick +
                      mantleThick + s(rake_mod, bs = "ps") + strainRate,
                    family = nb(), data = train_daten)
  CV_sum[[i]] <- sum((datenListe[[i]]$triggerCountTh -
                        predict(temp_model, newdata = datenListe[[i]]))^2)
}

CV_sum
# Mit Tohoku-Beben
# 277748.33  29219.44  25695.13  28202.99  32892.66
# Ohne
# 25719.05 29101.75 25610.19 28085.81 32764.44
mean(CV_sum)/n
# Mit Tohoku-Beben
# 5.744526
# Ohne
# 2.061146







