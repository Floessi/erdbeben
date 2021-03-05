#########################################################################################
# Automatisierte 10-fache Kreuzvalidierung ##############################################
#########################################################################################

# Notwendige Packages
library(splines)
library(pscl)
library(mgcv)
library(MASS)


# Lade die geupdateten Daten (27.02.2021)
load("Daten/data_full_00001_updated.Rda")



# Funktionsbeschreibung:
# 10-fold Cross-Validation für die Berechnung des Mittelwertes der quadrierten
# Prädiktionsfehler für verschiedene Modelle der selben Zielvariable

# Zum Input:
# Unterstütze Modelle sind
# zeroinfl (pscl)
# gam (mgcv) ; nur family = nb()
# glm.nb (MASS)
# glm
# Vom Input-Model ist lediglich das "formula" und die "family/dist/etc" relevant
# Der Datensatz muss die Zielvariable und Kovariablen des Models enthalten
# Der Seed ist lediglich für die identischen Unterteilungen der Daten verantwortlich


cross.validation <- function(model, data, seed = 12345) {

  # Bestimme um welche Modellfamilie es sich handelt (zeroinfl, gam, negbin, glm/lm)
  mod_fam <- class(model)[[1]]

  # Setze den Seed
  set.seed(seed)

  # Beobachtungsanzahl
  n <- nrow(data)

  # Spaltennummer der Zielvariable des Modells
  target_num <- which(model$call$formula[[2]] == colnames(data), seq_len(ncol(data)))

  # Unterteile die Daten für eine ten-fold CV
  k1_ind <- sample(x = seq_len(n), size = round(n *0.1))
  temp <- setdiff(seq_len(n), k1_ind)
  k2_ind <- sample(x = temp, size = round(n *0.1))
  temp <- setdiff(temp, k2_ind)
  k3_ind <- sample(x = temp, size = round(n *0.1))
  temp <- setdiff(temp, k3_ind)
  k4_ind <- sample(x = temp, size = round(n *0.1))
  temp <- setdiff(temp, k4_ind)
  k5_ind <- sample(x = temp, size = round(n *0.1))
  temp <- setdiff(temp, k5_ind)
  k6_ind <- sample(x = temp, size = round(n *0.1))
  temp <- setdiff(temp, k6_ind)
  k7_ind <- sample(x = temp, size = round(n *0.1))
  temp <- setdiff(temp, k7_ind)
  k8_ind <- sample(x = temp, size = round(n *0.1))
  temp <- setdiff(temp, k8_ind)
  k9_ind <- sample(x = temp, size = round(n *0.1))
  k10_ind <- setdiff(temp, k9_ind)


  dataList <- list()
  dataList[[1]] <- data[k1_ind,]
  dataList[[2]] <- data[k2_ind,]
  dataList[[3]] <- data[k3_ind,]
  dataList[[4]] <- data[k4_ind,]
  dataList[[5]] <- data[k5_ind,]
  dataList[[6]] <- data[k6_ind,]
  dataList[[7]] <- data[k7_ind,]
  dataList[[8]] <- data[k8_ind,]
  dataList[[9]] <- data[k9_ind,]
  dataList[[10]] <- data[k10_ind,]


  if (mod_fam == "zeroinfl"){
    CV_error_sum <- numeric()
    for (i in seq_len(10)) {
      data_ind <- setdiff(seq_len(10), i)
      train_data <- rbind(dataList[[data_ind[1]]], dataList[[data_ind[2]]],
                          dataList[[data_ind[3]]], dataList[[data_ind[4]]],
                          dataList[[data_ind[5]]], dataList[[data_ind[6]]],
                          dataList[[data_ind[7]]], dataList[[data_ind[8]]],
                          dataList[[data_ind[9]]])
      temp_model <- zeroinfl(formula = model$formula, data = train_data, dist = model$dist)
      CV_error_sum[[i]] <- sum((dataList[[i]][, target_num] -
                                  predict(temp_model, newdata = dataList[[i]]))^2)
    }

    data.frame("Formula" = paste(as.character(model$call$formula)[2], "~",
                                 as.character(model$call$formula)[3], collapse = " "),
               "Modeltype" = paste("zeroinfl", model$dist,collapse = " "),
               "CV_pred_err" = sum(CV_error_sum)/n)
  }

  else if (mod_fam == "gam"){
    CV_error_sum <- numeric()
    for (i in seq_len(10)) {
      data_ind <- setdiff(seq_len(10), i)
      train_data <- rbind(dataList[[data_ind[1]]], dataList[[data_ind[2]]],
                          dataList[[data_ind[3]]], dataList[[data_ind[4]]],
                          dataList[[data_ind[5]]], dataList[[data_ind[6]]],
                          dataList[[data_ind[7]]], dataList[[data_ind[8]]],
                          dataList[[data_ind[9]]])
      temp_model <- gam(formula = model$formula, family = nb(), data = train_data)
      CV_error_sum[[i]] <- sum((dataList[[i]][, target_num] -
                                  predict(temp_model, newdata = dataList[[i]]))^2)
    }

    data.frame("Formula" = paste(as.character(model$call$formula)[2], "~",
                                 as.character(model$call$formula)[3], collapse = " "),
               "Modeltype" = "gam nb",
               "CV_pred_err" = sum(CV_error_sum)/n)
  }

  else if (mod_fam == "negbin"){
    CV_error_sum <- numeric()
    for (i in seq_len(10)) {
      data_ind <- setdiff(seq_len(10), i)
      train_data <- rbind(dataList[[data_ind[1]]], dataList[[data_ind[2]]],
                          dataList[[data_ind[3]]], dataList[[data_ind[4]]],
                          dataList[[data_ind[5]]], dataList[[data_ind[6]]],
                          dataList[[data_ind[7]]], dataList[[data_ind[8]]],
                          dataList[[data_ind[9]]])
      temp_model <- glm.nb(formula = model$call$formula, data = train_data)
      CV_error_sum[[i]] <- sum((dataList[[i]][, target_num] -
                                  predict(temp_model, newdata = dataList[[i]]))^2)
    }

    data.frame("Formula" = paste(as.character(model$call$formula)[2], "~",
                                 as.character(model$call$formula)[3], collapse = " "),
               "Modeltype" = "glm negbin",
               "CV_pred_err" = sum(CV_error_sum)/n)
  }

  else {
    CV_error_sum <- numeric()
    for (i in seq_len(10)) {
      data_ind <- setdiff(seq_len(10), i)
      train_data <- rbind(dataList[[data_ind[1]]], dataList[[data_ind[2]]],
                          dataList[[data_ind[3]]], dataList[[data_ind[4]]],
                          dataList[[data_ind[5]]], dataList[[data_ind[6]]],
                          dataList[[data_ind[7]]], dataList[[data_ind[8]]],
                          dataList[[data_ind[9]]])
      temp_model <- glm(formula = model$call$formula, data = train_data,
                        family = model$family[[1]])
      CV_error_sum[[i]] <- sum((dataList[[i]][, target_num] -
                                  predict(temp_model, newdata = dataList[[i]]))^2)
    }

    data.frame("Formula" = paste(as.character(model$call$formula)[2], "~",
                                 as.character(model$call$formula)[3], collapse = " "),
               "Modeltype" = "glm",
               "CV_pred_err" = sum(CV_error_sum)/n)
  }

}




#########################################################################################
# Test/ Durchführung der Modelselektion #################################################
#########################################################################################

# Modellauswahl für zeroinfl aus R-file: model_assessment.R
# Restliche Modelle aus anderen R-files, die zu früheren Zeitpunkten mal unserer besten
# Modelle mit anderen Modellarten dargestellt haben


# zeroinfl-Model(ursprünglich bestes):
mod_zeroinfl <- zeroinfl(formula = triggerCountTh ~ mag + crustalThick + depth +
                           bs(rake_mod) + strainRate, data = full_data, dist = "negbin")
summary(mod_zeroinfl)
cv_mod_zeroinfl <- cross.validation(mod_zeroinfl, full_data)

# zeroinfl-Model(bestes):
mod_zeroinfl_best <- zeroinfl(formula = triggerCountTh ~ mag + depth + crustalThick +
                            mantleThick + bs(rake_mod) + strainRate, data = full_data,
                          dist = "negbin")
summary(mod_zeroinfl_best)
AIC(mod_zeroinfl_best)
cv_mod_zeroinfl_best <- cross.validation(mod_zeroinfl_best, full_data)

########################################################################################
mod_zeroinfl_best2 <- zeroinfl(formula = triggerCountTh ~ mag + depth + crustalThick +
                                mantleThick + rake_mod + strainRate, data = full_data,
                              dist = "negbin")
summary(mod_zeroinfl_best2)
AIC(mod_zeroinfl_best2)
cv_mod_zeroinfl_best2 <- cross.validation(mod_zeroinfl_best2, full_data)
#######################################################################################
mod_zeroinfl_best3 <- zeroinfl(formula = triggerCountTh ~ mag + depth + mantleThick +
                                 rake_mod + strainRate | mag + depth + crustalThick +
                                 mantleThick + rake_mod + strainRate, data = full_data,
                               dist = "negbin")
summary(mod_zeroinfl_best3)
AIC(mod_zeroinfl_best3)
cv_mod_zeroinfl_best3 <- cross.validation(mod_zeroinfl_best3, full_data)

########################


# gam-Negativ-Binomial
mod_gam_nb <- gam(formula = triggerCountTh ~ mag + depth + crustalThick + mantleThick +
                    s(rake_mod, bs = "ps") + strainRate, family = nb(), data = full_data)
cv_mod_gam_nb <- cross.validation(mod_gam_nb, full_data)

# glm.nb (Negativ-Binomial ohne Splines)
mod_nb <- glm.nb(formula = triggerCountTh ~ mag + depth + crustalThick + mantleThick +
                         dip + rake_mod + strainRate, data = full_data)
cv_mod_nb <- cross.validation(mod_nb, full_data)

# glm
mod_glm_quasip <- glm(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                        dip + rake_mod, family = "quasipoisson", full_data)
cv_mod_glm_quasip <- cross.validation(mod_glm_quasip, full_data)


# Vergleiche die Modelle anhand ihrer Mittelwerte der quadrierten Prädiktionsfehler
# (CV_pred_err)
temp2 <- rbind(cv_mod_zeroinfl, cv_mod_gam_nb, cv_mod_nb, cv_mod_glm_quasip)





#########################################################################################
# Test/ Durchführung der Modelselektion (Logit-Modelle) #################################
#########################################################################################

# Modellauswahl aus R-file: model_assessment.R

# Bestes vom AIC (Splines und nur signifikante Variablen)
mod_bin_full_s3 <- glm(data = full_data, willTrigger ~ mag + depth +
                         crustalThick + mantleThick + bs(rake_mod) +
                         strainRate, family = binomial(link = "logit"))
cv_mod_bin_full_s3 <- cross.validation(mod_bin_full_s3, full_data)

# Volles logit-Modell mit Splines
mod_bin_full_s <- glm(data = full_data, willTrigger ~ mag + depth +
                        heatFlow + crustalThick + mantleThick + dip + bs(rake_mod) +
                        strainRate, family = binomial(link = "logit"))
cv_mod_bin_full_s <- cross.validation(mod_bin_full_s, full_data)

# Logit-Modell ohne Splines laut stepAIC und zusätzlich dip entfernt
mod_bin_full_ns3 <- glm(data = full_data, willTrigger ~ mag + depth +
                          crustalThick + mantleThick + rake_mod +
                          strainRate, family = binomial(link = "logit"))
cv_mod_bin_full_ns3 <- cross.validation(mod_bin_full_ns3, full_data)






#########################################################################################
# Bonus #################################################################################
#########################################################################################


# Beispielsweise neue Zielvariable
mod_zeroinfl_target2 <- zeroinfl(nAftershocks ~ mag + crustalThick + depth +
                                   bs(rake_mod) + strainRate,
                                 data = full_data[full_data$nAftershocks >= 0, ], dist = "negbin")
temp <- cross.validation(mod_zeroinfl_target2, full_data[full_data$nAftershocks >= 0, ])
View(temp)
# 1.277764

# Normale Zielvariable CV ohne Tohoku-Hauptbeben
temp2 <- cross.validation(mod_zeroinfl, full_data[-6247,])
View(temp2)
# 0.788339
