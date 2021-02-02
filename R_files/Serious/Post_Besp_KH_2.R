######################################################################################################
# Post-Besprechung mit Prof. Kuechenhoff am 28.01 ####################################################
######################################################################################################


library(ggplot2)
library(mgcv)
library(MASS)
library(dplyr)
load("Daten/data_full_00001.Rda")

# Spalte fuer modifizierten rake
rake_mod <- full_data$rake
for (i in seq_len(length(rake_mod))) {
  if (rake_mod[[i]] < 0) {
    rake_mod[[i]] <- (-90) + abs(rake_mod[[i]] + 90)
  }
  else {
    rake_mod[[i]] <- 90 - abs(rake_mod[[i]] - 90)
  }
}
full_data$rake_mod <- rake_mod



######################################################################################################
# Welche Variablen und wie sind diese fuer ein Modell relevant? ######################################
######################################################################################################

colnames(full_data)

# Relevant zum jetztigen Stand fuer ein triggerCountTh-Modell:

# mag:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen, a*exp(b*mag) waere denkbar
#               a ist im Intercept enthalten und b ist dann die Koeffizient fuer mag
# depth:        kommt drauf an ob das im vollen Modell signifikant wird
# heatFlow:     einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# crustalThick: einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# mantleThick:  kommt drauf an ob das im vollen Modell signifikant wird
# dip:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# rake_mod:     mit Spline oder Polynom einfliessen lassen, siehe stacked-Barplot
# strainRate:   einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen

model_negBin <- glm.nb(formula = triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                         dip + rake_mod + strainRate, data = full_data)
summary(model_negBin)

stepAIC(model_negBin)



