#########################################################################################
# Model Assesment (zeroinfl) ############################################################
#########################################################################################

# Notwendige Packages
library(splines)
library(pscl)
library(mgcv)
library(MASS)


# Lade die geupdateten Daten (27.02.2021)
load("Daten/data_full_00001_updated.Rda")


# Erneute konkrete Modelvergleiche mit Fokus auf Signifikanz der Koeffizienten
# Wird dip signigikant im Modell?
# heatFlow,crustalThick, mantleThick sind stark korreliert
#   => welche Kombinationen sind signifikant?
# Grimm:
# heatFlow > crustalThick > mantleThick (inhaltliche Gewichtung/Interesse)

# Formula des besten logit-Modells
# willTrigger ~ mag + depth + crustalThick + mantleThick + bs(rake_mod) + strainRate

# "Problematische" Korrelationen
cor(as.matrix(full_data[, c("depth", "heatFlow", "crustalThick", "mantleThick", "elevation")]))



# Modellanwärter
#1 bestes bisher, Vermutung
mod_zeroinfl <- zeroinfl(formula = triggerCountTh ~ mag + crustalThick + depth +
                           bs(rake_mod) + strainRate, data = full_data, dist = "negbin")
AIC(mod_zeroinfl)
summary(mod_zeroinfl)



# Modellformula für logit aus dem besten logit-Modell
# negBin voll
mod_zeroinfl2 <- zeroinfl(formula = triggerCountTh ~ mag + depth + heatFlow + crustalThick + dip +
                            mantleThick + bs(rake_mod) + strainRate | mag + depth + crustalThick +
                            mantleThick + bs(rake_mod) + strainRate, data = full_data,
                          dist = "negbin")
AIC(mod_zeroinfl2)
summary(mod_zeroinfl2)
cv_zf2 <- cross.validation(mod_zeroinfl2, full_data)


# Mnus dip und heatFlow
mod_zeroinfl3 <- zeroinfl(formula = triggerCountTh ~ mag + depth + crustalThick +
                            mantleThick + bs(rake_mod) + strainRate, data = full_data,
                          dist = "negbin")
AIC(mod_zeroinfl3)
summary(mod_zeroinfl3)
cv_zf3 <- cross.validation(mod_zeroinfl3, full_data)


# crustalThick oder mantleThick weglassen, beides schlecht
mod_zeroinfl4 <- zeroinfl(formula = triggerCountTh ~ mag + depth +
                            mantleThick + bs(rake_mod) + strainRate | mag + depth +
                            mantleThick + bs(rake_mod) + strainRate, data = full_data,
                          dist = "negbin")
AIC(mod_zeroinfl4)
summary(mod_zeroinfl4)
cv_zf4 <- cross.validation(mod_zeroinfl4, full_data)






#########################################################################################
# Model Assesment (binomial) ############################################################
#########################################################################################

# Relevant zum jetztigen Stand fuer ein triggerCountTh-Modell bzw willTrigger:

# mag:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen, a*exp(b*mag) waere denkbar
#               a ist im Intercept enthalten und b ist dann die Koeffizient fuer mag
# depth:        kommt drauf an ob das im vollen Modell signifikant wird
# heatFlow:     einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# crustalThick: einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# mantleThick:  kommt drauf an ob das im vollen Modell signifikant wird
# dip:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# rake_mod:     mit Spline oder Polynom einfliessen lassen, siehe stacked-Barplot
# strainRate:   einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen


# Volles logit-Modell ohne Splines
mod_bin_full_ns <- glm(data = full_data, willTrigger ~ mag + depth +
                 heatFlow + crustalThick + mantleThick + dip + rake_mod +
                 strainRate, family = binomial(link = "logit"))
summary(mod_bin_full_ns)
AIC(mod_bin_full_ns)

# Step_AIC
stepAIC(mod_bin_full_ns, direction = "both")
# Verwerfe nur heatFlow

# Logit-Modell ohne Splines laut stepAIC
mod_bin_full_ns2 <- glm(data = full_data, willTrigger ~ mag + depth +
                         crustalThick + mantleThick + dip + rake_mod +
                         strainRate, family = binomial(link = "logit"))
summary(mod_bin_full_ns2)
AIC(mod_bin_full_ns2)
# Dip nicht signifikant

# Logit-Modell ohne Splines laut stepAIC und zusätzlich dip entfernt
mod_bin_full_ns3 <- glm(data = full_data, willTrigger ~ mag + depth +
                          crustalThick + mantleThick + rake_mod +
                          strainRate, family = binomial(link = "logit"))
summary(mod_bin_full_ns3)
AIC(mod_bin_full_ns3)



# Volles logit-Modell mit Splines
mod_bin_full_s <- glm(data = full_data, willTrigger ~ mag + depth +
                         heatFlow + crustalThick + mantleThick + dip + bs(rake_mod) +
                         strainRate, family = binomial(link = "logit"))
summary(mod_bin_full_s)
AIC(mod_bin_full_s)

# Step_AIC
stepAIC(mod_bin_full_ns, direction = "both")
# Verwerfe nur heatFlow

# Logit-Modell mit Splines laut stepAIC
mod_bin_full_s2 <- glm(data = full_data, willTrigger ~ mag + depth +
                          crustalThick + mantleThick + dip + bs(rake_mod) +
                          strainRate, family = binomial(link = "logit"))
summary(mod_bin_full_s2)
AIC(mod_bin_full_s2)
# Dip nicht signifikant

# Logit-Modell mit Splines laut stepAIC und zusätzlich dip entfernt
mod_bin_full_s3 <- glm(data = full_data, willTrigger ~ mag + depth +
                          crustalThick + mantleThick + bs(rake_mod) +
                          strainRate, family = binomial(link = "logit"))
summary(mod_bin_full_s3)
AIC(mod_bin_full_s3)







