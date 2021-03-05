#### Post Besprechung ###
######################################################################################################
# Post-Besprechung mit Prof. Kuechenhoff am 28.01 ####################################################
######################################################################################################


library(ggplot2)
library(mgcv)
library(MASS)
library(dplyr)
full_data <- load("C:/Users/nikol/OneDrive/Dokumente/NeuErdbeben/Datensaetze/data_full_00001.Rda")

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

attach(full_data)
######################################################################################################
# Simples negBin #####################################################################################
model_negBin <- glm.nb(formula = triggerCountTh ~ mag + depth + heatFlow + crustalThick +
                         mantleThick + dip + rake_mod + strainRate, data = full_data)
summary(model_negBin)
# heatFlow nicht signifikant , wird aber nicht durch stepAIC rausgeschmissen
stepAIC(model_negBin)
# Result-model
# glm.nb(formula = triggerCountTh ~ mag + depth + crustalThick + mantleThick + dip +
#             rake_mod + strainRate, data = full_data, init.theta = 1.07199816, link = log)

# Ohne heatFlow #####################################################################################
model_negBin2 <- glm.nb(formula = triggerCountTh ~ mag + depth + crustalThick + mantleThick +
                          dip + rake_mod + strainRate, data = full_data)
summary(model_negBin2)
# Hypothese heatFlow-Koeffizient=0
anova(model_negBin2, model_negBin)
# Model2 ohne heatflow besser
# Auch AIC minimal besser



######################################################################################################
# Negbin aber penalisierte Splines fuer rake_mod? ####################################################
model_negBin_gam <- gam(formula = triggerCountTh ~ mag + depth + heatFlow + crustalThick +
                          mantleThick + dip + s(rake_mod, bs = "ps") + strainRate,
                        family = nb(), data = full_data)
summary(model_negBin_gam)
# dip wird nicht mehr signifikant und heatflow erneut nicht
# ~6 df fuer penalisierte splines von rake_mod => sinnvoll
AIC(model_negBin_gam)

# Ohne dip und heatFlow
model_negBin_gam2 <- gam(formula = triggerCountTh ~ mag + depth + crustalThick +
                           mantleThick + s(rake_mod, bs = "ps") + strainRate,
                         family = nb(), data = full_data)
summary(model_negBin_gam2)
# Alles signifikant, aber ist das das bessere Modell?
AIC(model_negBin_gam2)
anova(model_negBin_gam2, model_negBin_gam)
# Resid. Dev ware minimal besser, aber viel zu wenig







# StepAIC-Funktion aus anderen R-file ###############################################################
# Folgende Packages sind zu laden
library(checkmate)
library(pscl)
library(MASS)

# Zum input dieser Funktion:
# model:        Regressionmodell mit allen Variablen, die beruecksichtigt werden sollen
# data:         Der Datensatz auf dem das Modell basiert
# all_models:   Wenn TRUE werden Modelle fuer alle Regressionmodelle mit AIC ausgegeben (default = FALSE)

AIC_step_koft <- function(model, data, all_models = FALSE) {

  # Test ob die Eingabe ein passendes Regressionsmodell ist
  assert_choice(class(model)[[1]], c("lm", "negbin", "zeroinfl"))
  # Falls all_models == TRUE, werden alle Modelle ausgegeben
  ifelse(all_models, cl <- c("lm", "negbin", "zeroinfl"), cl <- class(model)[[1]])

  summary_input <- summary(model)
  target_var <- as.character(formula(model))[[2]]


  if ("lm" %in% cl){
    coeff <- rownames(summary_input$coefficients)
    iterations <- length(coeff) - 1

    # Spalten der Ausgabe
    formula_iterations <- character()
    aic_iterations <- numeric()

    for (i in seq_len(iterations)) {
      temp_formula <- paste0(target_var, " ~ ", paste(coeff[seq(2, i + 1)], collapse=" + "))
      formula_iterations[[i]] <- temp_formula
      temp_model <- lm(formula = as.formula(temp_formula), data)
      aic_iterations[[i]] <- AIC(temp_model)
    }

    part_1 <- data.frame("Family" = rep("Lm", iterations), "Formula" = formula_iterations,
                         "AIC" = aic_iterations, "CoefficientTotal" = seq_len(iterations),
                         "CoefficientCount" = seq_len(iterations), "CoefficientBin" = rep(0, iterations),
                         "CoefficientMax" = rep(iterations, iterations))
  }

  if ("negbin" %in% cl) {
    coeff <- rownames(summary_input$coefficients)
    iterations <- length(coeff) - 1

    # Spalten der Ausgabe
    formula_iterations <- character()
    aic_iterations <- numeric()

    for (i in seq_len(iterations)) {
      temp_formula <- paste0(target_var, " ~ ", paste(coeff[seq(2, i + 1)], collapse=" + "))
      formula_iterations[[i]] <- temp_formula
      temp_model <- glm.nb(formula = as.formula(temp_formula), data, link = log, maxit = 100)
      aic_iterations[[i]] <- AIC(temp_model)
    }

    part_2 <- data.frame("Family" = rep("negbin", iterations), "Formula" = formula_iterations,
                         "AIC" = aic_iterations, "CoefficientTotal" = seq_len(iterations),
                         "CoefficientCount" = seq_len(iterations), "CoefficientBin" = rep(0, iterations),
                         "CoefficientMax" = rep(iterations, iterations))
  }

  if ("zeroinfl" %in% cl) {
    if (class(model)[[1]] == "zeroinfl") {
      coeff <- rownames(summary_input$coefficients[[1]])
      iterations <- length(coeff) - 2
    }
    else {
      coeff <- rownames(summary_input$coefficients)
      iterations <- length(coeff) - 1
    }

    # Spalten der Ausgabe
    formula_iterations <- character()
    aic_iterations <- numeric()
    coefftotal_iterations <- numeric()
    coeffcount_iterations <- numeric()
    coeffbin_iterations <- numeric()

    for (j in seq_len(iterations + 1)) {
      for (i in seq_len(iterations)) {
        if (j == 1) {
          temp_formula <- paste0(target_var, " ~ ", paste(coeff[seq(2, i + 1)], collapse=" + "), " | 1")
        }
        else {
          temp_formula <- paste0(target_var, " ~ ", paste(coeff[seq(2, i + 1)], collapse=" + "), " | ",
                                 paste(coeff[seq(2, j)], collapse=" + "))
        }
        formula_iterations[[i + ((j - 1) * iterations)]] <- temp_formula
        temp_model <- zeroinfl(formula = as.formula(temp_formula), data, dist = "negbin")
        aic_iterations[[i + ((j - 1) * iterations)]] <- AIC(temp_model)
        coefftotal_iterations[[i + ((j - 1) * iterations)]] <- i + j - 1
        coeffcount_iterations[[i + ((j - 1) * iterations)]] <- i
        coeffbin_iterations[[i + ((j - 1) * iterations)]] <- j - 1
      }
    }

    part_3 <- data.frame("Family" = rep("zeroinfl NegBin", iterations * (iterations + 1)), "Formula" = formula_iterations,
                         "AIC" = aic_iterations, "CoefficientTotal" = coefftotal_iterations,
                         "CoefficientCount" = coeffcount_iterations, "CoefficientBin" = coeffbin_iterations,
                         "CoefficientMax" = rep(iterations * 2,iterations * (iterations + 1)))
  }

  if (all_models) {
    rbind(part_1, part_2, part_3)
  }
  else {
    if (class(model)[[1]] == "lm") {
      part_1
    }
    else if (class(model)[[1]] == "negbin") {
      part_2
    }
    else {
      part_3
    }
  }
}


test <- zeroinfl(triggerCountTh ~ mag + depth + heatFlow + crustalThick +
                   mantleThick + dip + rake_mod + strainRate, full_data, dist = "negbin")
