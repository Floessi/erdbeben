#############################################################################################################################
# Eine Funktion, die Modellvergleiche anhand des AICs erleichtern soll ######################################################
#############################################################################################################################

# 1) Funktion
# 2) Tests
# 3) Full_models

# Datensatz
load("Daten/data_full_00001.Rda")

#############################################################################################################################
# 1) Funktion, die Step-AIC (vorerst nur "forward") auch fuer zeroinfl-NegBin implementiert #################################
#############################################################################################################################

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



#############################################################################################################################
# 2) Tests und Beispielausgaben #############################################################################################
#############################################################################################################################
# Testmodelle
test1 <- lm(triggerCountTh ~ mag + I(mag^2) + I((mag > 4.9) * ((mag - 5.0)^2)) + I((mag > 5.9) * ((mag - 6.0)^2)) +
              I((mag > 6.9) * ((mag - 7.0)^2)) + depth + heatFlow + crustalThick + mantleThick + elevation + strike + dip +
              rake, full_data)

test2 <- glm.nb(triggerCountTh ~ mag + I(mag^2) + I(mag^3) + depth + heatFlow + crustalThick + mantleThick +
                  elevation +strike + dip + rake, full_data, link = log)

test3 <- zeroinfl(triggerCountTh ~ mag + depth + heatFlow + crustalThick +
                    mantleThick + dip + rake_mod + strainRate, full_data, dist = "negbin")

test4 <- lm(triggerCountTh ~ mag + dip, full_data)

# Testergebnisse
test1_res <- AIC_step_koft(test1, full_data)
View(test1_res)
test2_res <- AIC_step_koft(test2, full_data)
View(test2_res)
test3_res <- AIC_step_koft(test3, full_data)
View(test3_res)
test4_res <- AIC_step_koft(test4, full_data, all_models = TRUE)
View(test4_res)



#############################################################################################################################
# 3) Full_models ############################################################################################################
#############################################################################################################################

models_var1 <- lm(triggerCountTh ~ mag + I(mag^2) + I((mag > 4.9) * ((mag - 5.0)^2)) + I((mag > 5.9) * ((mag - 6.0)^2)) +
                    I((mag > 6.9) * ((mag - 7.0)^2)) + depth + heatFlow + crustalThick +  mantleThick + elevation + strike +
                    dip + rake, full_data)

models_var2 <- lm(triggerCountTh ~ mag + I(mag^2) + I(mag^3) + depth + heatFlow + crustalThick + mantleThick + elevation +
                    strike + dip + rake, full_data)


full_models_var1 <- AIC_step_koft(models_var1, full_data, all_models = TRUE)
full_models_var2 <- AIC_step_koft(models_var2, full_data, all_models = TRUE)

full_models_step_AIC <- rbind(full_models_var1, full_models_var2)

# Speicher alle Modelle mit AIC aus der step_AIC-Prozedur ab
save(full_models_step_AIC, file = "full_models_step_AIC_03_01.Rda")

# Mit und ohne dem Tohoku-Hauptbeben
load("full_models_step_AIC_03_01.Rda")
load("full_models_step_AIC_03_01_2.Rda")
