##############################################################################################################################
# Post Besprechung mit Herrn Grimm ###########################################################################################
##############################################################################################################################

# 1) Rake-Winkel (modifizierter Wert zu (-)90°, Version 2)
# 2) Analyse Strain-Rate


# Lade notwendige Packages
library(ggplot2)
library(MASS)
library(mgcv)

# Daten laden
load("Daten/data_full_00001.Rda")


##############################################################################################################################
# 1) Rake-Winkel, V2 #########################################################################################################
##############################################################################################################################
# Modifizierter Rake
# -90 "perfekte" Abschiebung bis zu 90° "perfekte" Aufschiebung
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
head(full_data[, c("rake", "rake_mod")], n = 10)

# Kategorisiere die rake_mod Varibale
labels_rake_mod <- character(18)
for (i in seq_len(length(labels_rake_mod))) {
  labels_rake_mod[[i]] <- paste0(seq(-90, 90 , 10)[[i]], " to ", c(seq(-90, 95 , 10))[[i + 1]])
}
full_data$rakeModType <- cut(full_data$rake_mod, breaks = c(-90.1, seq(-80, 90 , 10)),
                          labels = labels_rake_mod)
head(full_data[, c("rake", "rake_mod", "rakeModType")], n =10)

# Graphisch erneut über einen zu 100% gestackten Barplot
full_data$count <- rep(1, nrow(full_data))
ggplot(data = full_data, aes(x = rakeModType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" RakeModType") + ylab("Prozentanteil") +
  theme(axis.text.x = element_text(angle = 90, size = 12))
# Boxplots
ggplot(full_data[full_data$triggerCountTh > 0, ], aes(rakeModType, triggerCountTh)) + geom_boxplot() +
  ylim(c(0,90))


# Das macht zyklisch keinen Sinn mehr einfliessen zu lassen
# Gam
test_model1 <- gam(formula = triggerCountTh ~ I(8.971e-08 * exp(2.581 * mag)) + s(rake_mod, bs = "ps"), family = nb(),
                  data = full_data)
summary(test_model1)
plot(x = test_model1, shade = TRUE, select = 1, cex.main = 0.9, main = "Glatter Effekt rake_mod)")

test_model2 <- gam(formula = triggerCountTh ~ I(8.971e-08 * exp(2.581 * mag)) + s(rake_mod, bs = "ps", by = dip),
                   data = full_data)
summary(test_model2)
plot(x = test_model2, shade = TRUE, select = 1, cex.main = 0.9, main = "Glatter Effekt rake_mod)")



##############################################################################################################################
# 2) Strain-Rate #############################################################################################################
##############################################################################################################################
summary(full_data$strainRate)

ggplot(full_data, aes(strainRate, triggerCountTh)) + geom_point()
ggplot(full_data[full_data$triggerType != "0 Nachbeben", ], aes(strainRate, triggerCountTh)) +
  geom_point()

ggplot(full_data, aes(strainRate)) +  geom_histogram(bins = 100)
ggplot(full_data, aes(strainRate, colour = triggerType)) + geom_histogram(bins = 50)
ggplot(full_data, aes(strainRate, colour = triggerType)) + geom_histogram(bins = 20)
ggplot(full_data[full_data$triggerType != "0 Nachbeben", ], aes(strainRate, colour = triggerType)) +
  geom_histogram(bins = 50)
ggplot(full_data[full_data$triggerType != "0 Nachbeben", ], aes(strainRate, colour = triggerType)) +
  geom_histogram(bins = 20)

# Kategoriale Variable fuer die grafische Analyse
# strainRate 1205.640431472 kommt 351 mal vor
full_data$strainType <- cut(full_data$strainRate, breaks = c(-0.1, 250, 500, 750, 1000, 1250, max(full_data$strainRate)),
                         labels = c("0-250", "250-500", "500-750", "750-1000", "1000-1250", "1250+"))
summary(full_data$strainType)

# Barplots
ggplot(data = full_data, aes(x = strainType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" strainType") + ylab("Prozentanteil") +
  theme(axis.text.x = element_text(angle = 90, size = 12))
# Andersrum, nicht sehr aussagekraeftig
ggplot(data = full_data, aes(x = triggerType, y = count, fill = strainType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" triggerType") + ylab("Prozentanteil") +
  theme(axis.text.x = element_text(angle = 90, size = 12))








# Curve-fitting-Toolbox von Matlab
int_fun_matlab <- function(x) {
  8.971e-08 * exp(2.581 * x)
}
