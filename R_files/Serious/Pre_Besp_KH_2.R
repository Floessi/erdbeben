######################################################################################################
# Pre-Besprechung mit Prof. Kuechenhoff am 28.01 #####################################################
######################################################################################################


library(ggplot2)
library(mgcv)
library(MASS)
load("Daten/data_full_00001.Rda")




######################################################################################################
# Zu ortsspezifischen Variablen mit hoher Korrelation
# heatFlow:       Inhaltlich sehr interessant
# crustalthick:   Drin lassen von den korrelierten Groessen, inhaltlich am relevantesten
# mantleThick:    Dirn lassen, falls Endmodell deutlich schlechter nur mit crustalThick
# elevation:      Entfernen aus dem Modell



######################################################################################################
# Zu Winkeln
# strike:         Relativ unrelevant (Ausrichtung zum geographischen Norden der Herdflaeche)
# dip:            Leichter linearer positiver Zusammenhang
# rake:           Modifiziert besser als zyklisch
# dip*rake:       Interaktion noch sinnvoll bzw wie zu implementieren?


# Dip:
# Temporaere Hilfsspalte
full_data$count <- rep(1, nrow(full_data))
# Intervalle fuer dip
labels_dip <- character(18)
for (i in seq_len(length(labels_dip))) {
  labels_dip[[i]] <- paste0(seq(-0, 90, 5)[[i]], " to ", c(seq(-0, 90 , 5))[[i + 1]])
}
full_data$dipType <- cut(full_data$dip, breaks = c(-0.1, seq(5, 90, 5)),
                         labels = labels_dip)
# Barplot der kategorisierten dip-Variable stacked zu 100%
ggplot(data = full_data, aes(x = dipType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" DipType") +
  ylab("Prozentanteil") + theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Prozentanteil der kategorisierten Triggerbeben in dip-Kategorie")


# Rake
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
# Graphisch erneut über einen zu 100% gestackten Barplot
full_data$count <- rep(1, nrow(full_data))
ggplot(data = full_data, aes(x = rakeModType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" RakeModType") + ylab("Prozentanteil") +
  theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Prozentanteil der kategorisierten Triggerbeben in rake_mod-Kategorie")


# Einfluss der rake_mod ins Modell
test_rake_mod <- gam(formula = triggerCountTh ~ I(8.971e-08 * exp(2.581 * mag)) +
                       s(rake_mod, bs = "ps"), family = negbin(0.183), data = full_data)
summary(test_rake_mod)
plot(x = test_rake_mod, shade = TRUE, select = 1, cex.main = 0.9, main = "Glatter Effekt rake_mod")


model_3 <- lm(triggerCountTh ~ I(exp(-9.97)*exp(1.74*mag)) - 1,full_data[-6247,])
summary(model_3)

model_4 <- lm(triggerCountTh ~ I(8.971*10^(-8)*exp(2.581*mag)) - 1,full_data)
summary(model_4)

######################################################################################################
# Zu Mag
# - Regressionsplines und andere Loesungen mangelhaft, da alles ausser ein streng monoton steigender
#   Zusammenhang inhaltlich wenig Sinn ergibt
# - Exponentieller Zusammenhang der Form a*exp(b*mag) waere gut denkbar (inhaltlich stimmig mit
#   Gutenberg-Richter Gesetz)
# - Fitting bisher nur ueber Matlab: 8.971*10^{-08}*exp(2.581*mag)
# - y = a*exp(b*x)

model_test_negBin <- glm.nb(triggerCountTh ~ mag, data = full_data)
summary(model_test_negBin)
plot(full_data$mag,exp(-9.9)*exp(1.74*full_data$mag))


######################################################################################################
# Zu StrainRake
# Histogmme
ggplot(full_data, aes(strainRate)) +  geom_histogram(bins = 100)
ggplot(full_data, aes(strainRate, colour = triggerType)) + geom_histogram(bins = 50)
# Nur Beben, die Nachbeben triggern
ggplot(full_data[full_data$triggerType != "0 Nachbeben", ], aes(strainRate, colour = triggerType)) + geom_histogram(bins = 20)
# Kategoriale Variable fuer die grafische Analyse
full_data$strainType <- cut(full_data$strainRate, breaks = c(-0.1, 250, 500, 750, 1000, 1250, max(full_data$strainRate)), labels = c("0-250", "250-500", "500-750", "750-1000", "1000-1250", "1250+"))
summary(full_data$strainType)
# Barplot
ggplot(data = full_data, aes(x = strainType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" strainType") + ylab("Prozentanteil") +
  theme(axis.text.x = element_text(angle = 90, size = 12))









