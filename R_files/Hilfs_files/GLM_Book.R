############
# GLM Book #
############


# Daten laden und auch eine Variante ohne das Tohoku Hauptbeben
load("Daten/data_full_00001.Rda")
daten <- full_data
daten_ohne_To <- full_data[- 6247, ]


# Kapitel 2, Lineare Regression ############################################################

# Plotte die Zielvariable (allein)
hist(daten$triggerCountTh, breaks = 500, xlab = "Anzahl direkter Nachbeben" ,
     xlim = c(0, 5), main = "Histogramm")

# Plotte die Zielvariable gegen die Magnitude
plot(triggerCountTh ~ mag, data = daten, las=1, pch = ifelse(wasTriggered, 1, 19),
      xlab = "Magnitude", ylab = "Anzahl direkter Nachbeben")
# Limitierter y-Achsenbereich
plot(triggerCountTh ~ mag, data = daten, las=1, pch = ifelse(wasTriggered, 1, 19),
     xlab = "Magnitude", ylab = "Anzahl direkter Nachbeben", ylim = c(0, 80))
# Plotte die logarithmierte Zielvariable gegen die Magnitude
plot(log(triggerCountTh) ~ mag, data = daten, las=1, pch = ifelse(wasTriggered, 1, 19),
     xlab = "Magnitude", ylab = "Anzahl direkter Nachbeben")

