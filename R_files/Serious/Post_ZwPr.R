#############################################################################################################
# Ergebnisse der Anregungen, Fragen (etc.) der Zwischenpraesentation ########################################
############################################################################################################

# Packages und Datensatz
library(ggplot2)
library(corrplot)
library(gridExtra)
library(MASS)
library(lattice)
library(pscl)
library(leaps)
library(png)
library(car)
# Beachte der Datensatz muss "full_data" heissen, damit der Code durchlaeuft
load("Daten/data_full_00001.Rda")




##############################################################################################################################
# Grafiken, die manche der Fragen abdecken bzw. fuer welche Interesse im Zwischenvortrag entstand ############################
##############################################################################################################################


# Verteilung der Zielvariable allgemein ######################################################################################

ggplot(data = as.data.frame(table(full_data[full_data$triggerCountTh <= 10 & full_data$triggerCountTh > 0, ]$triggerCountTh)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + xlab("Anzahl direkter Nachbeben") +
  ylab("Absolute Häufigkeit") + labs(title = "Häufigkeit von Beben mit\nzwischen 1-10 Nachbeben") +
  theme(plot.title = element_text(size = 10))

ggplot(data = as.data.frame(table(full_data[full_data$triggerCountTh <= 10, ]$triggerCountTh)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + xlab("Anzahl direkter Nachbeben") +
  ylab("Absolute Häufigkeit") + labs(title = "Häufigkeit von Beben mit\nzwischen 0-10 Nachbeben") +
  theme(plot.title = element_text(size = 10))

ggplot(data = as.data.frame(table(full_data$triggerCountTh)), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") + xlab("Anzahl direkter Nachbeben") +
  ylab("Absolute Häufigkeit") + labs(title = "Häufigkeit von Beben nach\nder Anzahl ihrer Nachbeben") +
  theme(plot.title = element_text(size = 10))



# Anteile der Trigger-Beben ##################################################################################################
# FALSE: triggert kein Nachbeben
table(full_data$willTrigger)



# Verteilung der Triggerbeben/Beben ohne Nachbeben uber die Magnitude ########################################################
plot(density(full_data[!full_data$willTrigger,]$mag, from = 3, to = 9),
     xlab = "Alter in Jahrezehnten", xlim = c(3, 9), ylim = c(0, 1.7),
     ylab = "Wahrscheinlichkeitsdichte", main = "Kerndichteschaetzung nach willTrigger")
lines(density(full_data[full_data$willTrigger,]$mag, from = 3, to =9), col = "red")
legend(  "topright", title = "Triggert Nachbeben", legend = c("Nein", "Ja"), col = c("black", "red"), lty = 1)



# Sind die Anteile der Triggerbeben unter verschiedenen Bereichen der Magnitude unterschiedlich ##############################
# Unterteile hierfuer die Magnituden in 4 Level:
# 4.0-4.9,
# 5.0-5.9,
# 6.0-6.9,
# ab 7.0
full_data$count <- rep(1, nrow(full_data))
magType <- character()
for (i in seq_len(nrow(full_data))) {
  if (full_data$mag[[i]] >= 4.0 & full_data$mag[[i]] <= 4.9) {
    magType[[i]] <- "4.0-4.9"
  }
  else if (full_data$mag[[i]] >= 5.0 & full_data$mag[[i]] <= 5.9) {
    magType[[i]] <- "5.0-5.9"
  }
  else if (full_data$mag[[i]] >= 6.0 & full_data$mag[[i]] <= 6.9) {
    magType[[i]] <- "6.0-6.9"
  }
  else {
    magType[[i]] <- "ab 7.0"
  }
}
full_data$magType <- factor(magType, levels = c("4.0-4.9", "5.0-5.9", "6.0-6.9", "ab 7.0"))

AnteileTriggerBeben <- data.frame("keinNachbeben" = c(table(full_data$magType, full_data$willTrigger)[, 1],
                                                      sum(table(full_data$magType, full_data$willTrigger)[, 1])),
                                  "TriggerBeben" = c(table(full_data$magType, full_data$willTrigger)[, 2],
                                                     sum(table(full_data$magType, full_data$willTrigger)[, 2])),
                                  "Sum" = c(table(full_data$magType), nrow(full_data)),
                                  row.names = c(levels(full_data$magType), "Sum"))
AnteileTriggerBeben$relativerAnteil_Triggerbeben <- round(AnteileTriggerBeben$TriggerBeben/AnteileTriggerBeben$Sum,
                                                          digits = 2)

ggplot(data = full_data, aes(x = magType, y = count, fill = willTrigger)) +
  geom_bar(position="fill", stat="identity") + xlab("Magnitude") + ylab("Prozentanteil")
AnteileTriggerBeben



# Nochmal Winkel geplottet ###################################################################################################
ggplot(full_data, aes(x = strike, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = dip, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = rake, colour = willTrigger)) + geom_histogram(bins = 50)


c("Keine Nachbeben" = nrow(full_data[!full_data$willTrigger, ]), "Nachbeben > 0" = nrow(full_data[full_data$willTrigger, ]),
  "Nachbeben 1-5" = nrow(full_data[full_data$triggerCountTh <= 5 & full_data$triggerCountTh > 0, ]),
  "Nachbebenanzahl > 5" = nrow(full_data[full_data$triggerCountTh > 5, ]))

grid.arrange(ggplot(full_data[!full_data$willTrigger, ], aes(x = strike)) + geom_density() + ggtitle("Keine Nachbeben"),
             ggplot(full_data[full_data$triggerCountTh <= 5 & full_data$triggerCountTh > 0, ],
                    aes(x = strike)) + geom_density() + ggtitle("Nachbebenanzahl 1-5"),
             ggplot(full_data[full_data$willTrigger, ], aes(x = strike)) + geom_density() + ggtitle("Nachbeben > 0"),
             ggplot(full_data[full_data$triggerCountTh > 5, ],
                    aes(x = strike)) + geom_density() + ggtitle("Nachbebenanzahl > 5"),
             nrow = 2)
grid.arrange(ggplot(full_data[!full_data$willTrigger, ], aes(x = dip)) + geom_density() + ggtitle("Keine Nachbeben"),
             ggplot(full_data[full_data$triggerCountTh <= 5 & full_data$triggerCountTh > 0, ],
                    aes(x = dip)) + geom_density() + ggtitle("Nachbebenanzahl 1-5"),
             ggplot(full_data[full_data$willTrigger, ], aes(x = dip)) + geom_density() + ggtitle("Nachbeben > 0"),
             ggplot(full_data[full_data$triggerCountTh > 5, ], aes(x = dip)) +
               geom_density() + ggtitle("Nachbebenanzahl > 5"),
             nrow = 2)

grid.arrange(ggplot(full_data[!full_data$willTrigger, ], aes(x = rake)) + geom_density() + ggtitle("Keine Nachbeben"),
             ggplot(full_data[full_data$triggerCountTh <= 5 & full_data$triggerCountTh > 0, ],
                    aes(x = rake)) + geom_density() + ggtitle("Nachbebenanzahl 1-5"),
             ggplot(full_data[full_data$willTrigger, ], aes(x = rake)) + geom_density() + ggtitle("Nachbeben > 0"),
             ggplot(full_data[full_data$triggerCountTh > 5, ], aes(x = rake)) +
               geom_density() + ggtitle("Nachbebenanzahl > 5"),
             nrow = 2)



# Plots zu den geographischen Groessen #######################################################################################
# Teil unserer Fragestellungen:
# In welchen Regionen kann man ein ueber unterdurchschnittliches Triggering beobachten
# Konzentration aller Beben (Trigger- oder Nichttrigger- Beben) um die Faults zwischen den drei grossen tektonischen Platten
# in dieser Region


# Ueberdurchschnittliches Triggering, welches ? ##############################################################################
# Unter allen Beben (Schon ein Nachbeben waere ueberdurchschnittlich)
mean(full_data[,"triggerCountTh"])
# oder nur unter den Triggerbeben (Ab 3 Nachbeben waere das Triggering ueberdurchschnittlich)
mean(full_data[full_data$willTrigger,"triggerCountTh"])


# Zur Veranschaulichung wo die Plattengrenzen liegen #########################################################################
pp <- readPNG("Dokumente/Plates.png")
plot.new(); rasterImage(pp,0,0,0.6,1)

pp2 <- readPNG("Dokumente/HeatFLow.png")
plot.new(); rasterImage(pp2,0,0,1,1)


# Plots der geographischen Groessen mit den zwei unterschiedlichen Definitionen des ueberdurchschnittlichen Triggerings ######
grid.arrange(
  ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = heatFlow)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben"),
  ggplot(full_data[full_data$willTrigger, ], aes(x = lon, y = lat)) + geom_point(aes(colour = heatFlow)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Trigger-Beben"),
  ggplot(full_data[full_data$triggerCountTh > 2, ], aes(x = lon, y = lat)) + geom_point(aes(colour = heatFlow)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben mit Nachbeben > 2"),
  ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = crustalThick)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben"),
  ggplot(full_data[full_data$willTrigger, ], aes(x = lon, y = lat)) + geom_point(aes(colour = crustalThick)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Trigger-Beben"),
  ggplot(full_data[full_data$triggerCountTh > 2, ], aes(x = lon, y = lat)) + geom_point(aes(colour = crustalThick)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben mit Nachbeben > 2"),
  nrow = 2
)

grid.arrange(
  ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = mantleThick)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben"),
  ggplot(full_data[full_data$willTrigger, ], aes(x = lon, y = lat)) + geom_point(aes(colour = mantleThick)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Trigger-Beben"),
  ggplot(full_data[full_data$triggerCountTh > 2, ], aes(x = lon, y = lat)) + geom_point(aes(colour = mantleThick)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben mit Nachbeben > 2"),
  ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = elevation)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben"),
  ggplot(full_data[full_data$willTrigger, ], aes(x = lon, y = lat)) + geom_point(aes(colour = elevation)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Trigger-Beben"),
  ggplot(full_data[full_data$triggerCountTh > 2, ], aes(x = lon, y = lat)) + geom_point(aes(colour = elevation)) +
    scale_color_gradient(low = "blue", high = "yellow") + labs(title = "Alle Beben mit Nachbeben > 2"),
  nrow = 2
)



##############################################################################################################################
# Untersuche Korrelationen ###################################################################################################
##############################################################################################################################

# Korrelationsmatrizen fuer geographische, physikalische Groessen
cor(as.matrix(full_data[, c("mag", "depth", "heatFlow", "crustalThick", "mantleThick", "elevation", "triggerCountTh")]))

# Visuell
corrplot(cor(as.matrix(full_data[, c("mag", "depth", "heatFlow", "crustalThick",
                                     "mantleThick", "elevation", "triggerCountTh")])),
         type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(cor(full_data[, c("mag", "depth", "heatFlow", "crustalThick", "mantleThick", "elevation",
                           "strike", "dip", "rake", "triggerCountTh")]), method = "color", order = "hclust")
# heatFlow, crustalThick und elevation sind alle relativ stark positiv miteinander korreliert
# mantleThcik ist mit allen drei relativ stark negativ korreliert
# Theoriefragen an Grimm:
# Dickere Mantelschicht => weniger Waerme kommt durch
# Dickere Mantelschicht => weniger Platz fuer die Krustenschicht
# Dickere Mantelschicht => warum elevation negativ und nicht positiv korreliert?!


# Korrelationen zur Zielgroesse ##############################################################################################
cor(as.matrix(full_data[, c("mag", "depth", "heatFlow", "crustalThick", "mantleThick", "elevation",
                            "strike", "dip", "rake", "triggerCountTh")]))[, 10]



##############################################################################################################################
# Versuche ein gutes Modell nur mit der Magnitude zu finden ##################################################################
##############################################################################################################################
# Teil unserer Fragestellungen:
# Wie viele direkte Nachbeben triggert ein Erdbeben , abhaengig von seiner Magnitude, im Durchschnitt
# Scatterplot
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  xlab("Magnitude") + ylab("Anzahl der Nachbeben")
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() + xlim(4, 7.5) +
  ylim(0, 65) + xlab("Magnitude") + ylab("Anzahl der Nachbeben")


# Plotte die durchschnittliche Nachbebenanzahl gegen die Magnitude
aggregate(full_data$triggerCountTh,  by = list(magID = full_data$mag), FUN = mean)
xyplot(x ~ magID,
       data = aggregate(full_data$triggerCountTh,  by = list(magID = full_data$mag), FUN = mean),
       type = "l", xlab = "Magnitude", ylab = "Durchschnittliche Nachbebenanzahl",
       main = "Gesamter Bereich der Magnitude")

xyplot(x ~ magID,
       data = aggregate(full_data$triggerCountTh,  by = list(magID = full_data$mag), FUN = mean),
       type = "l", xlab = "Magnitude", ylab = "Durchschnittliche Nachbebenanzahl",
       xlim = c(4, 6.5), ylim = c(0,5), main = "Magnitude 4 - 6.5")
# Grober mit der Kategoriesierung vom Anfang
aggregate(full_data$triggerCountTh,  by = list(magID = full_data$magType), FUN = mean)



# Lineare Modelle ############################################################################################################
# Polynome in einem linearen Modell
m_p_1 <- lm(triggerCountTh ~ mag, full_data)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  stat_smooth(method = "lm", se = TRUE, formula = y ~ x)

m_p_2 <- lm(triggerCountTh ~ mag + I(mag^2), full_data)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x,2))

m_p_3 <- lm(triggerCountTh ~ mag + I(mag^2) + I(mag^3), full_data)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x,3))

m_p_4 <- lm(triggerCountTh ~ mag + I(mag^2) + I(mag^3) + I(mag^4), full_data)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x,4))

m_p_5 <- lm(triggerCountTh ~ mag + I(mag^2) + I(mag^3) + I(mag^4) + I(mag^5), full_data)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x,5))

summary(m_p_1)
summary(m_p_2)
summary(m_p_3)
summary(m_p_4)
summary(m_p_5)

full_data$predicted <- predict(m_p_5)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red") + xlim(6,7) + ylim(0, 40)
# Wie bewertet man Overfitting? (Kreuzvalidierung?)


# Stetige Splines
# Eine Schnittstelle
m_s_1 <- lm(triggerCountTh ~ mag + I((mag > 5.9) * (mag - 6.0)), full_data)
summary(m_s_1)
full_data$predicted <- predict(m_s_1)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")

# Schnittstellen wie in der vorherigen Kategorisierung
m_s_2 <- lm(triggerCountTh ~ mag + I((mag > 4.9) * (mag - 5.0)) + I((mag > 5.9) * (mag - 6.0)) +
                I((mag > 6.9) * (mag - 7.0)), full_data)
summary(m_s_2)
full_data$predicted <- predict(m_s_2)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")

# Schnittstellen wie in der vorherigen Kategorisierung und mag^2
m_s_3 <- lm(triggerCountTh ~ mag + I(mag^2) + I((mag > 4.9) * ((mag - 5.0)^2)) + I((mag > 5.9) * ((mag - 6.0)^2)) +
                I((mag > 6.9) * ((mag - 7.0)^2)), full_data)
summary(m_s_3)
full_data$predicted <- predict(m_s_3)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")



# Negativ-Binomialmodell #####################################################################################################

# Ganz basic
m_nb_1 <- glm.nb(triggerCountTh ~ mag , full_data, link = log)
summary(m_nb_1)
full_data$predicted <- predict(m_nb_1)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")

# Vergleich mit linearen Modellen mit hoeheren Polynomen und Splines
BIC(m_nb_1)
BIC(m_p_5)
BIC(m_s_3)

# Polynome und Splines sind uneffektiv in Modellen fuer Count-Data, warum?
# Weil Magnitude eh schon anders ueber die link-function auf Zeilgroesse Einfluss nimmt?



##############################################################################################################################
# Negatives-Binomialmodell und Zeroinflated negative-binomial-model ##########################################################
##############################################################################################################################
# Count-Data und der Fakt Var(y)>Mean(y) => negative Binomialverteilung


# Normales NegBin Modell #####################################################################################################

model_negbin_1 <- glm.nb(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick + elevation +
                           strike + dip + rake, full_data, link = log)
summary(model_negbin_1)
AIC(model_negbin_1)
BIC(model_negbin_1)
# Devianzanalyse (sequenziell), Teilfragestellung: Hat die Variable „dip“ einen Einfluss auf Triggering?
anova(model_negbin_1)
# Devianzanalyse (Variablenweise)
Anova(model_negbin_1)
# dip hat einen signifikanten Einfluss auf die Devianz, aber verhaeltnismaessig gering


# Mit nur einer der 4 stark korrelierten Kovariablen
model_negbin_2 <- glm.nb(triggerCountTh ~ mag + depth + heatFlow + strike + dip + rake, full_data, link = log)
summary(model_negbin_2)
AIC(model_negbin_2)
BIC(model_negbin_2)
# Geht relativ viel Modellguete verloren
anova(model_negbin_1, model_negbin_2)



# Zeroinflated NegBin Modell #################################################################################################

model_zfnegbin_1 <- zeroinfl(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                               elevation + strike + dip + rake | mag, full_data, dist = "negbin")
summary(model_zfnegbin_1)
AIC(model_zfnegbin_1)
AIC(model_negbin_1)
BIC(model_zfnegbin_1)
BIC(model_negbin_1)
# Auf BIC getestet ist das negBin Modell besser als Zeroinf (mit mag) negBin (signifikant)
vuong(model_negbin_1, model_zfnegbin_1)



model_zfnegbin_2 <- zeroinfl(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                               elevation + strike + dip + rake , full_data, dist = "negbin")
summary(model_zfnegbin_2)
AIC(model)
AIC(model_zfnegbin_2)
AIC(model_negbin_1)
BIC(model_zfnegbin_2)
BIC(model_negbin_1)
# Auf BIC getestet ist das negBin Modell besser als Zeroinf (mit allen Variablen) negBin (signifikant), aber
# Auf AIC getestet andersrum
vuong(model_negbin_1, model_zfnegbin_2)


# In sqrt(diag(object$vcov)) : NaNs wurden erzeugt
# 3 Variablen NA wegen ihrer starken Korrelation zu heatFlow?
# Welche Variable rauslassen?
Anova(model_zfnegbin_2)




# Resiuden des besten Modells (zeroinfl, da AIC) ############################################################################
# Plotte die wahren Werten und die gefitteten ueber die Magnitude
full_data$fitted_values <- predict(model_zfnegbin_2)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")



# wahre y
true_values <- full_data$triggerCountTh
# y dach
fitted_values <- full_data$fitted_values


# Gehe die verschiedenen Residuenarten durch
# Response
ggplot(full_data, aes(x = evID, y = triggerCountTh - fitted_values)) + geom_point() + ylab("response-Residuen")

# Pearson
full_data$pearson_residuals <- residuals(model_zfnegbin_2, type = "pearson")
ggplot(full_data, aes(x = evID, y = pearson_residuals)) + geom_point() + ylab("Pearson-Residuen")




# Plots aus der Zwischenpraesentation
# Residuen vs Fitted
plot(fitted(model_zfnegbin_2), residuals(model_zfnegbin_2, type = "pearson"), main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

# Wie in der Praesentation
plot(fitted(model_zfnegbin_2), residuals(model_zfnegbin_2, type = "pearson"), main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals", xlim = c(0, 5))
abline(h = 0, lty = 2)

# Residuen vs Magnitude
plot(full_data$mag, residuals(model_zfnegbin_2, type = "pearson"), main = "Residuals vs Magnitude",
     xlab = "Fitted Values", ylab = "Residuals")

































# Nicht-Gegluecktes
# Schrittweise Variablenselektion
model_0 <- zeroinfl(triggerCountTh ~ mag | mag, full_data, dist = "negbin")
summary(model_0)

model_full <- zeroinfl(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                      elevation + strike + dip + rake | mag, full_data, dist = "negbin")
summary(model_full)
stepAIC(model_0, scope = list(upper = model_full, lower = model_0), direction = c("forward"))

# Nur mit negBin
model_0_2 <- glm.nb(triggerCountTh ~ mag, full_data)
summary(model_0_2)

model_full_2 <- glm.nb(triggerCountTh ~ mag + depth + heatFlow + crustalThick + mantleThick +
                         elevation + strike + dip + rake, full_data)
summary(model_full_2)
stepAIC(model_0_2, scope = list(upper = model_full_2, lower = model_0_2), direction = c("forward"))
stepAIC(model_0_2, scope = list(upper = model_full_2, lower = model_0_2), direction = c("both"))

# Mit leaps-package
temp <- regsubsets(triggerCountTh ~ mag + I(mag^2) + I((mag > 4.9) * ((mag - 5.0)^2)) + I((mag > 5.9) * ((mag - 6.0)^2)) +
                     I((mag > 6.9) * ((mag - 7.0)^2)) + depth + heatFlow + crustalThick + mantleThick +
                     elevation + strike + dip + rake, data = full_data, nvmax = 14)

plot(1:13, summary(temp)$bic, xlab = "Zahl der Variablen", ylab = "Bic")



