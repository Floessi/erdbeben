##############################################################################################################################
# Post Zwischenpraesentation, post Besprechung mit Prof. Kuechenhoff #########################################################
##############################################################################################################################

# Packages und Datensatz
library(ggplot2)
library(ggmosaic)
library(corrplot)
library(gridExtra)
library(MASS)
library(lattice)
library(pscl)
library(leaps)
library(png)
library(car)
# Beachte der Datensatz muss "full_data" heissen, damit der Code durchlaeuft
# Standard-Threshold (0.00001)
load("Daten/data_full_00001.Rda")
# Alternativ 0.0001 oder 0.000001
#load("Daten/data_full_0001.Rda")
#load("Daten/data_full_000001.Rda")




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

# Merke KH:
# Diese Plots zeigen zwar die Grundsaetzliche Struktur von Zaehldaten(der Zielvariable) im Datensatz, aber lassen
# allein noch keine Aussage darueber machen welcher Verteilung die Zielvariable prinzipiell folgt.
# Bspw. koennten die Kovariablen immer noch genug erklaeren, sodass die Zielvariable normalverteilt sein kann


# Fuer weitere Analyse unterteile die Zielvariable in Kategorien #############################################################
triggerType <- character()
for (i in seq_len(nrow(full_data))) {
  if (full_data$triggerCountTh[[i]] == 0) {
    triggerType[[i]] <- "0 Nachbeben"
  }
  else if (full_data$triggerCountTh[[i]] == 1) {
    triggerType[[i]] <- "1 Nachbeben"
  }
  else if (full_data$triggerCountTh[[i]] >= 2 & full_data$triggerCountTh[[i]] <= 5) {
    triggerType[[i]] <- "2-5 Nachbeben"
  }
  else {
    triggerType[[i]] <- "6+ Nachbeben"
  }
}
full_data$triggerType <- factor(triggerType, levels = c("0 Nachbeben", "1 Nachbeben", "2-5 Nachbeben", "6+ Nachbeben"))



# Anteile der Trigger-Beben ##################################################################################################
# FALSE: triggert kein Nachbeben
table(full_data$willTrigger)


# Sind die Anteile der Triggerbeben unter verschiedenen Bereichen der Magnitude unterschiedlich ##############################
# Unterteile hierfuer die Magnituden in 4 Level:
# 4.0-4.9,
# 5.0-5.9,
# 6.0-6.9,
# ab 7.0

# Spalte fuer die Kategorisierung der Magnitude
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

# Relative und absolute Anteile der Triggerbeben in den Kategorien
AnteileTriggerBeben <- data.frame("keinNachbeben" = c(table(full_data$magType, full_data$willTrigger)[, 1],
                                                      sum(table(full_data$magType, full_data$willTrigger)[, 1])),
                                  "TriggerBeben" = c(table(full_data$magType, full_data$willTrigger)[, 2],
                                                     sum(table(full_data$magType, full_data$willTrigger)[, 2])),
                                  "Sum" = c(table(full_data$magType), nrow(full_data)),
                                  row.names = c(levels(full_data$magType), "Sum"))

# Spalte fuer die absolute Haeufigkeit der Magnitudenkategorie
magTypeCount <- numeric()
for (i in seq_len(nrow(full_data))) {
  if (full_data$magType[[i]] == levels(full_data$magType)[[1]]) {
    magTypeCount[[i]] <- AnteileTriggerBeben[, 3][[1]]
  }
  else if (full_data$magType[[i]] == levels(full_data$magType)[[2]]) {
    magTypeCount[[i]] <- AnteileTriggerBeben[, 3][[2]]
  }
  else if (full_data$magType[[i]] == levels(full_data$magType)[[3]]) {
    magTypeCount[[i]] <- AnteileTriggerBeben[, 3][[3]]
  }
  else {
    magTypeCount[[i]] <- AnteileTriggerBeben[, 3][[4]]
  }
}
full_data$magTypeCount <- magTypeCount

AnteileTriggerBeben$relativerAnteil_Triggerbeben <- round(AnteileTriggerBeben$TriggerBeben/AnteileTriggerBeben$Sum,
                                                          digits = 2)
AnteileTriggerBeben

# Barplot
ggplot(data = full_data, aes(x = magType, y = lat, fill = willTrigger)) +
  geom_bar(position="fill", stat="identity") + xlab(" Kategorie der Magnitude") + ylab("Prozentanteil")

# Mosaikplot
ggplot(full_data, aes(x = magType, y = magTypeCount, width = magTypeCount, fill = willTrigger)) +
  geom_bar(stat = "identity", position = "fill") + facet_grid(~magType, scales = "free_x", space = "free_x") +
  theme(panel.background = element_blank(), panel.spacing.x = unit(0.002, "npc"), strip.background = element_blank(),
        strip.text = element_blank(), axis.text.x = element_text(angle = 90, size = 12, face = "bold"),
        axis.text.y = element_text(angle = 90, size = 12, face = "bold"), axis.title=element_text(size=14)) +
  xlab("Kategorie der Magnitude") + ylab("Relativer Anteil innerhalb einer Kategorie")

# Mit log() der Breite
ggplot(full_data, aes(x = magType, y = magTypeCount, width = log(magTypeCount), fill = willTrigger)) +
  geom_bar(stat = "identity", position = "fill") + facet_grid(~magType, scales = "free_x", space = "free_x") +
  theme(panel.background = element_blank(), panel.spacing.x = unit(0.002, "npc"), strip.background = element_blank(),
        strip.text = element_blank(), axis.text.x = element_text(angle = 90, size = 12, face = "bold"),
        axis.text.y = element_text(angle = 90, size = 12, face = "bold"), axis.title=element_text(size=14)) +
  xlab("Kategorie der Magnitude") + ylab("Relativer Anteil innerhalb einer Kategorie")




# Nochmal Winkel geplottet ###################################################################################################
# Strike
ggplot(full_data, aes(x = strike, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = strike, colour = triggerType)) + geom_histogram(bins = 50)
ggplot(full_data[full_data$triggerCountTh > 0, ], aes(x = strike, colour = triggerType)) + geom_histogram(bins = 50)

# Dip
ggplot(full_data, aes(x = dip, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = dip, colour = triggerType)) + geom_histogram(bins = 50)
ggplot(full_data[full_data$triggerCountTh > 0, ], aes(x = dip, colour = triggerType)) + geom_histogram(bins = 50)

# Rake
ggplot(full_data, aes(x = rake, colour = willTrigger)) + geom_histogram(bins = 50)
ggplot(full_data, aes(x = rake, colour = triggerType)) + geom_histogram(bins = 50)
ggplot(full_data[full_data$triggerCountTh > 0, ], aes(x = rake, colour = triggerType)) + geom_histogram(bins = 50)

# Dichten Strike
table(full_data$triggerType)
grid.arrange(ggplot(full_data[full_data$triggerType == "0 Nachbeben", ], aes(x = strike)) + geom_density() +
               ggtitle("0 Nachbeben"),
             ggplot(full_data[full_data$triggerType == "2-5 Nachbeben", ], aes(x = strike)) + geom_density() +
               ggtitle("2-5 Nachbeben"),
             ggplot(full_data[full_data$triggerType == "1 Nachbeben", ], aes(x = strike)) + geom_density() +
               ggtitle("1 Nachbeben"),
             ggplot(full_data[full_data$triggerType == "6+ Nachbeben", ], aes(x = strike)) + geom_density() +
               ggtitle("6+ Nachbeben"),
             nrow = 2)


# Versuch Strike zyklisch als Einflussgroesse aufzunehmen
# Grafisch testen
# 1
ggplot(full_data, aes(x = strike, colour = triggerType)) + geom_histogram(bins = 50) +
  geom_line(aes(y = sin(strike * 2 * pi / 200) + 200 * cos(strike * 2 * pi / 200) + 200, col = "green"))
# 2
ggplot(full_data, aes(x = strike, colour = triggerType)) + geom_histogram(bins = 50) +
  geom_line(aes(y = sin(strike * 2 * pi / 360 - pi) + 200 * cos(strike * 2 * pi / 360 - pi) + 200, col = "green"))


# Wie testet man ob zyklisch-eingebrachte Variablen "besser" sind
test1 <- lm(triggerCountTh ~ I(sin(strike * 2 * pi / 200) + cos(strike * 2 * pi / 200) + 1), full_data)
test2 <- lm(triggerCountTh ~ I(sin(strike * 2 * pi / 360 - pi) + cos(strike * 2 * pi / 360 - pi) + 1), full_data)
test3 <- lm(triggerCountTh ~ strike, full_data)
summary(test1)
summary(test2)
summary(test3)




# Plots zu den geographischen Groessen #######################################################################################
# Teil unserer Fragestellungen:
# In welchen Regionen kann man ein ueber unterdurchschnittliches Triggering beobachten
# Konzentration aller Beben (Trigger- oder Nichttrigger- Beben) um die Faults zwischen den drei grossen tektonischen Platten
# in dieser Region


# Ueberdurchschnittliches Triggering, welches ? ##############################################################################
# Unter allen Beben (Schon ein Nachbeben waere ueberdurchschnittlich)
mean(full_data$triggerCountTh)
# oder nur unter den Triggerbeben (Ab 3 Nachbeben waere das Triggering ueberdurchschnittlich)
mean(full_data[full_data$willTrigger,]$triggerCountTh)


# Zur Veranschaulichung wo die Plattengrenzen liegen #########################################################################
pp <- readPNG("Dokumente/Plates.png")
plot.new(); rasterImage(pp,0,0,0.6,1)

pp2 <- readPNG("Dokumente/HeatFLow.png")
plot.new(); rasterImage(pp2,0,0,1,1)


# Plots der geographischen Groessen mit den zwei unterschiedlichen Definitionen des ueberdurchschnittlichen Triggerings ######
grid.arrange(
  ggplot(full_data[], aes(x = lon, y = lat)) + geom_point(aes(colour = heatFlow)) +
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


# Prinzipiell nur die Nachbebenanzahl als Kategorie grafisch ueber die geographische Lage geplottet ##########################
ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = triggerType, size = triggerType)) +
  scale_size_manual(values=c(0.5, 1, 2, 3)) + scale_color_manual(values=c("80F369", "#FFEF00", "#FF9100", "#FF0000")) +
  labs(title = "Alle Beben")


grid.arrange(ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = triggerType, size = triggerType)) +
               scale_size_manual(values=c(0.5, 1, 2, 3)) +
               scale_color_manual(values=c("80F369", "#FFEF00","#FF9100", "#FF0000")) + labs(title = "Alle Beben"),
             ggplot(full_data[!(full_data$triggerType == "0 Nachbeben") ],
                    aes(x = lon, y = lat)) + geom_point(aes(colour = triggerType, size = triggerType)) +
               scale_size_manual(values=c(2, 3)) + scale_color_manual(values=c("#FFEF00", "#FF9100", "#FF0000")) +
               labs(title = "Alle Beben"))





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
# Magnitude sinnvoll einfliessen lassen ######################################################################################
##############################################################################################################################

# Polynome in einem linearen Modell
# 3. Grad
model_p_3 <- lm(triggerCountTh ~ mag + I(mag^2) + I(mag^3), full_data)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x,3))
# 5. Grad
model_p_5 <- lm(triggerCountTh ~ mag + I(mag^2) + I(mag^3) + I(mag^4) + I(mag^5), full_data)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x,5))

summary(model_p_3)
summary(model_p_5)

full_data$predicted <- predict(model_p_5)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red") + xlim(6,7) + ylim(0, 40)
# Wie bewertet man Overfitting? (Kreuzvalidierung?)


# Stetige Splines
# Schnittstellen wie in der vorherigen Kategorisierung
model_s <- lm(triggerCountTh ~ mag + I((mag > 4.9) * (mag - 5.0)) + I((mag > 5.9) * (mag - 6.0)) +
                I((mag > 6.9) * (mag - 7.0)), full_data)
summary(m_s_2)
full_data$predicted <- predict(model_s_)
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red")
ggplot(data = full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(y = predicted), col = "red") + xlim(6,7) + ylim(0, 40)




model <- zeroinfl(triggerCountTh ~ I(0.00018*exp(1.74* mag)) + depth + heatFlow +
                    crustalThick + mantleThick + elevation + strike + dip + rake, full_data, dist = "negbin" )

model <- zeroinfl(triggerCountTh ~ I(0.00018*exp(1.74* mag))+ depth, full_data, dist = "negbin" )
summary(model)

model2 <- glm.nb(triggerCountTh ~ I(0.00018*exp(1.74* mag)) + depth + heatFlow +
                    crustalThick + mantleThick + elevation + strike + dip + rake + 0, full_data)
