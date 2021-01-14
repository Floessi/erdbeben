##############################################################################################################################
# Post Besprechung mit Herrn Grimm ###########################################################################################
##############################################################################################################################

# 0) Spalten
# 1) Interpolation der durchschnittliche Anzahl an Nachbeben bedingt auf die Magnitude
# 2) Rake-Winkel-Variable anpassen fuer die Modelle


# Lade notwendige Packages
library(ggplot2)
library(MASS)
library(mgcv)
library(dplyr)

# Lade den Datensatz. Beachte der Datensatz muss "full_data" heissen, damit der Code durchlaeuft!
# Standard-Threshold (0.00001)
load("Daten/data_full_00001.Rda")
# Alternativ 0.0001 oder 0.000001
#load("Daten/data_full_0001.Rda")
#load("Daten/data_full_000001.Rda")

# 0)
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
 # Simple Countspalte
full_data$count <- rep(1, nrow(full_data))



##############################################################################################################################
# 1) Interpolation der durchschnittliche Anzahl an Nachbeben bedingt auf die Magnitude #######################################
##############################################################################################################################

# Grundlegende Variablen, Magnitude und triggerCountTh
ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point()
ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() + xlim(c(4, 8)) + ylim(c(0, 80))


# Aggregiere die Daten sinnvoll mit durchschnittlichen Nachbeben und Gewichten
interp_data <- cbind(aggregate(triggerCountTh ~ mag, data = full_data, FUN = mean),
                      aggregate(evID ~ mag, data = full_data, FUN = length)[, 2])
colnames(interp_data) <- c("mag", "avg_afters", "weigth")
interp_data
# Visualisiere
ggplot(interp_data, aes(x = mag, y = avg_afters, color = weigth)) + geom_point() +
  scale_color_gradient(low = "yellow", high = "red") + xlab("Magnitude") + ylab("Durchschnittliche Nachbeben")

# Finde fuer die Interpolation a*exp(alpha*mag) das alpha, warum braucht man hier nen Intercept?!
mod_temp <- glm.nb(triggerCountTh ~ mag, data = full_data)
summary(mod_temp)
# alpha = 1.74
# mod_temp <- glm.nb(triggerCountTh ~ mag + 0, data = full_data)
# summary(mod_temp)

# Finde fuer die Interpolation a*exp(alpha*mag) das a
mod_temp2 <- lm(triggerCountTh ~ I(exp(mag * 1.74)) + 0, full_data)
summary(mod_temp2)
# a = 0.00018

full_data$temp <- predict(mod_temp2)
ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(mag,temp), col = "red")

# Formel fuer Interpolation unseres exponentiellen Wachstums?
# 0.00018*exp(1.74* mag)
int_fun <- function(x) {
  0.00018*exp(1.74* x)
}

interp_data$int_avg_afers <- int_fun(interp_data$mag)
ggplot(interp_data, aes(x = mag, y = avg_afters)) + geom_point() +
  geom_line(aes(mag, int_avg_afers), col = "red")


# Mit gam?
mod_gam <- gam(formula = triggerCountTh ~ s(mag, k = 20, bs = "ps"), sp = 100, data = full_data)
summary(mod_gam)
full_data$temp <- predict(mod_gam)
ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(mag,temp), col = "red")


ggplot(interp_data, aes(x = mag, y = avg_afters)) + geom_point() +
  geom_line(aes(mag, predict(mod_gam, newdata = interp_data)), col = "red")




##############################################################################################################################
# 2) Rake-Winkel-Variable anpassen fuer die Modelle ##########################################################################
##############################################################################################################################
# Absoluter Abstand zu den naeheren (-)90°
rake_abs <- full_data$rake
for (i in seq_len(length(rake_abs))) {
  if (rake_abs[[i]] < 0) {
    rake_abs[[i]] <- abs(rake_abs[[i]] + 90)
  }
  else {
    rake_abs[[i]] <- abs(rake_abs[[i]] - 90)
  }
}

full_data$rake_abs <- rake_abs
View(full_data[, c("rake", "rake_abs")])





# Versuch Rake zyklisch als Einflussgroesse aufzunehmen
# Grafisch testen
# 1
ggplot(full_data, aes(x = rake, colour = triggerType)) + geom_histogram(bins = 50) +
  geom_line(aes(y = sin(rake * 2 * pi / 180 + 110) + 200 * cos(rake * 2 * pi / 180 + 110) + 200), col = "black")


# Wie testet man ob zyklisch-eingebrachte Variablen "besser" sind
test1 <- lm(triggerCountTh ~ I(sin(rake * 2 * pi / 180 + 110) + 200 * cos(rake * 2 * pi / 180 + 110) + 1), full_data)
test2 <- lm(triggerCountTh ~ rake_abs, full_data)
test3 <- lm(triggerCountTh ~ rake, full_data)
summary(test1)
summary(test2)


# Alternativ
labels_rake <- character(36)
for (i in seq_len(length(labels_rake))) {
  labels_rake[[i]] <- paste0(seq(-180, 180 , 10)[[i]], " to ", c(seq(-180, 185 , 10))[[i + 1]])
}
full_data$rakeType <- cut(full_data$rake, breaks = c(-180.1, seq(-170, 180 , 10)),
                   labels = labels_rake)
View(full_data[, c("rake", "rake_abs", "rakeType")])

ggplot(data = full_data, aes(x = rakeType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" RakeType") + ylab("Prozentanteil") +
  theme(axis.text.x = element_text(angle = 90, size = 12))
# Scheinbar doch nix bei den 90/-90°

# Schaue nach Zusammenhaengen mit dip
rake_data <- cbind(aggregate(dip ~ rakeType, data = full_data, FUN = mean),
                   aggregate(triggerCountTh ~ rakeType, data = full_data, FUN = mean)[, 2],
                   aggregate(triggerType ~ rakeType, data = full_data, FUN = function(x){sum(x=="0 Nachbeben")/length(x)})[, 2],
                   aggregate(triggerType ~ rakeType, data = full_data, FUN = function(x){sum(x=="1 Nachbeben")/length(x)})[, 2],
                   aggregate(triggerType ~ rakeType, data = full_data, FUN = function(x){sum(x=="2-5 Nachbeben")/length(x)})[, 2],
                   aggregate(triggerType ~ rakeType, data = full_data, FUN = function(x){sum(x=="6+ Nachbeben")/length(x)})[, 2])
colnames(rake_data) <- c("rakeType", "meanDip", "meanTC", "tt1", "tt2", "tt3", "tt4")

# Dip allein
labels_dip <- character(18)
for (i in seq_len(length(labels_dip))) {
  labels_dip[[i]] <- paste0(seq(-0, 90, 5)[[i]], " to ", c(seq(-0, 90 , 5))[[i + 1]])
}
full_data$dipType <- cut(full_data$dip, breaks = c(-0.1, seq(5, 90, 5)),
                          labels = labels_dip)

ggplot(data = full_data, aes(x = dipType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" DipType") + ylab("Prozentanteil") +
  theme(axis.text.x = element_text(angle = 90, size = 12))

##############################################################################################################################
# Aussortiertes und Versuche #################################################################################################
##############################################################################################################################

# Zu 1)

# Durchschnittliche Anzahl in den Daten
tc_mean <- aggregate(full_data$triggerCountTh, by = list(Magnitude = full_data$mag), FUN = mean)
tc_mean

# Interpoliere ueber die lm-Funktion
# Linear
mod_1 <- lm(x ~ Magnitude, tc_mean)
summary(mod_1)
tc_mean$linear <- mod_1$fitted
X <-model.matrix(x ~ Magnitude, tc_mean)
solve(t(X)%*%X)%*%t(X)%*%tc_mean$x
# Quadratisch
mod_2 <- lm(x ~ Magnitude + I(Magnitude^2), tc_mean)
summary(mod_2)
tc_mean$quadratic <- mod_2$fitted
X <-model.matrix(x ~ Magnitude + I(Magnitude^2), tc_mean)
solve(t(X)%*%X)%*%t(X)%*%tc_mean$x
# Kubisch
mod_3 <- lm(x ~ Magnitude + I(Magnitude^2) + I(Magnitude^3), tc_mean)
summary(mod_3)
tc_mean$cubic <- mod_3$fitted
X <- model.matrix(x ~ Magnitude + I(Magnitude^2) + I(Magnitude^3), tc_mean)
solve(t(X)%*%X)%*%t(X)%*%tc_mean$x
# Exponentiell
mod_4 <- lm(x ~ I(exp(Magnitude)), tc_mean)
summary(mod_4)
tc_mean$exponential <- mod_4$fitted
# Ueber ein negBin Modell
mod_5 <- glm.nb(x ~ Magnitude, tc_mean)
summary(mod_5)
tc_mean$expNegBin <- exp(predict(mod_5))

tc_mean

ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(Magnitude, linear), tc_mean, col = "green") +
  geom_line(aes(Magnitude, quadratic), tc_mean, col = "blue") +
  geom_line(aes(Magnitude, cubic), tc_mean, col = "red") +
  geom_line(aes(Magnitude, exponential), tc_mean, col = "brown") +
  geom_line(aes(Magnitude, expNegBin), tc_mean, col = "black")

mod_6 <- lm(x ~ I(exp((Magnitude - 4) * 2)), tc_mean)
summary(mod_6)
tc_mean$exponential2 <- mod_6$fitted

ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  geom_line(aes(Magnitude, exponential2), tc_mean, col = "brown")

ggplot(tc_mean, aes(x = Magnitude, y = x)) + geom_point() +
  geom_line(aes(Magnitude, exponential2), tc_mean, col = "brown")

mod_5 <- glm.nb(x ~ Magnitude, tc_mean)
summary(mod_5)
tc_mean$expNegBin <- exp(predict(mod_5))

mod_6 <- lm(x ~ I(exp((Magnitude - 4) * 2)), tc_mean)
summary(mod_6)
tc_mean$exponential2 <- mod_6$fitted

mod_7 <- lm(x ~ I(exp((Magnitude - 4) * 2)) + 0, tc_mean)
summary(mod_7)

ggplot(tc_mean, aes(Magnitude, x)) + geom_point() +
  geom_line(aes(x = tc_mean$Magnitude, y = mod_7$fitted.values))

ggplot(tc_mean, aes(Magnitude, x)) + geom_point() +
  geom_line(aes(x = tc_mean$Magnitude, y = mod_7$fitted.values)) +
  xlim(c(4, 7)) + ylim(c(0, 20))

