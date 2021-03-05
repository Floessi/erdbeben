##########################################################################################################
# Alternative Variable für die Nachbebenanzahl unabhängig von dem Distanzmaß erstellt vom 27.02.2021 #####
##########################################################################################################

# Lade benötigte Packages
library(ggplot2)
library(pscl)
library(splines)

# Lade die geupdateten Daten mit der "nAftershocks"-Spalte
load("Daten/data_full_00001_updated.Rda")


# Häufigkeitstabelle
table(full_data$nAftershocks)

# Korrelationen zur Magnitude
cor(full_data$mag, full_data$triggerCountTh)
cor(full_data[full_data$nAftershocks >= 0, ]$mag, full_data[full_data$nAftershocks >= 0, ]$nAftershocks)


# Visualisierung gegen die Magnitude
# Scatterplot mit Magnitude
ggplot(full_data[full_data$nAftershocks >= 0, ], aes(x = mag, y = nAftershocks)) + geom_point() +
  labs(title = "Magnitude~Nachbebenanzahl", x = "Magnitude", y = "Nachbebenanzahl")

# Boxplot
ggplot(full_data[full_data$nAftershocks >= 0, ], aes(x = magType, y = nAftershocks, col = magType)) +
  geom_boxplot() + theme(legend.position = "none") +
  labs(x = "kategorisierte Magnitude", y = "Nachbebenanzahl")



# Zeroinfl auf die neue Zielvariable
mod_zeroinf_full <- zeroinfl(nAftershocks ~ mag + crustalThick + heatFlow + dip + depth + bs(rake_mod) +
                               strainRate, data = full_data[full_data$nAftershocks >= 0, ],
                             dist = "negbin")
summary(mod_zeroinf_full)
AIC(mod_zeroinf_full)
plot(predict(mod_zeroinf_full), mod_zeroinf_full$residuals)

# Zeroinfl aud die alte Zielvariable
mod_zeroinf_full_old <- zeroinfl(triggerCountTh ~ mag + crustalThick + heatFlow + dip +
                               depth + bs(rake_mod) + strainRate, data = full_data,
                             dist = "negbin")
summary(mod_zeroinf_full_old)
AIC(mod_zeroinf_full_old)
plot(predict(mod_zeroinf_full_old), mod_zeroinf_full_old$residuals)



























