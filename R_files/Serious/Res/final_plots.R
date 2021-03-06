########################################################################################
# Finale Visualisierungen ##############################################################
########################################################################################

# Lade noetige Packages
library(ggplot2)
library(dplyr)
library(png)
library(grid)
library(data.table)
library(gridExtra)
library(mapdata)

# Lade die geupdateten Daten vom 27.02.2021
load("Daten/data_full_00001_updated.Rda")

# Daten zum aggregieren
heat_data <- copy(full_data)
heat_data$lon <- floor(heat_data$lon) + 0.5
heat_data$lat <- floor(heat_data$lat) + 0.5
heat_data_res <- cbind(aggregate(formula = heatFlow~lon+lat, data = heat_data, FUN = mean),
                       aggregate(formula = crustalThick~lon+lat, data = heat_data, FUN = mean)[, 3],
                       aggregate(formula = mantleThick~lon+lat, data = heat_data, FUN = mean)[, 3],
                       aggregate(formula = heatFlow~lon+lat, data = heat_data, FUN = length)[, 3])
colnames(heat_data_res) <- c("long", "lat", "heatFlow", "crustalThick", "mantleThick", "count")
full_data$count <- rep(1, nrow(full_data))

# Relevant zum jetztigen Stand fuer ein triggerCountTh-Modell:

# mag:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen,
#               a*exp(b*mag) waere denkbar; a ist im Intercept enthalten
#               und b ist dann die Koeffizient fuer mag
# depth:        kommt drauf an ob das im vollen Modell signifikant wird
# heatFlow:     einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# crustalThick: einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# mantleThick:  kommt drauf an ob das im vollen Modell signifikant wird
# dip:          einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen
# rake_mod:     mit Spline oder Polynom einfliessen lassen, siehe stacked-Barplot
# strainRate:   einfach ohne Zusatzfuntion in negBin-Modelle aufnehmen


# Hoehe der Grafiken 600p
myTheme <- theme(plot.title = element_text(hjust = 0.5, size = 14),
                 axis.text.x = element_text(size = 11),
                 axis.text.y = element_text(size = 11))

japan <- map_data("japan")


# Tohoku-Cluster-Plot
ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) +
  geom_point(data = full_data[full_data$clusterCount > 300, ], aes(x = lon, y = lat),
             col = "red", alpha = 0.55) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2), limits = c(127, 146)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2), limits = c(27, 46)) +
  ggtitle("Erdbebencluster des Tohoku-Bebens") + myTheme

# Zielvariablen-Plot
target_freq <- as.data.frame(table(full_data$triggerCountTh))
ggplot(data = target_freq, aes(x = Var1, y = log(Freq+0.1))) +
  geom_bar(stat = "identity") + xlab("Anzahl direkter Nachbeben") +
  labs(title = "Logarithmierte Häufigkeit der Nachbebenanzahl",
       y = "Logarithmierte absolute Häufigkeit") + myTheme +
  scale_x_discrete(breaks = c(1:9,10,13,18,26,32,45,507))

# Magnitude-Boxplot
ggplot(full_data, aes(x = magType, y = log(triggerCountTh), col = magType)) +
  geom_boxplot() + theme(legend.position = "none",
                         plot.title = element_text(hjust = 0.5, size = 13.5),
                         axis.text.x = element_text(size = 11),
                         axis.text.y = element_text(size = 11)) +
  labs(x = "Kategorisierte Magnitude", y = "Logarithmierte Nachbebenanzahl",
       title = "Logarithmierte Nachbebenanzahl ohne Beben mit keinen Nachbeben")
table(subset(full_data, select = c("magType", "triggerType")))
summary(full_data$magType)

# Magnitude-Scatterplot
ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() + xlim(c(4, 8)) +
  ylim(c(0, 80)) + myTheme +
  labs(x = "Magnitude", y = "Nachbebenanzahl")

# Magnitude-Stacked-Barplot
ggplot(data = full_data, aes(x = magType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 0, size = 12)) + myTheme +
  labs(title = "Anteile der Kategorien der Nachbebenanzahl", x = "Kategorie Magnitude",
       y = "Relativer Anteil",fill = "")


# StrainRate-Stacked-Barplot
full_data$strainType <- cut(
  full_data$strainRate, breaks = c(-0.1, 250, 500, 750, 1000, 1250, max(full_data$strainRate)),
  labels = c("0-250", "250-\n500", "500-\n750", "750-\n1000", "1000-\n1250", "1250+"))

ggplot(data = full_data, aes(x = strainType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 0, size = 12)) + myTheme +
  labs(title = "Anteile der Kategorien der Nachbebenanzahl", x = "Kategorie Strain-Rate",
       y = "Relativer Anteil",fill = "")
table(full_data$strainType)

# Rake_mod-Stacked-Barplot
ggplot(data = full_data, aes(x = rakeModType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(angle = 90, size = 11),
        axis.text.y = element_text(size = 11)) +
  labs(title = "Nachbeben-Kategorien innerhalb der modifizierten Rake-Kategorien",
       x = "Kategorie des modifizierten Rakes", y = "Relativer Anteil", fill = "")

# heatflow
ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) + geom_point(data = heat_data_res,
                                aes(x = long, y = lat, col = heatFlow),
                                size = 4, shape = 15, alpha=0.7) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2), limits = c(127, 146)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2), limits = c(27, 46)) +
  ggtitle(expression(paste("Heat-Flow im Koordinaten-Gitter in W/", m^{2}))) + labs(col = "Heat Flow") +
  scale_color_gradient(low = "yellow", high = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

# crustalThick
ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) + geom_point(data = heat_data_res,
                                aes(x = long, y = lat, col = crustalThick * 10),
                                size = 4, shape = 15, alpha=0.7) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2), limits = c(127, 146)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2), limits = c(27, 46)) +
  ggtitle("Erdkrustendicke im Koordinaten-Gitter in km") + labs(col = "Erdkrustendicke") +
  scale_color_gradient(low = "yellow", high = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

# mantleThick
ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) + geom_point(data = heat_data_res,
                                aes(x = long, y = lat, col = mantleThick * 10),
                                size = 4, shape = 15, alpha=0.7) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2), limits = c(127, 146)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2), limits = c(27, 46)) +
  ggtitle("Lithosphärischer Mantel im Koordinaten-Gitter in km") + labs(col = "Manteldicke") +
  scale_color_gradient(low = "yellow", high = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

# Optionaler Heat-Flow-Stacked-Barplot
full_data$heatflowType <- cut(
  full_data$heatFlow, breaks = c(0.049, 0.07, 0.09, 0.11, 0.13),
  labels = c("0.05 -\n0.07", "0.07 -\n0.09", "0.09 -\n0.11", "0.11 -\n0.13"))

ggplot(data = full_data, aes(x = heatflowType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 0, size = 11)) + myTheme +
  labs(title = "Anteile der Kategorien der Nachbebenanzahl", x = "Kategorie Heat-Flow",
       y = "Relativer Anteil",fill = "")

# Heatflow-Box
ggplot(full_data, aes(x = heatFlow, y = triggerType, col = triggerType)) +
  geom_boxplot() + theme(legend.position = "none") + myTheme +
  labs(y = "Kategorisierte Nachbebenanzahlen", x = "Heatflow")

# Optionaler depth-Stacked-Barplot
full_data$depthType <- cut(
  full_data$depth, breaks = c(-0.1, 25, 50, 75, 100),
  labels = c("0 - 25", "25 - 50", "50 -75", "75 - 100"))

ggplot(data = full_data, aes(x = depthType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 0, size = 11)) + myTheme +
  labs(title = "Anteile der Kategorien der Nachbebenanzahl", x = "Kategorie Tiefe in km",
       y = "Relativer Anteil",fill = "")











########################################################################################
# Magnitude und Stainrate ##############################################################
########################################################################################
# Scatterplot Magnitude
ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() +
  labs(title = "Magnitude~Nachbebenanzahl", x = "Magnitude", y = "Nachbebenanzahl") +
  myTheme

ggplot(full_data, aes(x = mag, y = triggerCountTh)) + geom_point() + xlim(c(4, 8)) +
  ylim(c(0, 80)) +
  labs(title = "Magnitude~Nachbebenanzahl", x = "Magnitude", y = "Nachbebenanzahl")

# Boxplots
ggplot(full_data, aes(x = magType, y = log(triggerCountTh), col = magType)) +
  geom_boxplot() + theme(legend.position = "none") +
  labs(x = "kategorisierte Magnitude", y = "Nachbebenanzahl")
ggplot(full_data[full_data$triggerCountTh > 0 & full_data$triggerCountTh < 100, ],
       aes(x = magType, y = log(triggerCountTh), col = magType)) + geom_boxplot() +
  theme(legend.position = "none") +
  labs(title = "Nur Beben mit 1-99 Nachbeben", x = "kategorisierte Magnitude",
       y = "Nachbebenanzahl")

# Aggregiere die Daten sinnvoll mit durchschnittlichen Nachbeben
ggplot(aggregate(triggerCountTh ~ mag, data = full_data, FUN = mean),
       aes(x = mag, y = triggerCountTh)) + geom_point() +
  xlab("Magnitude") + ylab("Durchschnittliche Nachbeben")
ggplot(aggregate(triggerCountTh ~ mag, data = full_data, FUN = mean),
       aes(x = mag, y = triggerCountTh)) + geom_line() +
  xlab("Magnitude") + ylab("Durchschnittliche Nachbeben")


# Strainrate ############################################################################
ggplot(full_data, aes(strainRate, colour = triggerType)) + geom_histogram(bins = 50) +
  labs(title = "Histogramm der Strainrate", x = "Strainrate", y = "Anzahl der Beben",
       colour = "")

# Stacked Barplot
full_data$strainType <- cut(full_data$strainRate, breaks = c(-0.1, 250, 500, 750, 1000,
                                       1250, max(full_data$strainRate)),
                            labels = c("0-250", "250-500", "500-750",
                                       "750-1000", "1000-1250", "1250+"))
full_data$count <- rep(1, nrow(full_data))
ggplot(data = full_data, aes(x = strainType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Kategorien der Nachbebenanzahl über die Strainrate", x = "Strainrate",
       y = "Relativer Anteil",
       colour = "")
table(full_data[full_data$strainType=="1000-1250",]$triggerCountTh)

ggplot(data = full_data, aes(x = strainType, y = count, fill = magType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Kategorien der Magnitude über die Strainrate", x = "Strainrate",
       y = "Relativer Anteil", colour = "Magnitude")



########################################################################################
# Winkel ###############################################################################
########################################################################################

# Barplot der kategorisierten dip-Variable stacked zu 100%
# (0:aufeinander, 90:nebeneinander)
ggplot(data = full_data, aes(x = dipType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" DipType") +
  ylab("Prozentanteil") + theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Nachbeben-Kategorien innerhalb der Dip-Kategorien") +
  labs(fill = "Nachbeben-Kategorie")

ggplot(data = full_data, aes(x = dipType, y = count, fill = magType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" DipType") +
  ylab("Prozentanteil") + theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Mag-Kategorien innerhalb der Dip-Kategorien") +
  labs(fill = "Mag-Kategorie")

# Barplot der kategorisierten rake_mod-Variable stacked zu 100%
# (-90:perfekte Abschiebung, 90:perfekte Aufschiebung)
ggplot(data = full_data, aes(x = rakeModType, y = count, fill = triggerType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" RakeModType") +
  ylab("Prozentanteil") +  theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Nachbeben-Kategorien innerhalb der modifizierten Rake-Kategorien") +
  labs(fill = "Nachbeben-Kategorie")

ggplot(data = full_data, aes(x = rakeModType, y = count, fill = magType)) +
  geom_bar(position = "fill", stat = "identity") + xlab(" RakeModType") +
  ylab("Prozentanteil") +  theme(axis.text.x = element_text(angle = 90, size = 12)) +
  labs(title = "Mag-Kategorien innerhalb der modifizierten Rake-Kategorien") +
  labs(fill = "Mag-Kategorie")



########################################################################################
# Geographische Groessen ###############################################################
########################################################################################

japan_background <- readPNG("Dokumente/japan.png")


# depth
ggplot(data = full_data)  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = depth), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(vjust = - 0.15),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Tiefe der Epizentren in km") + labs(col = "Tiefe")

ggplot(data = full_data[full_data$triggerType == "2-5 Nachbeben" |
                          full_data$triggerType == "6+ Nachbeben"])  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = depth), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(vjust = - 0.15),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Tiefe der Epizentren in km") + labs(col = "Tiefe")


# heatFlow
ggplot(data = full_data)  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = heatFlow), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(vjust = - 0.3),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Verteilung der Wärmestromdichte in W/(m^2)") + labs(col = "Wärmestromdichte")

ggplot(data = full_data[full_data$triggerType == "2-5 Nachbeben" |
                          full_data$triggerType == "6+ Nachbeben",])  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = heatFlow), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(vjust = - 0.3),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Verteilung der Wärmestromdichte in W/(m^2)") + labs(col = "Wärmestromdichte")

ggplot(full_data, aes(x = heatFlow, y = triggerType, col = triggerType)) +
  geom_boxplot() + theme(legend.position = "none") +
  labs(y = "Kategorisierte Nachbebenanzahlen", x = "Heatflow")
ggplot(full_data, aes(x = triggerType, y = depth, col = triggerType)) +
  geom_boxplot() + theme(legend.position = "none") +
  labs(x = "Kategorisierte Nachbebenanzahlen", y = "Heatflow")
ggplot(full_data, aes(x = triggerType, y = mantleThick, col = triggerType)) +
  geom_boxplot() + theme(legend.position = "none") +
  labs(x = "Kategorisierte Nachbebenanzahlen", y = "Heatflow")

# crustalThick
ggplot(data = full_data)  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = crustalThick), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Verteilung der Erdkrustendicke in 10km") + labs(col = "Erdkrustendicke")

ggplot(data = full_data[full_data$triggerType == "2-5 Nachbeben" |
                          full_data$triggerType == "6+ Nachbeben"])  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = crustalThick), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Verteilung der Erdkrustendicke in 10km") + labs(col = "Erdkrustendicke")

ggplot(full_data, aes(x = triggerType, y = crustalThick, col = triggerType)) +
  geom_boxplot() + theme(legend.position = "none") +
  labs(x = "Kategorisierte Nachbebenanzahlen", y = "Erdkrustendicke in 10km")



# mantleThick
ggplot(data = full_data)  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = mantleThick), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(- 0.3),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Verteilung der lithosphärischen Manteldicke in 10km") +
  labs(col = "Manteldicke")

ggplot(data = full_data[full_data$triggerType == "2-5 Nachbeben" |
                          full_data$triggerType == "6+ Nachbeben",])  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = mantleThick), alpha = 0.5,  size = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(- 0.3),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Verteilung der lithosphärischen Manteldicke in 10km") +
  labs(col = "Manteldicke")



# Beben mit Magnitude >= 6 und deren Nachbebenanzahl
ggplot(data = full_data[full_data$mag > 5.9, ])  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = triggerType), size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(- 0.3),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  ggtitle("Beben mit Magnitude >= 6 und deren Nachbebenanzahl") + labs(col = "")

table(full_data[full_data$mag > 5.9, ]$triggerType)




########################################################################################
# Cluster  #############################################################################
########################################################################################
table(full_data$clusterCount)

ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) +
  geom_point(data = full_data[full_data$clusterCount > 300, ], aes(x = lon, y = lat),
             col = "red", alpha = 0.55) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2), limits = c(127, 146)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2), limits = c(27, 46)) +
  ggtitle("Erdbebencluster des Tohoku-Bebens")





# Tests für Heatmaps #####################################################################

# Hintergrundbild Japan
japan_background <- readPNG("Dokumente/japan.png")


sum(heat_data_res$count)
ggplot(data = heat_data_res[heat_data_res$count > 5, ])  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(aes(x = lon, y = lat, col = heatFlow), size = 8, shape = 15, alpha=0.7) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(- 0.3),
        axis.line = element_line(colour = "black", size = 0.2, linetype = "solid")) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2)) +
  ggtitle("Heatflow für Koordinaten-Gitter mit mehr als 5 Beben") + labs(col = "heatflow") +
  scale_color_gradient(low = "yellow", high = "red")






japan <- map_data("japan")



ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) + geom_point(data = heat_data_res[heat_data_res$count > 5, ],
                                aes(x = long, y = lat, col = heatFlow),
                                size = 10, shape = 15, alpha=0.7) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2), limits = c(127, 146)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2), limits = c(27, 46)) +
  ggtitle("Heatflow für Koordinaten-Gitter mit mehr als 5 Beben") + labs(col = "heatflow") +
  scale_color_gradient(low = "yellow", high = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(vjust = -0.5)) +
  geom_point(data=full_data[full_data$triggerType=="1 Nachbeben",], aes(x=long, y =lat),
             col="blue",size=4)



full_data$long<- full_data$lon
ggplot() +
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) + geom_point(data = heat_data2[heat_data2$count > 5, ],
                                aes(x = long, y = lat, col = heatFlow),
                                size = 10, shape = 15, alpha=0.7) +
  scale_x_continuous(name = "Geographische Länge", expand = c(0, 0),
                     breaks = seq(128, 145, by = 2), limits = c(127, 146)) +
  scale_y_continuous(name = "Geographische Breite", expand = c(0, 0),
                     breaks = seq(28, 45, by = 2), limits = c(27, 46)) +
  ggtitle("Heatflow für Koordinaten-Gitter mit mehr als 5 Beben") + labs(col = "heatflow") +
  scale_color_gradient(low = "yellow", high = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(vjust = -0.5))













# Fehlgeschlagene Ideen
ggplot(full_data, aes(x=lon,y=lat))  +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf)+
  stat_density2d(geom = "polygon", aes(fill=..level..)) +
  geom_point(size=0.5)


ggplot(full_data, aes(x=lon,y=lat)) +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf)+
  stat_bin2d(alpha=0.5)



ggplot(full_data, aes(x=lon,y=lat)) +
  annotation_custom(rasterGrob(japan_background, width = unit(1,"npc"),
                               height = unit(1,"npc")), -Inf, Inf, -Inf, Inf)+
  stat_density2d(aes(fill = ..density..), geom = "tile", contour = FALSE, n = 25, alpha=0.5) +
  scale_fill_gradient(low = "white", high = "dark red")


# library
library(latticeExtra)

# create data
set.seed(1)
data <- data.frame(x = rnorm(100), y = rnorm(100))
data$z <- with(data, x * y + rnorm(100, sd = 1))

# showing data points on the same color scale
levelplot(heatFlow ~ lon * lat, full_data,
          panel = panel.levelplot.points, cex = 1.2
) +
  layer_(panel.2dsmoother(..., n = 200))

