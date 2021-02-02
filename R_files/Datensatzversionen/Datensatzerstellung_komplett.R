###################################################################################################
# R-File, dass alle Datenerstellungsprozesse zusammenfasst
###################################################################################################

# Waehle einen Threshhold
chosen_threshhold <- 0.00001

###################################################################################################
# Teil 1, der die urspruenglichen drei Datensaetze kombiniert
###################################################################################################
get.data.base <- function() {
  part1 <- read.table("Daten/Urspruengliche_Daten/JapanBosai_mw40_1997enriched.txt", header = TRUE, sep = ";")
  part2 <- read.csv("Daten/Urspruengliche_Daten/201126_strikeDipRake_selection.csv", header = TRUE, sep = ";")
  part3 <- read.csv("Daten/Urspruengliche_Daten/triggerRelations.csv", header = TRUE)
  part4 <- read.table("Daten/Urspruengliche_Daten/strainRates.txt", sep = ";", dec = ",")
  part1 <- part1[,-c(8, 9, 10, 11, 12, 13)]
  colnames(part3) <- c("distanceMeasure", "triggerID")
  colnames(part4) <- "strainRate"
  cbind(part1, part2, part4, part3)
}

input_data_base <- get.data.base()


###################################################################################################
# Teil 2, der Spalten generiert (keine Listenspalten/ kein data.table) abhaengig vom Threshhold
###################################################################################################
# Funktion ergänzt einen Inputdatensatz um die folgenden Spalten (abhängig vom Threshhold):
# - wasTriggered(boolean):    Wurde von einem anderen Beben getriggert
# - willTrigger(boolean):     Hat dieses Beben ein anderes Beben getriggered
# - singleEvent(boolean):     TRUE falls die beiden oberen Spalten jeweils FALSE sind
# - triggerCountTh(integer):  Anzahl direkter Nachbeben
new.columns1 <- function(input_data, threshhold) {
  resultData <- input_data
  resultData$wasTriggered <- rep(TRUE, nrow(resultData))
  resultData$willTrigger <- rep(FALSE, nrow(resultData))
  resultData$singleEvent <- rep(FALSE, nrow(resultData))
  resultData$triggerCountTh <- rep(0, nrow(resultData))

  # TriggerId-Spalte updaten je nachdem ob das Beben abhaengig vom Threshhold getriggered wurde
  for (i in seq_len(nrow(resultData))) {
    if (resultData$distanceMeasure[[i]] >= threshhold) {
      resultData$triggerID[[i]] <- -1
      resultData$wasTriggered[[i]] <- FALSE
    }
  }

  # Spalte hinzufuegen ob es ein Beben triggern wird, ob es ein Single Event (alleinstehendes Beben)
  # und wie viele Nachbeben von einem Beben getriggered wurden
  for (i in seq_len(nrow(resultData))) {
    if (i %in% resultData$triggerID) {
      resultData$willTrigger[[i]] <- TRUE
    }
    if (!(resultData$wasTriggered[[i]]) && !(resultData$willTrigger[[i]])) {
      resultData$singleEvent[[i]] <- TRUE
    }
    resultData$triggerCountTh[[i]] <- sum(i == resultData$triggerID)
  }

  resultData
}

input_data_columns1 <- new.columns1(input_data = input_data_base, threshhold = chosen_threshhold)


###################################################################################################
# Teil 3, der Spalten generiert (Listenspalten/ data.table)
###################################################################################################
# Lade das notwendige package 'data.table'
library(data.table)

# Funktion die einen Datensatz erweitert um folgende Spalten
# - directQuakes(list):     Liste direkter Nachbeben
# - clusterCount(integer):  Anzahl der Beben in einem Cluster
# - clusterQuakes(list):    Liste der Beben in einem Cluster
# - mainshock(integer):     Beben mit hoechster Magnitude in einem Cluster
new.columns2 <- function(input_data) {
  resultData <- as.data.table(input_data)
  directQuakes <- list()
  clusterQuakes <- list()

  # Fuege alle direkten Nachbeben in eine Liste
  for (i in seq_len(nrow(resultData))) {
    directQuakes[[i]] <- resultData$evID[resultData$triggerID == i]
  }

  # Fuege das Beben selbst in die Liste an den Platz seines CLusters
  for (i in seq_len(nrow(resultData))) {
    clusterQuakes[[i]] <- i
  }

  # Fuege das Cluster zusammen, part1
  for (i in seq(length(clusterQuakes), 1)) {
    if (resultData$wasTriggered[[i]]) {
      temp <- resultData$triggerID[[i]]
      clusterQuakes[[temp]] <- append(clusterQuakes[[temp]], clusterQuakes[[i]])
    }
  }

  # Fuege das Cluster zusammen, part2
  for (i in seq_len(length(clusterQuakes))) {
    for (j in directQuakes[[i]]) {
      clusterQuakes[[j]] <- clusterQuakes[[i]]
    }
  }

  # Sortiere die Cluster
  for (i in seq_len(length(clusterQuakes))) {
    clusterQuakes[[i]] <- sort(clusterQuakes[[i]])
  }

  # Fuege die direkten Nachbeben (Liste) als Spalte hinzu
  resultData <- cbind(resultData, directQuakes)

  # Fuege die Clustergroesse als Spalte hinzu
  resultData$clusterCount <- rep(0, nrow(resultData))
  for (i in seq_len(nrow(resultData))) {
    resultData$clusterCount[[i]] <- length(clusterQuakes[[i]])
  }

  # Fuege die Beben in einem Cluster (Liste) als Spalte hinzu
  resultData <- cbind(resultData, clusterQuakes)

  # Fuege die Spalte des Hauptbebens als Spalte hinzu
  resultData$mainshock <- rep(0, nrow(resultData))

  for (i in seq_len(nrow(resultData))) {
    resultData$mainshock[[i]] <- i
  }

  for (i in seq(nrow(resultData), 1)) {
    if (resultData$wasTriggered[[i]]) {
      if (resultData$mag[[resultData$mainshock[[i]]]] > resultData$mag[[resultData$mainshock[[resultData$triggerID[[i]]]]]]) {
        resultData$mainshock[[resultData$triggerID[[i]]]] <- resultData$mainshock[[i]]
      }
    }
  }

  for (i in seq_len(nrow(resultData))) {
    if (resultData$wasTriggered[[i]]) {
      if (resultData$mag[[resultData$mainshock[[i]]]] < resultData$mag[[resultData$mainshock[[resultData$triggerID[[i]]]]]]) {
        resultData$mainshock[[i]] <- resultData$mainshock[[resultData$triggerID[[i]]]]
      }
    }
  }

  # Kategoriale Variable fuer den triggerCountTh
  triggerType <- character()
  for (i in seq_len(nrow(resultData))) {
    if (resultData$triggerCountTh[[i]] == 0) {
      triggerType[[i]] <- "0 Nachbeben"
    }
    else if (resultData$triggerCountTh[[i]] == 1) {
      triggerType[[i]] <- "1 Nachbeben"
    }
    else if (resultData$triggerCountTh[[i]] >= 2 & resultData$triggerCountTh[[i]] <= 5) {
      triggerType[[i]] <- "2-5 Nachbeben"
    }
    else {
      triggerType[[i]] <- "6+ Nachbeben"
    }
  }
  resultData$triggerType <- factor(triggerType, levels = c("0 Nachbeben", "1 Nachbeben", "2-5 Nachbeben", "6+ Nachbeben"))

  resultData
}

full_data <- new.columns2(input_data = input_data_columns1)


###################################################################################################
# Teil 4, Transformation einiger numerischer Variablen
###################################################################################################

# Kovariablen die den Zero-inflated NegBin Algorithmen numerische Probleme bereiten
summary(full_data[, c("crustalThick", "mantleThick", "elevation")])
# Diese wurden urspruenglich alles in m angegeben, wir transformieren crustal- und mantleThick auf
# 10km und elevation auf km

full_data$crustalThick <- full_data$crustalThick / 10000
full_data$mantleThick <- full_data$mantleThick / 10000
full_data$elevation <- full_data$elevation / 1000


###################################################################################################
# Teil 5, Ueber- oder unterdurchschnittliches Triggering als kategoriale Variable
###################################################################################################
# Auf Werten basierend aus dem Matlab Curvefitting-Tool und nur auf Threshold 0.00001 sinnvoll
trigTrend <- character()
for (i in seq_len(nrow(full_data))) {
  if (full_data$triggerCountTh[[i]] > 8.971e-08 * exp(2.581 * full_data$mag[[i]])) {
    trigTrend[[i]] <- "above_avg"
  }
  else {
    trigTrend[[i]] <- "below_avg"
  }
}
full_data$trigTrend <- factor(trigTrend, levels = c("below_avg", "above_avg"))


###################################################################################################
# Teil 6, zum Daten abspeichern
###################################################################################################

# Als Rda
save(full_data, file = "Daten/data_full_00001.Rda")

# Listenspalten entfernen
full_data[, c("directQuakes","clusterQuakes"):=NULL]

# Als Rda
save(full_data, file = "Daten/data_full_no_lists_00001.Rda")

