# Lade  das hier angewandte Paket "data.table"
library(data.table)

# Lese den einen Datensatz ein (konzipiert fuer Datensaetze aus R-File: "Daten_abhaengig_Threshhold)
full_data_03 <- read.csv("Daten/data_full_03.csv", header = TRUE)


# Funktion die einen Datensatz erweitert um folgende Spalten
# - directQuakes(list):     Liste direkter Nachbeben
# - clusterCount(integer):  Anzahl der Beben in einem Cluster
# - clusterQuakes(list):    Liste der Beben in einem Cluster
# - mainshock(integer):     Beben mit hoechster Magnitude in einem Cluster
new.Columns.Th2 <- function(input_data) {
  resultData <- as.data.table(input_data)
  directQuakes <- list()
  clusterQuakes <- list()

  # Fuege alle direkten Nachbeben in eine Liste
  for (i in seq_len(nrow(resultData))) {
    directQuakes[[i]] <- resultData$evID[resultData$triggerID == i]
  }

  for (i in seq_len(nrow(resultData))) {
    clusterQuakes[[i]] <- i
  }

  for (i in seq(length(clusterQuakes), 1)) {
    if (resultData$wasTriggered[[i]]) {
      temp <- resultData$triggerID[[i]]
      clusterQuakes[[temp]] <- append(clusterQuakes[[temp]], clusterQuakes[[i]])
    }
  }

  for (i in seq_len(length(clusterQuakes))) {
    clusterQuakes[[i]] <- sort(clusterQuakes[[i]])
  }

  for (i in seq_len(length(clusterQuakes))) {
    for (j in directQuakes[[i]]) {
      clusterQuakes[[j]] <- clusterQuakes[[i]]
    }
  }

  resultData <- cbind(resultData, directQuakes)

  resultData$clusterCount <- rep(0, nrow(resultData))

  for (i in seq_len(nrow(resultData))) {
    resultData$clusterCount[[i]] <- length(clusterQuakes[[i]])
  }

  resultData <- cbind(resultData, clusterQuakes)

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

  resultData
}


full_data_04 <- new.Columns.Th2(full_data_03)
View(full_data_04)


