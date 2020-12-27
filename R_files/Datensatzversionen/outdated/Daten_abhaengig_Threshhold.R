# Dieses R-File soll dazu dienen den Datensatz "full_data_02" um mehrere Spalten abhaenging vom
# gewaehlten Threshholds für das Distanzmass zu ergaenzen

# Wähle den Threshhold für das Distanzmaß
threshhold1 <- 0.00001

# Lese den Datensatz "full_data_02" ein
full_data_02 <- read.csv("Daten/data_full_02.csv", header = TRUE)

# Funktion ergänzt einen Inputdatensatz um die folgenden Spalten (abhängig vom Threshhold):
# - wasTriggered(boolean):    Wurde von einem anderen Beben getriggert
# - willTrigger(boolean):     Hat dieses Beben ein anderes Beben getriggered
# - singleEvent(boolean):     TRUE falls die beiden oberen Spalten jeweils FALSE sind
# - triggerCountTh(integer):  Anzahl direkter Nachbeben
# - clusterCount(integer):    Anzahl der Beben im Cluster
# - hauptbeben(interger):     ID des Bebens mit groesster Magnituede im Cluster
new.Columns.Th <- function(input_data, threshhold) {
  resultData <- input_data
  resultData$wasTriggered <- rep(TRUE, nrow(resultData))
  resultData$willTrigger <- rep(FALSE, nrow(resultData))
  resultData$singleEvent <- rep(FALSE, nrow(resultData))
  resultData$triggerCountTh <- rep(0, nrow(resultData))

  # TriggerId-Spalte updaten je nachdem ob es abhaengig vom Threshhold getriggered wurde
  for (i in seq_len(nrow(resultData))) {
    if (resultData$distanceMeasure[[i]] >= threshhold) {
      resultData$triggerID[[i]] <- -1
      resultData$wasTriggered[[i]] <- FALSE
    }
  }

  # Spalte hinzufügen ob es ein Beben triggern wird, ob es ein Single Event (alleinstehendes Beben)
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

full_data_03 <- new.Columns.Th(input_data = full_data_02, threshhold = threshhold1)
full_data_03

# Speichere den
write.csv(full_data_03,"Daten/data_full_03.csv", row.names = FALSE)

