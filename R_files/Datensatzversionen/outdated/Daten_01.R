# Lese die Hauptdaten in der anfänglichen Form ein
data_main <- read.table("Daten/Urspruengliche_Daten/JapanBosai_mw40_1997enriched.txt", header = TRUE, sep = ";")

# Lese die Daten zu den Trigger-Relationen in der anfänglichen Form ein
data_trigger_relation <- read.csv("Daten/Urspruengliche_Daten/triggerRelations.csv", header = TRUE)
colnames(data_trigger_relation) <- c("distanceMeasure", "triggerID")

# Füge die Daten zusammen
full_data_00 <- cbind(data_main, data_trigger_relation)


# Füge Spalte ein für die Anzahl an direkten Nachbeben eines Bebens
# (ohne Abhängigkeit des Distanzmaßes)
# Häufigkeitstabelle
amount_triggered <- table(full_data_00$triggerID)
amount_triggered_df <- as.data.frame(amount_triggered)
# Anzahl der Nachbeben an Stelle der zugehörigen ID in einen Vektor
triggerCount <- integer()
for (i in seq_len(nrow(full_data_00))) {
  if (i %in% amount_triggered_df$Var1) {
    triggerCount[[i]] <- amount_triggered_df$Freq[i == amount_triggered_df$Var1]
  }
  else {
    triggerCount[[i]] <- 0
  }
}

# Füge die neue Spalte hinzu
full_data_01 <- cbind(full_data_00, triggerCount)


# Exportiere die Daten 'data_full_01'
write.csv(full_data_01,"Daten/data_full_01.csv", row.names = FALSE)


