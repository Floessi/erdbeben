# Lese die Hauptdaten in der anfänglichen Form ein
data_main <- read.table("Daten/Urspruengliche_Daten/JapanBosai_mw40_1997enriched.txt", header = TRUE, sep = ";")
data_main_2 <- data_main[,-c(8, 9, 10, 11, 12, 13)]

# Lese die relevanten Winkeltripel ein
data_angles <- read.csv("Daten/Urspruengliche_Daten/201126_strikeDipRake_selection.csv", header = TRUE, sep = ";")

# Lese die Daten zu den Trigger-Relationen in der anfänglichen Form ein
data_trigger_relation <- read.csv("Daten/Urspruengliche_Daten/triggerRelations.csv", header = TRUE)
colnames(data_trigger_relation) <- c("distanceMeasure", "triggerID")

# Füge die Daten zusammen
full_data_02 <- cbind(data_main_2,data_angles, data_trigger_relation)

# Exportiere die Daten 'data_full_01'
write.csv(full_data_02,"Daten/data_full_02.csv", row.names = FALSE)


