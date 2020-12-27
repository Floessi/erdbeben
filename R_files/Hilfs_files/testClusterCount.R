# Nötiges Datenpackage
library(data.table)

# Beispiel Daten aus Augabenblatt 5 (ProgR)
widget.corp.data.list <- data.table(
  machine = c("Machine01", "Machine02", "Machine03", "Machine03", "Machine03", "Machine02", "Machine01",
              "Machine01", "Machine02", "Machine01", "Machine03"),
  quality = c(78, 28, 32, 80, 58, 74, 46, 24, 7, 22, 98),
  sensor = list(
    c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23),
    c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27),
    c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10),
    c(sensor02 = 32.3),
    c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3),
    c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3),
    c(sensor01 = 81),
    c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22),
    c(sensor01 = 96, sensor02 = 96, sensor03 = 0),
    c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8),
    c(sensor03 = 11)
  )
)

# Siehe Spaltenarten
class(widget.corp.data.list$sensor)
class(widget.corp.data.list$quality)

# Mache selbst eine Listenspalte
y <- list(c(2,3),c(2,3),c(2,3),c(2,3),c(2,3),c(2,3),c(2,3),c(2,3),c(2,3),c(2,3),c(2,3))
y

# Füge es zusammen
x <- cbind(widget.corp.data.list, y)
x

# Welche Zahlen sind bereits in Cluster/ der Listenspalte
unlist(x$y)

test <- full_data_02[1:200,]

x <- test$evID[test$triggerID==16]
