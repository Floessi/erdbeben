####################################################################################################
# cluster-data-Erstellung ##########################################################################
####################################################################################################

# Package
library(data.table)

# Wegen Listenstruktur zum filtern von den Single-events
unlist(lapply(full_data$clusterQuakes, length))
full_data2 <- full_data
full_data2 <- full_data[unlist(lapply(full_data$clusterQuakes, length)) > 1, ]

# Zum aggregieren darf es keine Listenspalte sein
full_data3 <- full_data2
full_data3$clusterQuakes <- as.character(full_data3$clusterQuakes)




# Einzelne Variablen ###############################################################################

# Mean Magnitude im Cluster
temp <- aggregate(mag ~ clusterQuakes, data = full_data3, mean)

# ClusterCount im Cluster
temp2 <- aggregate(clusterCount ~ clusterQuakes, data = full_data3, mean)

# Anzahl der TriggerBeben im Cluster
temp3 <- aggregate(willTrigger ~ clusterQuakes, data = full_data3, sum)

# Anteil der TriggerBeben an gesamter Anzahl an Beben im Cluster
temp2$rel_trigger <- temp3$willTrigger / temp2$clusterCount



# Fuege die einzelnen Spalten zusammen #############################################################
cluster_data <- merge(merge(temp, temp2), temp3)
