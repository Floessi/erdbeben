# Zusammenhangssuche (Koordinaten)

library(ggplot2)
library(cowplot)


# Lade die vollstaendigen Daten
load("Daten/data_full_00001.Rda")


ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = heatFlow)) +
  scale_color_gradient(low = "yellow", high = "red")
ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = crustalThick)) +
  scale_color_gradient(low = "yellow", high = "red")
ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = mantleThick)) +
  scale_color_gradient(low = "yellow", high = "red")
ggplot(full_data, aes(x = lon, y = lat)) + geom_point(aes(colour = elevation)) +
  scale_color_gradient(low = "yellow", high = "red")



# Scatterplot alle Events
heat_full_plot <- ggplot(full_data, aes(x = lon, y = lat)) +
  geom_point(aes(colour = heatFlow), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
crustal_full_plot <- ggplot(full_data, aes(x = lon, y = lat)) +
  geom_point(aes(colour = crustalThick), show.legend = FALSE)+
  scale_color_gradient(low = "yellow", high = "red")
mantle_full_plot <- ggplot(full_data, aes(x = lon, y = lat)) +
  geom_point(aes(colour = mantleThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
elevation_full_plot <- ggplot(full_data, aes(x = lon, y = lat)) +
  geom_point(aes(colour = elevation), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")



# Filter SingleEvents raus
full_data_no_single <- full_data[!full_data$singleEvent,]
# Scatterplot ohne alle SingleEvents
heat_ns_plot <- ggplot(full_data_no_single, aes(x = lon, y = lat)) +
  geom_point(aes(colour = heatFlow), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
crustal_ns_plot <- ggplot(full_data_no_single, aes(x = lon, y = lat)) +
  geom_point(aes(colour = crustalThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
mantle_ns_plot <- ggplot(full_data_no_single, aes(x = lon, y = lat)) +
  geom_point(aes(colour = mantleThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
elevation_ns_plot <- ggplot(full_data_no_single, aes(x = lon, y = lat)) +
  geom_point(aes(colour = elevation), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")



# Filter alle Beben mit mehr als einem Nachbeben
full_data_go <- full_data[full_data$triggerCountTh > 1,]
# Scatterplot Beben mit mehr als einem Nachbeben
heat_go_plot <- ggplot(full_data_go, aes(x = lon, y = lat)) +
  geom_point(aes(colour = heatFlow)) +
  scale_color_gradient(low = "yellow", high = "red")
crustal_go_plot <- ggplot(full_data_go, aes(x = lon, y = lat)) +
  geom_point(aes(colour = crustalThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
mantle_go_plot <- ggplot(full_data_go, aes(x = lon, y = lat)) +
  geom_point(aes(colour = mantleThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
elevation_go_plot <- ggplot(full_data_go, aes(x = lon, y = lat)) +
  geom_point(aes(colour = elevation), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")



# Filter alle Beben mit mehr als fuenf Nachbeben
full_data_gf <- full_data[full_data$triggerCountTh > 5,]
# Scatterplot Beben mit mehr als fuenf Nachbeben
heat_gf_plot <- ggplot(full_data_gf, aes(x = lon, y = lat)) +
  geom_point(aes(colour = heatFlow), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
crustal_gf_plot <- ggplot(full_data_gf, aes(x = lon, y = lat)) +
  geom_point(aes(colour = crustalThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
mantle_gf_plot <- ggplot(full_data_gf, aes(x = lon, y = lat)) +
  geom_point(aes(colour = mantleThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
elevation_gf_plot <- ggplot(full_data_gf, aes(x = lon, y = lat)) +
  geom_point(aes(colour = elevation), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")


plot_grid(heat_full_plot, heat_ns_plot, heat_go_plot, heat_gf_plot,
          crustal_full_plot, crustal_ns_plot, crustal_go_plot, crustal_gf_plot,
          mantle_full_plot, mantle_ns_plot, mantle_go_plot, mantle_gf_plot,
          elevation_full_plot, elevation_ns_plot, elevation_go_plot, elevation_gf_plot, nrow = 4)



table(full_data$clusterCount)
# Filter die 5 groessten Cluster
full_data_gc <- full_data[full_data$clusterCount > 77,]
# Scatterplot mit Beben der fuenf groessten Cluster
heat_gc_plot <- ggplot(full_data_gc, aes(x = lon, y = lat)) +
  geom_point(aes(colour = heatFlow), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
crustal_gc_plot <- ggplot(full_data_gc, aes(x = lon, y = lat)) +
  geom_point(aes(colour = crustalThick), show.legend = FALSE)+
  scale_color_gradient(low = "yellow", high = "red")
mantle_gc_plot <- ggplot(full_data_gc, aes(x = lon, y = lat)) +
  geom_point(aes(colour = mantleThick), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")
elevation_gc_plot <- ggplot(full_data_gc, aes(x = lon, y = lat)) +
  geom_point(aes(colour = elevation), show.legend = FALSE) +
  scale_color_gradient(low = "yellow", high = "red")


plot_grid(heat_full_plot, heat_gc_plot,
          crustal_full_plot, crustal_gc_plot,
          mantle_full_plot, mantle_gc_plot,
          elevation_full_plot, elevation_gc_plot, nrow = 4)


# Filter starke Beben (Mag > 6)
full_data_sq <- full_data[full_data$mag > 6,]
# Scatterplot
ggplot(full_data_sq, aes(x = lon, y = lat)) + geom_point(aes(colour = mag)) +
  scale_color_gradient(low = "yellow", high = "red")



