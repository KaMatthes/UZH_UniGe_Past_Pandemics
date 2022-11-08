plot1 <- tmap_grob(function_maps_inla(1890))
plot2 <- tmap_grob(function_maps_inla(1918))
plot3 <- tmap_grob(function_maps_inla(2020))

plot4 <- tmap_grob(function_hotspot(1890))
plot5 <- tmap_grob(function_hotspot(1918))
plot6 <- tmap_grob(function_hotspot(2020))


plot_maps <- plot_grid(plot1, plot4,
                       plot2, plot5,
                       plot3, plot6,
                       ncol=2)


cowplot::save_plot("output/Figure_maps.pdf", plot_maps ,base_height=25,base_width=15)