source("R/maps_Figure1.R")
source("R/hotspots_Figure1.R")

plot1 <- tmap_grob(function_maps_inla(1890))
plot2 <- tmap_grob(function_maps_inla(1918))
plot3 <- tmap_grob(function_maps_inla(2020))

# plot_legend <- tmap_grob(plot_legend)
# 
# plot_maps <- plot_grid(plot1, plot2,
#                        plot3,
#                        ncol=1)
# 
# cowplot::save_plot("output/Figure_maps1.pdf", plot1,base_height=15,base_width=15)
# cowplot::save_plot("output/Figure_maps2.pdf", plot2,base_height=15,base_width=15)
# cowplot::save_plot("output/Figure_maps3.pdf", plot3,base_height=15,base_width=15)
# cowplot::save_plot("output/Figure_legend.pdf", plot_legend,base_height=5,base_width=10)


plot4 <- tmap_grob(function_hotspot(1890))
plot5 <- tmap_grob(function_hotspot(1918))
plot6 <- tmap_grob(function_hotspot(2020))

# plot4 <- tmap_grob(function_hotspot(1890))
# cowplot::save_plot("output/Figure_hot1.pdf", plot4,base_height=20,base_width=20)
# 
# cowplot::save_plot("output/Figure_hot2.pdf", plot5,base_height=15,base_width=15)
# cowplot::save_plot("output/Figure_hot3.pdf", plot6,base_height=15,base_width=15)
# cowplot::save_plot("output/Figure_legend.pdf", plot_legend,base_height=15,base_width=20)


plot_maps <- plot_grid(plot1, plot4,
                       NULL, NULL,
                       plot2, plot5,
                       NULL, NULL,
                       plot3, plot6,
                       ncol=2, align = "hv",
                       rel_heights = c(1,-0.25,1,-0.25,1,1,-0.25,1, -0.25,1))


cowplot::save_plot("output/Figure1.pdf", plot_maps ,base_height=25,base_width=30)


source("R/maps_Figure2.R")

plot7 <- tmap_grob(function_maps_sex(1890))
plot8 <- tmap_grob(function_maps_sex(1918))
plot9 <- tmap_grob(function_maps_sex(2020))

# plot10 <- tmap_grob(function_maps_sex(1890))
# plot11 <- tmap_grob(function_maps_sex(1918))
# plot12 <- tmap_grob(function_maps_sex(2020))


plot_maps_sex <- plot_grid(plot7,
                       NULL,
                      plot8,
                       NULL,
                       plot9,
                       ncol=1, align = "hv",
                       rel_heights = c(1,-0.6,1,-0.6,1))


cowplot::save_plot("output/Figure2.pdf", plot_maps_sex ,base_height=25,base_width=22)



source("R/maps_Figure3.R")

plot13 <- tmap_grob(function_maps_age(1890))
plot14 <- tmap_grob(function_maps_age(1918))
plot15 <- tmap_grob(function_maps_age(2020))

# plot16 <- tmap_grob(function_maps_age(1890))
# plot17 <- tmap_grob(function_maps_age(1918))
# plot18 <- tmap_grob(function_maps_age(2020))


plot_maps_age <- plot_grid(plot13,
                           NULL,
                           plot14,
                           NULL,
                           plot15,
                           ncol=1, align = "hv",
                           rel_heights = c(1,-0.6,1,-0.6,1))

cowplot::save_plot("output/Figure3.pdf", plot_maps_age ,base_height=25,base_width=22)

# cowplot::save_plot("output/Figure_1890_0_69.pdf", plot13,base_height=15,base_width=15)


source("R/maps_Figure4.R")


plot19 <- tmap_grob(function_maps_age_1918())

cowplot::save_plot("output/Figure4.pdf",  plot19 ,base_height=25,base_width=35)


source("R/maps_Figure5.R")
plot20 <- tmap_grob(function_maps_sex_age_1918())
cowplot::save_plot("output/Figure5.pdf",  plot20 ,base_height=25,base_width=40)

# plot19 <- tmap_grob(function_maps_age_1918("0-19 years"))
# plot20 <- tmap_grob(function_maps_age_1918("20-29 years"))
# plot20_2 <- tmap_grob(function_maps_age_1918("30-39 years"))
# plot21 <- tmap_grob(function_maps_age_1918("40-69 years"))
# plot22 <- tmap_grob(function_maps_age_1918(">70 years"))
# 
# 
# plot_maps_age_1918 <- plot_grid(plot19, NULL,
#                                 plot20, plot20_2,
#                            plot21, plot22,
#                      
#                            ncol=2)
# 
# cowplot::save_plot("output/Figure_maps_age_1918.pdf", plot_maps_age_1918 ,base_height=25,base_width=15)



plot23 <- tmap_grob(function_maps_sex_age_1918("female"))
plot24 <- tmap_grob(function_maps_sex_age_1918("male"))


plot_maps_age_sex_1918 <- plot_grid(plot23, plot24,
                                NULL, NULL,
                                NULL, NULL,
                                ncol=2)

cowplot::save_plot("output/Figure_maps_age_sex_1918.pdf", plot_maps_age_sex_1918 ,base_height=25,base_width=15)



# Maps crude mortality


plot25 <- tmap_grob(function_maps_crude(1890))
plot26<- tmap_grob(function_maps_crude(1918))
plot27 <- tmap_grob(function_maps_crude(2020))
plot28 <- tmap_grob(function_maps_crude_legend(2020))

plot_maps_crude <- plot_grid(plot25,plot26,
                       plot27,plot28,
                       ncol=2, align = "hv")

cowplot::save_plot("output/Figure2_supplement.pdf", plot_maps_crude ,base_height=20,base_width=30)

