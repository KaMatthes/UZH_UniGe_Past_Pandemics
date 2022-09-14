function_maps_inla <- function(){

    load(paste0("../data/expected_death_inla1890.RData"))
    Expected_death_Russian <-expected_deaths
    load(paste0("../data/expected_death_inla1918.RData"))
    Expected_death_Spanish <- expected_deaths
    load(paste0("../data/expected_death_inla2020.RData"))
    Expected_death_Covid <- expected_deaths

    # load(paste0("data/expected_death_inla1890.RData"))
    # Expected_death_Russian <-expected_deaths
    # load(paste0("data/expected_death_inla1918.RData"))
    # Expected_death_Spanish <- expected_deaths
    # load(paste0("data/expected_death_inla2020.RData"))
    # Expected_death_Covid <- expected_deaths

    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid)
    

# save(data_excess ,file=paste0("data/data_excess.RData"))
# write.xlsx(data_excess,file=paste0("data/data_excess.xlsx"),row.names=FALSE, overwrite = TRUE)

data_excess <- data_excess %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890") %>%
  mutate(significant_dummy = ifelse(death > LL & death < UL,0,1),
         significant_dummy = as.factor( significant_dummy ))

# sf::sf_use_s2(TRUE)

bezirk_geo <- read_sf("../data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
           | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
           | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
           | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  dplyr::rename(Bezirk = BEZIRKSNUM) %>%
  dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
  dplyr::select(Bezirk, geometry) %>%
  full_join(data_excess) %>%
  mutate(
          excess_percentage_o = ((death-fit)/fit)*100,
          excess_percentage = round(((death-fit)/fit)*100,2),
          excess_percentage_o = ifelse(excess_percentage_o <0,0, excess_percentage_o))
          # excess_percentage = ifelse(excess_percentage<0, 0, excess_percentage),
          # excess_percentage = ifelse(excess_percentage>100, 100, excess_percentage),
          # excess_perc_groups =  cut(excess_percentage, 
          #                           breaks = c(-45, 0, 5, 10, 20, 40, 60,80,100,125), 
          #                           labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
          #                                      "[10 %, 20%)", "[20%, 40%)",  "[40%, 60%)",
          #                                      "[60%, 80%)","[80%, 100%)", ">100"),
          #                           include.lowest = TRUE, right = FALSE))



plot_excess <- tm_shape( bezirk_geo  ) + 
  tm_fill("excess_percentage_o",
           palette = "YlOrBr", 
          # legend.hist = TRUE,
          style = "jenks",
          # style = "kmeans",
          title = "Relative excess deaths",
          legend.is.portrait = FALSE) +
  tm_borders(alpha = 0.5)+
  tm_facets(by="Year")+
  tm_layout(
  #   main.title = "Relative excess deaths")
  # main.title.position = "left",
  # legend.text.size = legend_size_map,
  # legend.width = 5,
  # legend.height = 8,
  # legend.position = c(0.7,0.8),
  legend.outside.position = "bottom")
  # legend.outside.size = .1)
  # legend.title.size=legend_size_map,
  # main.title.size = main_size_map)

      
# plot_excess <- ggplot()+
#   # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
#   #                 pattern_spacing = 0.03,pattern_size=0.5 )+
#   geom_sf(data= bezirk_geo, aes(fill=  excess_perc_groups ),alpha=1,col="black", size=0.1) +
#   facet_wrap(~Year, ncol = 2) +
#   scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
#   scale_pattern_manual("significant",
#                        breaks =c("0", "1"),
#                        labels=c("no", "yes"),
#                        values = c("none","circle"))+
#   ggtitle("Relative excess deaths")+
#   theme(
#     panel.grid.major=element_blank(),
#     axis.title=element_blank(),
#     axis.line=element_blank(),
#     axis.text=element_blank(),
#     axis.ticks=element_blank(),
#     panel.border = element_blank(),
#     legend.position = "bottom")
# cowplot::save_plot("output/excess_rate_group_inla_test2.pdf",plot_excess,base_height=12,base_width=10)

return(plot_excess)

}

