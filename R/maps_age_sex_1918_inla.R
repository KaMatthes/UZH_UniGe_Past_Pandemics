function_maps_sex_age_1918 <- function(){
  
  load(paste0("../data/expected_death_inla_20_39_f_1918.RData"))
  death_1918_20_39_f <- expected_deaths

  load(paste0("../data/expected_death_inla_20_39_m_1918.RData"))
  death_1918_20_39_m <- expected_deaths
  
  # load(paste0("data/expected_death_inla_20_39_f_1918.RData"))
  # death_1918_20_39_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla_20_39_m_1918.RData"))
  # death_1918_20_39_m <- expected_deaths


  data_excess <- rbind(death_1918_20_39_f,death_1918_20_39_m)%>%
    select(-column_label) %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk),
         age_group =  age_group.x,
         significant_dummy = ifelse(death > LL & death < UL,0,1),
         significant_dummy = as.factor( significant_dummy )) %>%
   filter(Year==1918) 
  
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
  mutate(excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage_o = ifelse(excess_percentage_o <0,0, excess_percentage_o))
  #        excess_percentage = round(((death-fit)/fit)*100,2)) %>%
  # filter(!is.na(excess_percentage))%>%
  #   mutate(
  #     excess_perc_groups =  cut(excess_percentage, 
  #                               breaks = c(0,100,150, 200,  250,300, 350, 400, 900), 
  #                               labels = c("[14 %,100%)", "[100%, 150%)","[150%, 200%)",  "[200%, 250%)",
  #                                          "[250%, 300%)","[300%, 350%)","[300%, 400%)",">400"),
  #                               include.lowest = TRUE, right = FALSE)) %>%
  #   filter(!is.na(excess_percentage))
  


plot_excess <- tm_shape( bezirk_geo  ) + 
  tm_fill("excess_percentage_o",
          # palette = "Reds", 
          # legend.hist = TRUE,
          style = "jenks",
          # style = "kmeans",
          title = "Relative excess deaths",
          legend.is.portrait = FALSE) +
  tm_borders(alpha = 0.5)+
  tm_facets(by="sex", ncol=2)+
  tm_layout(
    #   main.title = "Relative excess deaths")
    # main.title.position = "left",
    # legend.text.size = legend_size_map,
    # legend.width = 5,
    # legend.height = 8,
    # legend.position = c(0.7,0.8),
    legend.outside.position = "bottom")

  # plot_excess <- ggplot(data=bezirk_geo)+
  #   # geom_sf_pattern(aes(pattern=significant_dummy, fill= excess_perc_groups),pattern_fill = "grey50", pattern_color="grey50",
  #   #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  #   geom_sf(mapping = aes(fill =excess_perc_groups)) +
  #   facet_wrap(sex~age_group, ncol = 4) +
  #   scale_fill_manual("",values = col8viridis)+
  #   scale_pattern_manual("significant",
  #                        breaks =c("0", "1"),
  #                        labels=c("no", "yes"),
  #                        values = c("none","circle"))+
  #   ggtitle("Relative excess deaths - 1918")+
  #   theme(
  #     panel.grid.major=element_blank(),
  #     axis.title=element_blank(),
  #     axis.line=element_blank(),
  #     axis.text=element_blank(),
  #     axis.ticks=element_blank(),
  #     panel.border = element_blank(),
  #     legend.position = "bottom")
  # 
  
#   
# cowplot::save_plot("output/excess_sex_age1918.pdf",plot_excess,base_height=12,base_width=10)
  
  return(plot_excess)

}

