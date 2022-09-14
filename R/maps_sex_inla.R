function_maps_sex<- function(Year_Pan){


  load(paste0("../data/expected_death_inlaf_1890.RData"))
  death_1890_f <- expected_deaths

  load(paste0("../data/expected_death_inlaf_1918.RData"))
  death_1918_f <- expected_deaths

  load(paste0("../data/expected_death_inlaf_2020.RData"))
  death_2020_f <- expected_deaths

  load(paste0("../data/expected_death_inlam_1890.RData"))
  death_1890_m <- expected_deaths

  load(paste0("../data/expected_death_inlam_1918.RData"))
  death_1918_m <- expected_deaths

  load(paste0("../data/expected_death_inlam_2020.RData"))
  death_2020_m <- expected_deaths


# load(paste0("data/expected_death_inlaf_1890.RData"))
# death_1890_f <- expected_deaths
# 
# load(paste0("data/expected_death_inlaf_1918.RData"))
# death_1918_f <- expected_deaths
# 
# load(paste0("data/expected_death_inlaf_2020.RData"))
# death_2020_f <- expected_deaths
# 
# load(paste0("data/expected_death_inlam_1890.RData"))
# death_1890_m <- expected_deaths
# 
# load(paste0("data/expected_death_inlam_1918.RData"))
# death_1918_m <- expected_deaths
# 
# load(paste0("data/expected_death_inlam_2020.RData"))
# death_2020_m <- expected_deaths

data_excess <- rbind(death_1890_m,death_1918_m, death_2020_m,
                       death_1890_f,death_1918_f, death_2020_f )%>%
    select(-column_label) %>%
    rename(sex=sex.x) %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year==Year_Pan) %>%
  mutate(significant_dummy = ifelse(death > LL & death <UL,0,1),
         significant_dummy = as.factor( significant_dummy ))


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
         excess_percentage_o = ifelse(excess_percentage_o <0,0, excess_percentage_o),
         excess_percentage = round(((death-fit)/fit)*100,2),
         excess_perc_groups =  as.numeric(excess_percentage)) 



if(Year_Pan==1890) {
  # bezirk_geo <- bezirk_geo %>%
  #   mutate(
  #   excess_perc_groups =  cut(excess_percentage, 
  #                             breaks = c(-120, 0, 5, 10, 15, 20, 25, 30,35,70), 
  #                             labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
  #                                        "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
  #                                        "[25%, 30%)","[30%, 35%)",">35"),
  #                             include.lowest = TRUE, right = FALSE)) %>%
  #     filter(!is.na(excess_percentage))
    
  
  
  plot_excess <- tm_shape( bezirk_geo  ) + 
    tm_fill("excess_percentage_o",
            # palette = "OrRd", 
            # legend.hist = TRUE,
             style = "jenks",
            # style = "kmeans",
            title = "Relative excess deaths",
            legend.is.portrait = FALSE) +
    tm_borders(alpha = 0.5)+
    tm_facets(by="sex",  ncol = 2, showNA = FALSE)+
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
  
    # plot_excess <- ggplot(data=bezirk_geo)+
    #   # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
    #   #                 pattern_spacing = 0.03,pattern_size=0.5 )+
    #   geom_sf(mapping = aes(fill =excess_perc_groups)) +
    #   facet_wrap(~sex, ncol = 2) +
    #   scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
    #   scale_pattern_manual("significant",
    #                        breaks =c("0", "1"),
    #                        labels=c("no", "yes"),
    #                        values = c("none","circle"))+
    #   ggtitle("Relative excess deaths - 1890")+
    #   theme(
    #     panel.grid.major=element_blank(),
    #     axis.title=element_blank(),
    #     axis.line=element_blank(),
    #     axis.text=element_blank(),
    #     axis.ticks=element_blank(),
    #     panel.border = element_blank(),
    #     legend.position = "bottom")
    # 
    return(plot_excess)
}

if(Year_Pan==1918) {
  # bezirk_geo <- bezirk_geo %>%
  #   mutate(
  #     excess_perc_groups =  cut(excess_percentage, 
  #                               breaks = c(-60, 0, 20, 30, 40, 50, 60, 80,100,140), 
  #                               labels = c("<0 %", "[0 %,20%)", "[20%, 30%)",
  #                                          "[30 %, 40%)", "[40%, 50%)",  "[50%, 60%)", "[60%, 80%)",
  #                                          "[80%, 100%)",">100"),
  #                               include.lowest = TRUE, right = FALSE)) %>%
  #   filter(!is.na(excess_percentage))
  # 
  # 
  # 
  # plot_excess <- ggplot(data=bezirk_geo)+
  #   # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
  #   #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  #   geom_sf(mapping = aes(fill =excess_perc_groups)) +
  #   facet_wrap(~sex, ncol = 2) +
  #   scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
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
  
  
  plot_excess <- tm_shape( bezirk_geo  ) + 
    tm_fill("excess_percentage_o",
            # palette = "Reds", 
            # legend.hist = TRUE,
            style = "jenks",
            # style = "kmeans",
            title = "Relative excess deaths",
            legend.is.portrait = FALSE) +
    tm_borders(alpha = 0.5)+
    tm_facets(by="sex",  ncol = 2, showNA = FALSE)+
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
  
  
  
  return(plot_excess)
}

if(Year_Pan==2020) {
#   bezirk_geo <- bezirk_geo %>%
#         mutate(excess_perc_groups =  cut(excess_percentage, 
#                                    breaks = c(-30, 0, 5, 10, 15,20, 30, 40, 50,70), 
#                                    labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",  "[10%, 15%)",
#                                               "[15 %, 20%)", "[20%, 30%)",  "[30%, 40%)",
#                                               "[40%, 50%)",">50"),
#                                    include.lowest = TRUE, right = FALSE)) %>%
#   filter(!is.na(excess_percentage))
# 
# plot_excess <- ggplot(data=bezirk_geo)+
#   # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
#   #                 pattern_spacing = 0.03,pattern_size=0.5 )+
#   geom_sf(mapping = aes(fill =excess_perc_groups)) +
#   facet_wrap(~sex, ncol = 2) +
#   scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
#   scale_pattern_manual("significant",
#                        breaks =c("0", "1"),
#                        labels=c("no", "yes"),
#                        values = c("none","circle"))+
#   ggtitle("Relative excess deaths - 2020")+
#   theme(
#     panel.grid.major=element_blank(),
#     axis.title=element_blank(),
#     axis.line=element_blank(),
#     axis.text=element_blank(),
#     axis.ticks=element_blank(),
#     panel.border = element_blank(),
#     legend.position = "bottom")

plot_excess <- tm_shape( bezirk_geo  ) + 
  tm_fill("excess_percentage_o",
          # palette = "Reds", 
          # legend.hist = TRUE,
          style = "jenks",
          # style = "kmeans",
          title = "Relative excess deaths",
          legend.is.portrait = FALSE) +
  tm_borders(alpha = 0.5)+
  tm_facets(by="sex",  ncol = 2, showNA = FALSE)+
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

return(plot_excess)
}

}

