function_maps_age_1918 <- function(){


  # load(paste0("data/expected_death_inla0_19_1918.RData"))
  # death_1918_0_19 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla20_39_1918.RData"))
  # death_1918_20_39 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla40_69_1918.RData"))
  # death_1918_40_69 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla70_1918.RData"))
  # death_1918_70 <- expected_deaths
  
  load(paste0("../data/expected_death_inla0_19_1918.RData"))
  death_1918_0_19 <- expected_deaths
  
  load(paste0("../data/expected_death_inla20_39_1918.RData"))
  death_1918_20_39 <- expected_deaths
  
  load(paste0("../data/expected_death_inla40_69_1918.RData"))
  death_1918_40_69 <- expected_deaths
  
  load(paste0("../data/expected_death_inla70_1918.RData"))
  death_1918_70 <- expected_deaths


  data_excess <- rbind(death_1918_0_19,death_1918_20_39, death_1918_40_69,death_1918_70)%>%
    select(-column_label) %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk),
         age_group =  age_group.x) %>%
    filter(Year==1918) %>%
    mutate(significant_dummy = ifelse(death > LL & death < UL,0,1),
           significant_dummy = as.factor( significant_dummy ),
           age_group=factor(age_group, levels=c("0_19","20_39","40_69", ">70")))
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
           excess_percentage = round(((death-fit)/fit)*100,2)) %>%
    filter(!is.na(excess_percentage))  %>%
    mutate(excess_perc_groups =  cut(excess_percentage, 
                              breaks = c(-130, 0, 10, 20, 30, 40, 70, 180, 250,860), 
                              labels = c("<0 %", "[0 %,10%)", "[10%, 20%)",
                                         "[20 %, 30%)", "[30%, 40%)",  "[40%, 70%)",
                                         "[70%, 180%)","[180%, 250%)",">250"),
                              include.lowest = TRUE, right = FALSE))
    
    plot_excess <- ggplot(data=bezirk_geo)+
      # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_perc_groups),pattern_fill = "grey50", pattern_color="grey50",
      #                 pattern_spacing = 0.03,pattern_size=0.5 )+
      geom_sf(mapping = aes(fill =excess_perc_groups)) +
      facet_wrap(~age_group, ncol = 2) +
      scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
      scale_pattern_manual("significant",
                           breaks =c("0", "1"),
                           labels=c("no", "yes"),
                           values = c("none","circle"))+
      ggtitle("Relative excess deaths - 1918")+
      theme(
        panel.grid.major=element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom")
    
  # cowplot::save_plot("output/excess_rate_group_inla_test2.pdf",plot_excess,base_height=12,base_width=15)
    
    return(plot_excess)
  }

        

