function_maps_sex <- function(Year_Pan){

  load(paste0("data/expected_death_inla_0_19_f_1890.RData"))
  death_1890_0_19_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_0_19_f_1918.RData"))
  death_1918_0_19_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_0_19_f_2020.RData"))
  death_2020_0_19_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_20_39_f_1890.RData"))
  death_1890_20_39_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_20_39_f_1918.RData"))
  death_1918_20_39_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_20_39_f_2020.RData"))
  death_2020_20_39_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_40_69_f_1890.RData"))
  death_1890_40_69_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_40_69_f_1918.RData"))
  death_1918_40_69_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_40_69_f_2020.RData"))
  death_2020_40_69_f <- expected_deaths

  load(paste0("data/expected_death_inla_70_f_1890.RData"))
  death_1890_70_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_70_f_1918.RData"))
  death_1918_70_f <- expected_deaths
  
  load(paste0("data/expected_death_inla_70_f_2020.RData"))
  death_2020_70_f  <- expected_deaths
  
  load(paste0("data/expected_death_inla_0_19_m_1890.RData"))
  death_1890_0_19_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_0_19_m_1918.RData"))
  death_1918_0_19_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_0_19_m_2020.RData"))
  death_2020_0_19_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_20_39_m_1890.RData"))
  death_1890_20_39_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_20_39_m_1918.RData"))
  death_1918_20_39_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_20_39_m_2020.RData"))
  death_2020_20_39_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_40_69_m_1890.RData"))
  death_1890_40_69_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_40_69_m_1918.RData"))
  death_1918_40_69_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_40_69_m_2020.RData"))
  death_2020_40_69_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_70_m_1890.RData"))
  death_1890_70_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_70_m_1918.RData"))
  death_1918_70_m <- expected_deaths
  
  load(paste0("data/expected_death_inla_70_m_2020.RData"))
  death_2020_70_m  <- expected_deaths

  data_excess <- rbind(death_1890_0_19_f,death_1918_0_19_f,death_2020_0_19_f,
                       death_1890_20_39_f,death_1918_20_39_f,death_2020_20_39_f,
                       death_1890_40_69_f,death_1918_40_69_f, death_2020_40_69_f,
                       death_1890_70_f, death_1918_70_f, death_2020_70_f,
                       death_1890_0_19_m,death_1918_0_19_m,death_2020_0_19_m,
                       death_1890_20_39_m,death_1918_20_39_m,death_2020_20_39_m,
                       death_1890_40_69_m,death_1918_40_69_m, death_2020_40_69_m,
                       death_1890_70_m, death_1918_70_m, death_2020_70_m)%>%
    select(-column_label) %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk),
         age_group =  age_group.x) %>%
  filter(Year==Year_Pan) 
  
# sf::sf_use_s2(TRUE)

bezirk_geo <- read_sf("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
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
  filter(!is.na(excess_percentage))



if(Year_Pan==1890) {
  bezirk_geo <- bezirk_geo %>%
    mutate(
      excess_perc_groups =  cut(excess_percentage, 
                                breaks = c(-120, 0, 5, 10, 15, 20, 25, 30,35,430), 
                                labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
                                           "[10 %, 15%)", "[15%, 20%)",  "[20%, 25%)",
                                           "[25%, 30%)","[30%, 35%)",">35"),
                                include.lowest = TRUE, right = FALSE)) %>%
    filter(!is.na(excess_percentage))
  
  plot_excess <- ggplot(data=bezirk_geo)+
    # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
    #                 pattern_spacing = 0.03,pattern_size=0.5 )+
    geom_sf(mapping = aes(fill =excess_perc_groups)) +
    facet_wrap(sex~age_group, ncol = 4) +
    scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
    scale_pattern_manual("significant",
                         breaks =c("0", "1"),
                         labels=c("no", "yes"),
                         values = c("none","circle"))+
    ggtitle("Excess Mortality")+
    theme(
      panel.grid.major=element_blank(),
      axis.title=element_blank(),
      axis.line=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom")
  
  return(plot_exces)
}
            
 

if(Year_Pan==1918) {
  bezirk_geo <- bezirk_geo %>%
    mutate(
      excess_perc_groups =  cut(excess_percentage, 
                                breaks = c(-130, 0, 10, 20, 30, 40, 90, 130, 180,860), 
                                labels = c("<0 %", "[0 %,10%)", "[10%, 20%)",
                                           "[20 %, 30%)", "[30%, 40%)",  "[40%, 90%)",
                                           "[90%, 130%)","[130%, 180%)",">180"),
                                include.lowest = TRUE, right = FALSE)) %>%
    filter(!is.na(excess_percentage))
  
  plot_excess <- ggplot(data=bezirk_geo)+
    # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
    #                 pattern_spacing = 0.03,pattern_size=0.5 )+
    geom_sf(mapping = aes(fill =excess_perc_groups)) +
    facet_wrap(sex~age_group, ncol = 4) +
    scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
    scale_pattern_manual("significant",
                         breaks =c("0", "1"),
                         labels=c("no", "yes"),
                         values = c("none","circle"))+
    ggtitle("Excess Mortality")+
    theme(
      panel.grid.major=element_blank(),
      axis.title=element_blank(),
      axis.line=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom")
  
  return(plot_exces)
}


if(Year_Pan==2020) {
  bezirk_geo <- bezirk_geo %>%
    mutate(excess_perc_groups =  cut(excess_percentage, 
                                     breaks = c(-120, 0, 5, 10, 15,20, 30, 40, 50,3.823e+20 ), 
                                     labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",  "[10%, 15%)",
                                                "[15 %, 20%)", "[20%, 30%)",  "[30%, 40%)",
                                                "[40%, 50%)",">50"),
                                     include.lowest = TRUE, right = FALSE)) %>%
    filter(!is.na(excess_percentage))
  
  plot_excess <- ggplot(data=bezirk_geo)+
    # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
    #                 pattern_spacing = 0.03,pattern_size=0.5 )+
    geom_sf(mapping = aes(fill =excess_perc_groups)) +
    facet_wrap(sex~age_group, ncol = 4) +
    scale_fill_manual("",values = c("#C6DBEF",col8viridis))+
    scale_pattern_manual("significant",
                         breaks =c("0", "1"),
                         labels=c("no", "yes"),
                         values = c("none","circle"))+
    ggtitle("Excess Mortality")+
    theme(
      panel.grid.major=element_blank(),
      axis.title=element_blank(),
      axis.line=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom")
  
  return(plot_exces)
}

}

