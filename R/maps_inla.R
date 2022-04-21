function_maps_inla <- function(var, method){

    load(paste0("../data/expected_death_inla1890.RData"))
    Expected_death_Russian <-expected_deaths
    load(paste0("../data/expected_death_inla1918.RData"))
    Expected_death_Spanish <- expected_deaths
    load(paste0("../data/expected_death_inla2020.RData"))
    Expected_death_Covid <- expected_deaths

    load(paste0("data/expected_death_inla1890.RData"))
    Expected_death_Russian <-expected_deaths
    load(paste0("data/expected_death_inla1918.RData"))
    Expected_death_Spanish <- expected_deaths
    load(paste0("data/expected_death_inla2020.RData"))
    Expected_death_Covid <- expected_deaths

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

bezirk_geo <- read_sf("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
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
          excess_percentage = ifelse(excess_percentage<0, 0, excess_percentage),
          excess_percentage = ifelse(excess_percentage>100, 100, excess_percentage),
          excess_perc_groups =  as.numeric(excess_percentage),
         excess_perc_groups = replace(excess_perc_groups ,excess_perc_groups  < 20, "0-19"),
         excess_perc_groups = replace(excess_perc_groups ,excess_perc_groups  >= 20 & excess_perc_groups  < 40 , "20-39"),
         excess_perc_groups = replace(excess_perc_groups ,excess_perc_groups  >= 40 & excess_perc_groups  < 60 , "40-59"),
         excess_perc_groups = replace(excess_perc_groups ,excess_perc_groups  >= 60 & excess_perc_groups  < 80 , "60-79"),
         excess_perc_groups = replace(excess_perc_groups ,excess_perc_groups  >=80 , "80-100"),
         excess_perc_groups = replace(excess_perc_groups ,excess_perc_groups=="100" , "80-100"),
         excess_rate_total = as.character(cut(excess_rate,
                                         breaks=quantile(excess_rate,
                                                         probs = seq(0, 1, length.out = no_classes_map + 1)),
                                         include.lowest = TRUE)),
         excess_rate_group = excess_rate_total) %>%
  mutate( excess_rate_group = recode( excess_rate_group ,
                                     "[-80.8,2.51]" = "Q1",
                                     "(2.51,11.1]" = "Q2",
                                     "(11.1,22.9]" = "Q3",
                                     "(22.9,52.3]" = "Q4",
                                     "(52.3,214]" = "Q5")) %>%
  group_by(Year) %>%
  mutate(excess_per_quant = as.character(cut(excess_percentage_o,
                                             breaks=quantile(excess_percentage_o,
                                                             probs = seq(0, 1, length.out = no_classes_map + 1)),
                                             include.lowest = TRUE))) %>%
  mutate(excess_per_quant2= recode(excess_per_quant,
                               "[-41.9,-5.11]" = "Q1",
                               "(-5.11,0.736]" = "Q2",
                               "(0.736,3.75]" = "Q3",
                               "(3.75,10.2]" = "Q4",
                               "(10.2,43.7]" = "Q5",
                               
                               "[-6.72,31.4]" = "Q1",
                               "(31.4,37.7]" = "Q2",
                               "(37.7,45]" = "Q3",
                               "(45,55.7]" = "Q4",
                               "(55.7,133]" = "Q5",
                               
                               "[-14.1,4.83]" = "Q1",
                               "(4.83,9.47]" = "Q2",
                               "(9.47,15]" = "Q3",
                               "(15,21.3]" = "Q4",
                               "(21.3,58.1]" = "Q5"))   %>%
   ungroup 

  # filter(!is.na(Bezirk)


if(var=="excess_rate_group") {
plot_excess <- ggplot()+
  # geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
  #                 pattern_spacing = 0.03,pattern_size=0.5 )+
  geom_sf(data= bezirk_geo, aes(fill= excess_rate_group),alpha=1,col="black", size=0.1) +
  facet_wrap(~Year, ncol = 2) +
  scale_fill_manual("Quintile:",
                    values = col5viridis)+
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
# cowplot::save_plot("output/excess_rate_group_inla_test2.pdf",plot_excess,base_height=12,base_width=10)
}


else if (var=="excess_per_quant2") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_per_quant2),pattern_fill = "grey50", pattern_color="grey50",
                  pattern_spacing = 0.03,pattern_size=0.5)+
  facet_wrap(~Year, ncol = 2)+
  scale_fill_manual("Quintile:",
                    values = col5viridis)+
  scale_pattern_manual("significant",
                       breaks =c("0", "1"),
                       labels=c("no", "yes"),
                       values = c("none","circle"))+
  ggtitle("Excess Mortality normalised")+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")
# 
# cowplot::save_plot("output/excess_per_quant_inlatest2.pdf",plot_excess,base_height=12,base_width=10)
}

return(plot_excess)
}

