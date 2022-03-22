function_maps_age2_inla <- function(var){


  load(paste0("../data/expected_death_inla0_69_1890.RData"))
  death_1890_0_69 <- expected_deaths

  load(paste0("../data/expected_death_inla0_69_1918.RData"))
  death_1918_0_69 <- expected_deaths

  load(paste0("../data/expected_death_inla0_69_2020.RData"))
  death_2020_0_69<- expected_deaths

  load(paste0("../data/expected_death_inla70_1890.RData"))
  death_1890_70 <- expected_deaths

  load(paste0("../data/expected_death_inla70_1918.RData"))
  death_1918_70 <- expected_deaths

  load(paste0("../data/expected_death_inla70_2020.RData"))
  death_2020_70<- expected_deaths


  # load(paste0("data/expected_death_inla0_69_1890.RData"))
  # death_1890_0_69 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla0_69_1918.RData"))
  # death_1918_0_69 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla0_69_2020.RData"))
  # death_2020_0_69<- expected_deaths
  # 
  # 
  # load(paste0("data/expected_death_inla70_1890.RData"))
  # death_1890_70 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla70_1918.RData"))
  # death_1918_70 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla70_2020.RData"))
  # death_2020_70<- expected_deaths


  data_excess <- rbind(death_1890_0_69,death_1918_0_69, death_2020_0_69,
                       death_1890_70,death_1918_70, death_2020_70)%>%
    select(-column_label) %>%
    rename(age_group = age_group.x)


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
  mutate(excess_percentage_o = ((death-fit)/fit)*100,
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
                                      "[-846,-4.2]" = "Q1",
                                      "(-4.2,3.82]" = "Q2",
                                      "(3.82,43.8]" = "Q3",
                                      "(43.8,92.4]" = "Q4",
                                      "(92.4,754]" = "Q5")) %>%
  group_by(Year, age_group) %>%
  mutate(excess_per_quant = as.character(cut(excess_percentage_o,
                                             breaks=quantile(excess_percentage_o,
                                                             probs = seq(0, 1, length.out = no_classes_map + 1)),
                                             include.lowest = TRUE))) %>%
  mutate(excess_per_quant2= recode(excess_per_quant,
                               "[-41.4,-5.22]" = "Q1",
                               "(-5.22,-0.263]" = "Q2",
                               "(-0.263,3.59]" = "Q3",
                               "(3.59,9.51]" = "Q4",
                               "(9.51,76.6]" = "Q5",
                               
                               "[-36.5,-9.15]" = "Q1",
                               "(-9.15,-1.38]" = "Q2",
                               "(-1.38,7.64]" = "Q3",
                               "(7.64,14.8]" = "Q4",
                               "(14.8,59.5]" = "Q5",
                               
                               "[6.41,40]" = "Q1",
                               "(40,54.1]" = "Q2",
                               "(54.1,62.3]" = "Q3",
                               "(62.3,70.6]" = "Q4",
                               "(70.6,156]" = "Q5",
                               
                               "[-40.6,-5.97]" = "Q1",
                               "(-5.97,0.754]" = "Q2",
                               "(0.754,7.59]" = "Q3",
                               "(7.59,17.8]" = "Q4",
                               "(17.8,47.7]" = "Q5",
                               
                               "[-46.4,-10.3]" = "Q1",
                               "(-10.3,-2.42]" = "Q2",
                               "(-2.42,3.99]" = "Q3",
                               "(3.99,16.9]" = "Q4",
                               "(16.9,52.7]" = "Q5",
                               
                               "[-19.1,1.59]" = "Q1",
                               "(1.59,8.35]" = "Q2",
                               "(8.35,14.4]" = "Q3",
                               "(14.4,22.6]" = "Q4",
                               "(22.6,70.3]" = "Q5"))   %>%
   ungroup()  %>%
  group_by(Year) %>%
  mutate(excess_perc_year = round(((death-fit)/fit)*100,2),
          excess_perc_year = ifelse(excess_perc_year<0, 0, excess_perc_year),
          excess_perc_year = ifelse(excess_perc_year>100, 200, excess_perc_year),
          excess_perc_year_cat = cut(excess_perc_year, c(seq(0, 100, 10), 200), 
                        include.lowest = TRUE, right = FALSE),
          excess_perc_year_cat = as.character( excess_perc_year_cat),
          excess_perc_year_cat = replace( excess_perc_year_cat, excess_perc_year_cat=="[0,20)", "0-19"),
          excess_perc_year_cat = replace( excess_perc_year_cat , excess_perc_year_cat=="[20,40)", "20-39"),
          excess_perc_year_cat = replace( excess_perc_year_cat , excess_perc_year_cat=="[40,60)" , "40-59"),
          excess_perc_year_cat = replace( excess_perc_year_cat , excess_perc_year_cat=="[60,80)" , "60-79"),
          excess_perc_year_cat = replace( excess_perc_year_cat , excess_perc_year_cat=="[80,100)" , "80-99"),
          excess_perc_year_cat = replace( excess_perc_year_cat , excess_perc_year_cat=="[100,200]" , ">=100"),
          excess_perc_year_cat = as.factor( excess_perc_year_cat),
          excess_perc_year_quant = as.character(cut(excess_percentage_o,
                                                     breaks=quantile(excess_percentage_o,
                                                                     probs = seq(0, 1, length.out = no_classes_map + 1)),
                                                     include.lowest = TRUE)),
          excess_per_year_quant2 = recode(excess_perc_year_quant,
                                          "[-41.4,-8.01]" = "Q1",
                                          "(-8.01,-0.335]" = "Q2",
                                          "(-0.335,5.44]" = "Q3",
                                          "(5.44,12.4]" = "Q4",
                                          "(12.4,76.6]" = "Q5",
                                          
                                          "[-40.6,0.768]" = "Q1",
                                          "(0.768,16.3]" = "Q2",
                                          "(16.3,40.3]" = "Q3",
                                          "(40.3,62.2]" = "Q4",
                                          "(62.2,156]" = "Q5",
                                          
                                          "[-46.4,-4.09]" = "Q1",
                                          "(-4.09,2.86]" = "Q2",
                                          "(2.86,10.8]" = "Q3",
                                          "(10.8,19.4]" = "Q4",
                                          "(19.4,70.3]" = "Q5" )) %>%
  ungroup() %>%
  mutate(age_group = factor(age_group, levels=c("0_69", ">70")))
            
            
 

  # filter(!is.na(Bezirk))
if(var=="excess_rate_group") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey50", pattern_color="grey50",
                  pattern_spacing = 0.03,pattern_size=0.5 )+
  facet_wrap(Year~age_group, ncol = 2) +
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
# cowplot::save_plot("output/plot_excess_age_inla.pdf",plot_excess,base_height=12,base_width=10)
}


else if (var=="excess_per_year_quant2") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_per_year_quant2),pattern_fill = "grey50", pattern_color="grey50",
                  pattern_spacing = 0.03,pattern_size=0.5)+
  facet_wrap(Year~age_group, ncol = 2) +
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
# cowplot::save_plot("output/plot_excess_age_norm_inla.pdf",plot_excess,base_height=12,base_width=10)
}

return(plot_excess)
}

