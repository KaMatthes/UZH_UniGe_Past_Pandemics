function_maps_sex <- function(var){


  load(paste0("../data/expected_death_f_1890.RData"))
  death_1890_f <- expected_deaths

  load(paste0("../data/expected_death_f_1918.RData"))
  death_1918_f <- expected_deaths

  load(paste0("../data/expected_death_f_2020.RData"))
  death_2020_f <- expected_deaths

  load(paste0("../data/expected_death_m_1890.RData"))
  death_1890_m <- expected_deaths

  load(paste0("../data/expected_death_m_1918.RData"))
  death_1918_m <- expected_deaths

  load(paste0("../data/expected_death_m_2020.RData"))
  death_2020_m <- expected_deaths


  # load(paste0("data/expected_death_f_1890.RData"))
  # death_1890_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_f_1918.RData"))
  # death_1918_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_f_2020.RData"))
  # death_2020_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_m_1890.RData"))
  # death_1890_m <- expected_deaths
  # 
  # load(paste0("data/expected_death_m_1918.RData"))
  # death_1918_m <- expected_deaths
  # 
  # load(paste0("data/expected_death_m_2020.RData"))
  # death_2020_m <- expected_deaths

  data_excess <- rbind(death_1890_m,death_1918_m, death_2020_m,
                       death_1890_f,death_1918_f, death_2020_f )%>%
    select(-column_label)


data_excess <- data_excess %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890")%>%
  mutate(significant_dummy = ifelse(death > lpi & death < upi,0,1),
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
                                      "[-184,-1.86]" = "Q1",
                                      "(-1.86,10.9]" = "Q2",
                                      "(10.9,26.1]" = "Q3",
                                      "(26.1,57]" = "Q4",
                                      "(57,216]" = "Q5")) %>%
  group_by(Year, sex) %>%
  mutate(excess_per_quant = as.character(cut(excess_percentage_o,
                                             breaks=quantile(excess_percentage_o,
                                                             probs = seq(0, 1, length.out = no_classes_map + 1)),
                                             include.lowest = TRUE))) %>%
  mutate(excess_per_quant2= recode(excess_per_quant,
                               "[-63.5,-11.1]" = "Q1",
                               "(-11.1,-2]" = "Q2",
                               "(-2,7.01]" = "Q3",
                               "(7.01,14.2]" = "Q4",
                               "(14.2,59.8]" = "Q5",
                               
                               "[-31,-8.13]" = "Q1",
                               "(-8.13,-1.34]" = "Q2",
                               "(-1.34,5.42]" = "Q3",
                               "(5.42,17.8]" = "Q4",
                               "(17.8,82.4]" = "Q5",
                               
                               "[-38.5,19.6]" = "Q1",
                               "(19.6,32.5]" = "Q2",
                               "(32.5,41]" = "Q3",
                               "(41,57]" = "Q4",
                               "(57,138]" = "Q5",
                               
                               "[-11,28.5]" = "Q1",
                               "(28.5,42.7]" = "Q2",
                               "(42.7,52.1]" = "Q3",
                               "(52.1,72.8]" = "Q4",
                               "(72.8,152]" = "Q5",
                               
                               "[-47.9,-2.85]" = "Q1",
                               "(-2.85,5.99]" = "Q2",
                               "(5.99,13.5]" = "Q3",
                               "(13.5,23.2]" = "Q4",
                               "(23.2,148]" = "Q5",
                               
                               "[-20.2,2.72]" = "Q1",
                               "(2.72,9.14]" = "Q2",
                               "(9.14,17.8]" = "Q3",
                               "(17.8,29.4]" = "Q4",
                               "(29.4,78.8]" = "Q5")) %>%
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
                                          "[-63.5,-9.87]" = "Q1",
                                          "(-9.87,-1.77]" = "Q2",
                                          "(-1.77,5.74]" = "Q3",
                                          "(5.74,17]" = "Q4",
                                          "(17,82.4]" = "Q5",
                                          
                                          "[-38.5,25.3]" = "Q1",
                                          "(25.3,36.1]" = "Q2",
                                          "(36.1,48.7]" = "Q3",
                                          "(48.7,65.2]" = "Q4",
                                          "(65.2,152]" = "Q5",
                                          
                                          
                                          "[-47.9,-0.488]" = "Q1",
                                          "(-0.488,8.19]" = "Q2",
                                          "(8.19,14.8]" = "Q3",
                                          "(14.8,26.6]" = "Q4",
                                          "(26.6,148]" = "Q5" )) %>%
  ungroup() 


  # filter(!is.na(Bezirk))
if(var=="excess_rate_group") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey30", pattern_color="grey30",
                  pattern_spacing = 0.03,pattern_size=0.01 )+
  facet_wrap(Year~sex, ncol = 2) +
  scale_fill_manual("Quntile:",
                    values = col_5_groups_green_trans)+
  scale_pattern_manual("significant",
                       breaks =c("0", "1"),
                       labels=c("no", "yes"),
                       values = c("none","crosshatch"))+
  ggtitle("Excess Mortality")+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")
# cowplot::save_plot("output/plot_excess_sex.pdf",plot_excess,base_height=12,base_width=10)
}


else if (var=="excess_per_year_quant2") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_per_year_quant2),pattern_fill = "grey30", pattern_color="grey30",
                  pattern_spacing = 0.03,pattern_size=0.01 )+
  facet_wrap(Year~sex, ncol = 2) +
  scale_fill_manual("Quantile:",
                    values = col_5_groups_green_trans)+
  scale_pattern_manual("significant",
                       breaks =c("0", "1"),
                       labels=c("no", "yes"),
                       values = c("none","crosshatch"))+
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
 # cowplot::save_plot("output/plot_excess_sex_norm.pdf",plot_excess,base_height=12,base_width=10)
}

return(plot_excess)
}

