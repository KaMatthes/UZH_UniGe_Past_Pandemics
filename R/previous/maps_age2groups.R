function_maps_age2 <- function(var){


  load(paste0("../data/expected_death_0_69_1890.RData"))
  death_1890_0_69 <- expected_deaths

  load(paste0("../data/expected_death_0_69_1918.RData"))
  death_1918_0_69 <- expected_deaths

  load(paste0("../data/expected_death_0_69_2020.RData"))
  death_2020_0_69<- expected_deaths

  load(paste0("../data/expected_death_70_1890.RData"))
  death_1890_70 <- expected_deaths

  load(paste0("../data/expected_death_70_1918.RData"))
  death_1918_70 <- expected_deaths

  load(paste0("../data/expected_death_70_2020.RData"))
  death_2020_70<- expected_deaths


  # load(paste0("data/expected_death_0_69_1890.RData"))
  # death_1890_0_69 <- expected_deaths
  # 
  # load(paste0("data/expected_death_0_69_1918.RData"))
  # death_1918_0_69 <- expected_deaths
  # 
  # load(paste0("data/expected_death_0_69_2020.RData"))
  # death_2020_0_69<- expected_deaths
  # 
  # 
  # load(paste0("data/expected_death_70_1890.RData"))
  # death_1890_70 <- expected_deaths
  # 
  # load(paste0("data/expected_death_70_1918.RData"))
  # death_1918_70 <- expected_deaths
  # 
  # load(paste0("data/expected_death_70_2020.RData"))
  # death_2020_70<- expected_deaths


  data_excess <- rbind(death_1890_0_69,death_1918_0_69, death_2020_0_69,
                       death_1890_70,death_1918_70, death_2020_70)%>%
    select(-column_label)


data_excess <- data_excess %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890") %>%
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
                                      "[-1.56e+03,-13.3]" = "Q1",
                                      "(-13.3,3.75]" = "Q2",
                                      "(3.75,47.3]" = "Q3",
                                      "(47.3,101]" = "Q4",
                                      "(101,971]" = "Q5")) %>%
  group_by(Year, age_group) %>%
  mutate(excess_per_quant = as.character(cut(excess_percentage_o,
                                             breaks=quantile(excess_percentage_o,
                                                             probs = seq(0, 1, length.out = no_classes_map + 1)),
                                             include.lowest = TRUE))) %>%
  mutate(excess_per_quant2= recode(excess_per_quant,
                               "[-49.7,-8.98]" = "Q1",
                               "(-8.98,-2.22]" = "Q2",
                               "(-2.22,4.5]" = "Q3",
                               "(4.5,15.3]" = "Q4",
                               "(15.3,65.2]" = "Q5",
                               
                               "[-51.2,-12]" = "Q1",
                               "(-12,0.155]" = "Q2",
                               "(0.155,13.5]" = "Q3",
                               "(13.5,23.3]" = "Q4",
                               "(23.3,99.1]" = "Q5",
                               
                               "[4.16,44.9]" = "Q1",
                               "(44.9,56.6]" = "Q2",
                               "(56.6,70.8]" = "Q3",
                               "(70.8,87.8]" = "Q4",
                               "(87.8,150]" = "Q5",
                               
                               "[11.8,111]" = "Q1",
                               "(111,141]" = "Q2",
                               "(141,170]" = "Q3",
                               "(170,202]" = "Q4",
                               "(202,508]" = "Q5",
                               
                               "[-50.1,-17.1]" = "Q1",
                               "(-17.1,-7.47]" = "Q2",
                               "(-7.47,4.16]" = "Q3",
                               "(4.16,14.6]" = "Q4",
                               "(14.6,143]" = "Q5",
                               
                               "[-68.8,-14.5]" = "Q1",
                               "(-14.5,-3.54]" = "Q2",
                               "(-3.54,9.23]" = "Q3",
                               "(9.23,30.2]" = "Q4",
                               "(30.2,209]" = "Q5",
                               
                               "[-20.6,1.56]" = "Q1",
                               "(1.56,10.7]" = "Q2",
                               "(10.7,17.5]" = "Q3",
                               "(17.5,27.9]" = "Q4",
                               "(27.9,90]" = "Q5"))   %>%
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
                                          "[-51.2,-9.85]" = "Q1",
                                          "(-9.85,-0.919]" = "Q2",
                                          "(-0.919,7.01]" = "Q3",
                                          "(7.01,19.1]" = "Q4",
                                          "(19.1,99.1]" = "Q5",
                                          
                                          "[-50.1,-7.36]" = "Q1",
                                          "(-7.36,13.4]" = "Q2",
                                          "(13.4,45.3]" = "Q3",
                                          "(45.3,71.1]" = "Q4",
                                          "(71.1,150]" = "Q5",
                                          
                                          "[-68.8,-5.52]" = "Q1",
                                          "(-5.52,4.31]" = "Q2",
                                          "(4.31,14.6]" = "Q3",
                                          "(14.6,29.5]" = "Q4",
                                          "(29.5,209]" = "Q5" )) %>%
  ungroup() %>%
  mutate(age_group = factor(age_group, levels=c("0_69", ">70")))
            
            
 

  # filter(!is.na(Bezirk))
if(var=="excess_rate_group") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_rate_group),pattern_fill = "grey30", pattern_color="grey30",
                  pattern_spacing = 0.03,pattern_size=0.01 )+
  facet_wrap(Year~age_group, ncol = 2) +
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
# cowplot::save_plot("output/plot_excess_age.pdf",plot_excess,base_height=12,base_width=10)
}


else if (var=="excess_per_year_quant2") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf_pattern(aes(pattern=significant_dummy, fill=excess_per_year_quant2),pattern_fill = "grey30", pattern_color="grey30",
                  pattern_spacing = 0.03,pattern_size=0.01 )+
  facet_wrap(Year~age_group, ncol = 2) +
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
# cowplot::save_plot("output/plot_excess_age_norm.pdf",plot_excess,base_height=12,base_width=10)
}

return(plot_excess)
}

