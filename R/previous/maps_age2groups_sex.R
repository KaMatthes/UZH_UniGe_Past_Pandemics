function_maps_age2_sex <- function(var, Age){

if(Age=="0_69") {
  load(paste0("../data/expected_death_0_69_f_1890.RData"))
  death_1890_0_69_f <- expected_deaths

  load(paste0("../data/expected_death_0_69_f_1918.RData"))
  death_1918_0_69_f <- expected_deaths

  load(paste0("../data/expected_death_0_69_f_2020.RData"))
  death_2020_0_69_f<- expected_deaths

  load(paste0("../data/expected_death_0_69_m_1890.RData"))
  death_1890_0_69_m <- expected_deaths

  load(paste0("../data/expected_death_0_69_m_1918.RData"))
  death_1918_0_69_m <- expected_deaths

  load(paste0("../data/expected_death_0_69_m_2020.RData"))
  death_2020_0_69_m<- expected_deaths

  
  # load(paste0("data/expected_death_0_69_f_1890.RData"))
  # death_1890_0_69_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_0_69_f_1918.RData"))
  # death_1918_0_69_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_0_69_f_2020.RData"))
  # death_2020_0_69_f<- expected_deaths
  # 
  # load(paste0("data/expected_death_0_69_m_1890.RData"))
  # death_1890_0_69_m <- expected_deaths
  # 
  # load(paste0("data/expected_death_0_69_m_1918.RData"))
  # death_1918_0_69_m <- expected_deaths
  # 
  # load(paste0("data/expected_death_0_69_m_2020.RData"))
  # death_2020_0_69_m<- expected_deaths
  
  data_excess <- rbind(death_1890_0_69_f,death_1918_0_69_f, death_2020_0_69_f,
                       death_1890_0_69_m,death_1918_0_69_m, death_2020_0_69_m)%>%
    select(-column_label)

}
  
  else if(Age==">70") {

    load(paste0("../data/expected_death_70_f_1890.RData"))
    death_1890_70_f <- expected_deaths

    load(paste0("../data/expected_death_70_f_1918.RData"))
    death_1918_70_f <- expected_deaths

    load(paste0("../data/expected_death_70_f_2020.RData"))
    death_2020_70_f <- expected_deaths

    load(paste0("../data/expected_death_70_m_1890.RData"))
    death_1890_70_m <- expected_deaths

    load(paste0("../data/expected_death_70_m_1918.RData"))
    death_1918_70_m <- expected_deaths

    load(paste0("../data/expected_death_70_m_2020.RData"))
    death_2020_70_m<- expected_deaths

    
    # load(paste0("data/expected_death_70_f_1890.RData"))
    # death_1890_70_f <- expected_deaths
    # 
    # load(paste0("data/expected_death_70_f_1918.RData"))
    # death_1918_70_f <- expected_deaths
    # 
    # load(paste0("data/expected_death_70_f_2020.RData"))
    # death_2020_70_f <- expected_deaths
    # 
    # load(paste0("data/expected_death_70_m_1890.RData"))
    # death_1890_70_m <- expected_deaths
    # 
    # load(paste0("data/expected_death_70_m_1918.RData"))
    # death_1918_70_m <- expected_deaths
    # 
    # load(paste0("data/expected_death_70_m_2020.RData"))
    # death_2020_70_m<- expected_deaths
  
  
  data_excess <- rbind(death_1890_70_f,death_1918_70_f, death_2020_70_f,
                       death_1890_70_m,death_1918_70_m, death_2020_70_m)%>%
    select(-column_label)
  
  }


data_excess <- data_excess %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890")

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
                                         include.lowest = TRUE))) %>%
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
          excess_perc_year_quant2 = recode(excess_perc_year_quant,
                                          "[-55.7,-11.5]" = "Q1",
                                          "(-11.5,-2.25]" = "Q2",
                                          "(-2.25,3.91]" = "Q3",
                                          "(3.91,15.7]" = "Q4",
                                          "(15.7,82.7]" = "Q5",
                                          
                                          "[-18.1,39.9]" = "Q1",
                                          "(39.9,57.2]" = "Q2",
                                          "(57.2,69.8]" = "Q3",
                                          "(69.8,94]" = "Q4",
                                          "(94,301]" = "Q5",
                                          
                                          "[-91.7,-20.9]" = "Q1",
                                          "(-20.9,-2.58]" = "Q2",
                                          "(-2.58,11.8]" = "Q3",
                                          "(11.8,32.7]" = "Q4",
                                          "(32.7,8.5e+03]" = "Q5",
      
                                          
                                          "[-78.6,-16.7]" = "Q1",
                                          "(-16.7,1.33]" = "Q2",
                                          "(1.33,13]" = "Q3",
                                          "(13,31.4]" = "Q4",
                                          "(31.4,233]" = "Q5",
                                          
                                          "[-63.5,-21.8]" = "Q1",
                                          "(-21.8,-9.5]" = "Q2",
                                          "(-9.5,5.25]" = "Q3",
                                          "(5.25,19.6]" = "Q4",
                                          "(19.6,252]" = "Q5",
                                          
                                          "[-39,-0.676]" = "Q1",
                                          "(-0.676,9.76]" = "Q2",
                                          "(9.76,18.4]" = "Q3",
                                          "(18.4,32.3]" = "Q4",
                                          "(32.3,116]" = "Q5")) %>%
  ungroup() 
            
            
 

  # filter(!is.na(Bezirk))
if(var=="excess_perc_groups") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =excess_perc_year_cat)) +
  facet_wrap(Year~sex, ncol = 2) +
  scale_fill_manual("Percentages:",
    values = col_11_groups_green_trans)+
  ggtitle("Excess Mortality Percentage")+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")
# cowplot::save_plot("output/plot_excess_percentages.pdf",plot_excess,base_height=12,base_width=10)
}


else if (var=="excess_perc_year_quant2") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =  excess_perc_year_quant2)) +
  facet_wrap(Year~sex, ncol = 2) +
  scale_fill_manual("Quantile:",
    values = col_5_groups_green_trans)+
  ggtitle("Excess Mortality Percentage normalised")+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")
# 
# cowplot::save_plot("output/plot_excess_percentages_norm.pdf",plot_excess,base_height=12,base_width=10)
}

return(plot_excess)
}

