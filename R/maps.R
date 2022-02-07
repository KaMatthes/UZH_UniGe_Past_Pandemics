function_maps <- function(var){

R1 <- read_excel(paste0("../data/Expected_death_Russian_ZH_101.xlsx"))
R2 <- read_excel(paste0("../data/Expected_death_Russian_ZH_102.xlsx"))
R3 <- read_excel(paste0("../data/Expected_death_Russian_ZH_103.xlsx"))
R4 <- read_excel(paste0("../data/Expected_death_Russian_ZH_104.xlsx"))
R5 <- read_excel(paste0("../data/Expected_death_Russian_ZH_105.xlsx"))
R6 <- read_excel(paste0("../data/Expected_death_Russian_ZH_106.xlsx"))
R7 <- read_excel(paste0("../data/Expected_death_Russian_ZH_107.xlsx"))
R8 <- read_excel(paste0("../data/Expected_death_Russian_ZH_108.xlsx"))
R9 <- read_excel(paste0("../data/Expected_death_Russian_ZH_109.xlsx"))
R10 <- read_excel(paste0("../data/Expected_death_Russian_ZH_110.xlsx"))
R13 <- read_excel(paste0("../data/Expected_death_Russian_ZH_113.xlsx"))

load(paste0("../data/Expected_death_Russian.RData"))
Expected_death_Russian <- rbind(alldata,R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R13)
load(paste0("../data/Expected_death_Spanish.RData"))
Expected_death_Spanish <- alldata
load(paste0("../data/Expected_death_Covid.RData"))
Expected_death_Covid <- alldata


data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) 


# save(data_excess ,file=paste0("data/data_excess.RData"))
# write.xlsx(data_excess,file=paste0("data/data_excess.xlsx"),row.names=FALSE, overwrite = TRUE)

data_excess <- data_excess %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1919" |Year=="1890" |Year=="1889")

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
                                     "[-153,-4.76]" = "Q1",
                                     "(-4.76,8.14]" = "Q2",
                                     "(8.14,19.7]" = "Q3",
                                     "(19.7,46.7]" = "Q4",
                                     "(46.7,7.78e+03]" = "Q5")) %>%
  group_by(Year) %>%
  mutate(excess_quant = as.character(cut(excess_rate,
                                        breaks=quantile(excess_rate,
                                                        probs = seq(0, 1, length.out = no_classes_map + 1)),
                                        include.lowest = TRUE)),
         excess_per_quant = as.character(cut(excess_percentage_o,
                                             breaks=quantile(excess_percentage_o,
                                                             probs = seq(0, 1, length.out = no_classes_map + 1)),
                                             include.lowest = TRUE)),
         excess_per_quant_test = cut(excess_percentage_o,
                                             breaks=quantile(excess_percentage_o,
                                                             probs = seq(0, 1, length.out = no_classes_map + 1)),
                                             include.lowest = TRUE)) %>%
  mutate(
         excess_quant2= recode(excess_quant,
                              "[-153,-16.9]" = "Q1",
                              "(-16.9,-1.29]" = "Q2",
                              "(-1.29,8.47]" = "Q3",
                              "(8.47,25.9]" = "Q4",
                              "(25.9,109]" = "Q5",
                              "[-102,-6.73]" = "Q1",
                              "(-6.73,3.69]" = "Q2",
                              "(3.69,13.1]" = "Q3",
                              "(13.1,29.4]" = "Q4",
                              "(29.4,177]" = "Q5",
                              "[-21.3,44.1]" = "Q1",
                              "(44.1,53.8]" = "Q2",
                              "(53.8,67]" = "Q3",
                              "(67,90.4]" = "Q4",
                              "(90.4,202]" = "Q5",
                              "[-115,-14.3]" = "Q1",
                              "(-14.3,0.166]" = "Q2",
                              "(0.166,15.4]" = "Q3",
                              "(15.4,29.1]" = "Q4",
                              "(29.1,7.78e+03]" = "Q5", 
                              "[-20.5,4.45]" = "Q1",
                              "(4.45,8.19]" = "Q2",
                              "(8.19,13]" = "Q3",
                              "(13,18.6]" = "Q4",
                              "(18.6,51.8]" = "Q5")) %>%
  mutate(excess_per_quant2= recode(excess_per_quant,
                               "[-46.9,-8.06]" = "Q1",
                               "(-8.06,-0.582]" = "Q2",
                               "(-0.582,4.23]" = "Q3",
                               "(4.23,12.5]" = "Q4",
                               "(12.5,49.9]" = "Q5",
                               "[-46.4,-2.7]" = "Q1",
                               "(-2.7,2.17]" = "Q2",
                               "(2.17,6.97]" = "Q3",
                               "(6.97,13.7]" = "Q4",
                               "(13.7,731]" = "Q5",
                               "[-10.2,31.9]" = "Q1",
                               "(31.9,40.9]" = "Q2",
                               "(40.9,51]" = "Q3",
                               "(51,61.2]" = "Q4",
                               "(61.2,145]" = "Q5",
                               "[-42.5,-9.23]" = "Q1",
                               "(-9.23,0.157]" = "Q2",
                               "(0.157,11.2]" = "Q3",
                               "(11.2,23.8]" = "Q4",
                               "(23.8,5.42e+03]" = "Q5",
                               "[-18.7,5.28]" = "Q1",
                               "(5.28,11.2]" = "Q2",
                               "(11.2,17]" = "Q3",
                               "(17,24.7]" = "Q4",
                               "(24.7,66.7]" = "Q5"))   %>%
   ungroup 

  # filter(!is.na(Bezirk))
if(var=="excess_perc_groups") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =excess_perc_groups)) +
  facet_wrap(~Year, ncol = 2) +
  scale_fill_manual("Percentages:",
    values = col_5_groups_green_trans)+
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


else if (var=="excess_per_quant2") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =  excess_per_quant2)) +
  facet_wrap(~Year, ncol = 2)+
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

else if (var=="excess_rate_group") {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =   excess_rate_group)) +
  facet_wrap(~Year, ncol = 2)+
  scale_fill_manual("Quantile:",
    values = col_5_groups_green_trans)+
  ggtitle("Excess rate per 10'000")+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")

# cowplot::save_plot("output/plot_excess_rate.pdf",plot_excess,base_height=12,base_width=10)
}

else {
plot_excess <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =  excess_quant2)) +
  facet_wrap(~Year, ncol = 2)+
  scale_fill_manual("Quantile:",
    values = col_5_groups_green_trans)+
  ggtitle("Excess rate per 10'000 normalised")+
  theme(
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom")

# cowplot::save_plot("output/plot_excess_rate_norm.pdf",plot_excess,base_height=12,base_width=10)
}
return(plot_excess)
}

