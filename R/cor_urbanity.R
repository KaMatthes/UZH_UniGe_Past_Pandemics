function_cor_urbanity <- function(){
  
  
  # 
  # load(paste0("data/expected_death_inla1890.RData"))
  # Expected_death_Russian <-expected_deaths
  # load(paste0("data/expected_death_inla1918.RData"))
  # Expected_death_Spanish <- expected_deaths
  # load(paste0("data/expected_death_inla2020.RData"))
  # Expected_death_Covid <- expected_deaths
  
  load("../data/expected_death_inla1890.RData")
Expected_death_Russian <-expected_deaths
load("../data/expected_death_inla1918.RData")
Expected_death_Spanish <- expected_deaths
load("../data/expected_death_inla2020.RData")
Expected_death_Covid <- expected_deaths

  # load("data/data_total.RData")
  # Canton <- data_total %>%
  #   dplyr::select(Canton, Bezirk) %>%
  #   distinct(Canton,Bezirk)

  
  load("../data/Urbanity.RData")
  
data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) %>%
      ungroup() %>%
      # left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918 | Year == 2020) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join(Urbanity) %>%
      mutate(city_bezirk = as.factor(city_bezirk),
             # Language = Canton,
             # Language = as.character(Language),
             # Language = recode(Language,
             #                   "AG" = "German",
             #                   "AI" = "German",
             #                   "AR" = "German",
             #                   "BE" = "German",
             #                   "BL" = "German",
             #                   "BS" = "German",
             #                   "FR" = "French",
             #                   "GE" = "French",
             #                   "GL" = "German",
             #                   "GR" = "German",
             #                   "IR" = "German",
             #                   "JU" = "French",
             #                   "LU" = "German",
             #                   "NE" = "French",
             #                   "NW" = "German",
             #                   "OW" = "German",
             #                   "SG" = "German",
             #                   "SH" = "German",
             #                   "SO" = "German",
             #                   "SZ" = "German",
             #                   "TG" = "German",
             #                   "TI" = "Italian",
             #                   "UR" = "German",
             #                   "VD" = "French",
             #                   "VS" = "French",
             #                   "ZG" = "German",
             #                   "ZH" = "German"),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage),
             significant_dummy = ifelse(death > LL & death <UL,0,1),
             significant_dummy = as.factor( significant_dummy ))
   
    
plot_urbanity <- ggplot(data=data_excess,aes(x=Year, y=excess_percentage_o, fill = city_bezirk)) +
  geom_split_violin() +
  stat_summary(fun = median,
               width = 0.25,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=TRUE) +
  scale_shape_manual("", 
                     values=c("median")) +
  scale_fill_manual("Urbanity: ",
                    values = c(16,15), 
                    labels=c("no","yes"))  +
  ggtitle("Urbanity")+
  ylab("Relative Excess Mortality")+
  xlab("Year") +
  theme_bw() +
  theme(
    axis.text=element_text(color="black",size=axis_size),
    axis.title=element_text(size=axis_size_title ),
    legend.text=element_text(size=legend_size),
    legend.title =element_text(size=legend_size_title),
    plot.title = element_text(size=size_title),
    legend.position = "bottom")

    # plot_urbanity <- ggplot(data=data_excess,aes(x=city_bezirk, y=excess_percentage)) +
    #   geom_violin(aes(x=city_bezirk, y=excess_percentage),draw_quantiles = 0.5)+
    #   geom_jitter(aes(x=city_bezirk, y=excess_percentage),height = 0, width = 0.1)+
    #   facet_wrap(~Year, nrow = 2,scales = "free") +
    #   # scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   # scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   # scale_shape_manual("Language region: ",values = c(15,16,17))+
    #   ggtitle("Urbanity")+
    #   ylab("Relative Excess Mortality")+
    #   xlab("") +
    #   theme_bw() +
    #   theme(
    #     strip.text.x=element_text(size=15),
    #     axis.text.x=element_text(color="black",size=10),
    #     axis.title=element_text(size=15),
    #     legend.text=element_text(size=15),
    #     legend.title =element_text(size=15),
    #     plot.title = element_text(size=15),
    #     legend.position = "bottom")
    # 
    # 
    # cowplot::save_plot("output/plot_Hospitals.pdf", plot_Hospitals,base_height=10,base_width=10)
    # 

return(plot_urbanity)

}

function_test_urbanity <-  function(Year_Pan)
  {
  
  
  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  load("../data/expected_death_inla2020.RData")
  Expected_death_Covid <- expected_deaths
  
  # load("../data/data_total.RData")
  # Canton <- data_total %>%
  #   dplyr::select(Canton, Bezirk) %>%
  #   distinct(Canton,Bezirk)
  # 
  
  load("../data/Urbanity.RData")
  
  
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) %>%
  ungroup() %>%
  # left_join(Canton) %>%
  # filter(Year == Year_Pan) %>%
  mutate(Year=as.factor(Year)) %>%
  full_join(Urbanity) %>%
  mutate(city_bezirk = as.factor(city_bezirk),
         # Language = Canton,
         # Language = as.character(Language),
         # Language = recode(Language,
         #                   "AG" = "German",
         #                   "AI" = "German",
         #                   "AR" = "German",
         #                   "BE" = "German",
         #                   "BL" = "German",
         #                   "BS" = "German",
         #                   "FR" = "French",
         #                   "GE" = "French",
         #                   "GL" = "German",
         #                   "GR" = "German",
         #                   "IR" = "German",
         #                   "JU" = "French",
         #                   "LU" = "German",
         #                   "NE" = "French",
         #                   "NW" = "German",
         #                   "OW" = "German",
         #                   "SG" = "German",
         #                   "SH" = "German",
         #                   "SO" = "German",
         #                   "SZ" = "German",
         #                   "TG" = "German",
         #                   "TI" = "Italian",
         #                   "UR" = "German",
         #                   "VD" = "French",
         #                   "VS" = "French",
         #                   "ZG" = "German",
         #                   "ZH" = "German"),
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage = round(((death-fit)/fit)*100,2),
         excess_perc_groups =  as.numeric(excess_percentage),
         significant_dummy = ifelse(death > LL & death <UL,0,1),
         significant_dummy = as.factor( significant_dummy ))%>%
    filter(Year==Year_Pan)

summary(lm(excess_percentage~city_bezirk,data_excess))
        
}