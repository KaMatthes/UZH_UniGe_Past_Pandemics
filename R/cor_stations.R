function_cor_stations <- function(){

  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  # 
  
  load("../data/Stations.RData")
  load("../data/Area.RData")
  
  dat_area <- Area %>%
    filter(Year == 1890 | Year == 1918) %>%
    mutate(Bezirk= as.factor(Bezirk),
           Year=as.factor(Year))
    
    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
      ungroup() %>%
      left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join(Stations) %>%
      left_join(dat_area) %>%
      mutate(Language = Canton,
             Language = as.character(Language),
             Language = recode(Language,
                               "AG" = "German",
                               "AI" = "German",
                               "AR" = "German",
                               "BE" = "German",
                               "BL" = "German",
                               "BS" = "German",
                               "FR" = "French",
                               "GE" = "French",
                               "GL" = "German",
                               "GR" = "German",
                               "JU" = "French",
                               "LU" = "German",
                               "NE" = "French",
                               "NW" = "German",
                               "OW" = "German",
                               "SG" = "German",
                               "SH" = "German",
                               "SO" = "German",
                               "SZ" = "German",
                               "TG" = "German",
                               "TI" = "Italian",
                               "UR" = "German",
                               "VD" = "French",
                               "VS" = "French",
                               "ZG" = "German",
                               "ZH" = "German"),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage),
             significant_dummy = ifelse(death > LL & death <UL,0,1),
             significant_dummy = as.factor( significant_dummy ),
             station_area = n_stat/area_Bezirk)
             
    plot_Stations <- ggplot(data=data_excess) +
      geom_point(aes(x=station_area, y=excess_percentage, shape=Language,col=Language),  lwd=lwd_size_points ) +
      geom_smooth(aes(x=station_area, y=excess_percentage),  method='lm',se=TRUE,lwd=lwd_size, col=col_line) +
      facet_wrap(~Year, nrow = 2,scales = "free") +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17))+
      ggtitle("Number of train stations")+
      ylab("Relative Excess Mortality")+
      xlab("Number of train stations per km2") +
      theme_bw() +
      theme(
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")
    
    # cowplot::save_plot("output/plot_Hospitals.pdf", plot_Hospitals,base_height=10,base_width=10)
    # 

return(     plot_Stations)

}

function_test_stations <- function(Year_Pan){
  # 
  #     load(paste0("../data/expected_death_inla1890.RData"))
  #     Expected_death_Russian <-expected_deaths
  #     load(paste0("../data/expected_death_inla1918.RData"))
  #     Expected_death_Spanish <- expected_deaths
  #     load(paste0("../data/expected_death_inla2020.RData"))
  #     Expected_death_Covid <- expected_deaths
  
  
  
  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  # 
  
  load("../data/Stations.RData")
  load("../data/Area.RData")
  
  dat_area <- Area %>%
    filter(Year == 1890 | Year == 1918) %>%
    mutate(Bezirk= as.factor(Bezirk),
           Year=as.factor(Year))
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
    ungroup() %>%
    left_join(Canton) %>%
    filter(Year == 1890 | Year == 1918) %>%
    mutate(Year=as.factor(Year)) %>%
    full_join(Stations) %>%
    left_join(dat_area) %>%
    mutate(Language = Canton,
           Language = as.character(Language),
           Language = recode(Language,
                             "AG" = "German",
                             "AI" = "German",
                             "AR" = "German",
                             "BE" = "German",
                             "BL" = "German",
                             "BS" = "German",
                             "FR" = "French",
                             "GE" = "French",
                             "GL" = "German",
                             "GR" = "German",
                             "JU" = "French",
                             "LU" = "German",
                             "NE" = "French",
                             "NW" = "German",
                             "OW" = "German",
                             "SG" = "German",
                             "SH" = "German",
                             "SO" = "German",
                             "SZ" = "German",
                             "TG" = "German",
                             "TI" = "Italian",
                             "UR" = "German",
                             "VD" = "French",
                             "VS" = "French",
                             "ZG" = "German",
                             "ZH" = "German"),
           excess_percentage_o = ((death-fit)/fit)*100,
           excess_percentage = round(((death-fit)/fit)*100,2),
           excess_perc_groups =  as.numeric(excess_percentage),
           significant_dummy = ifelse(death > LL & death <UL,0,1),
           significant_dummy = as.factor( significant_dummy ),
           station_area = n_stat/area_Bezirk) %>%
    filter(Year==Year_Pan)
  
  # summary(gam(excess_percentage ~station_area,data=data_excess))
  summary(lm(excess_percentage ~ station_area,data=data_excess))
}

