function_cor_hospitals <- function(){
# 
#     load(paste0("../data/expected_death_inla1890.RData"))
#     Expected_death_Russian <-expected_deaths
#     load(paste0("../data/expected_death_inla1918.RData"))
#     Expected_death_Spanish <- expected_deaths
#     load(paste0("../data/expected_death_inla2020.RData"))
#     Expected_death_Covid <- expected_deaths


  load(paste0("../data/data_total.RData"))
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  load(paste0("../data/expected_death_inla1890.RData"))
  Expected_death_Russian <-expected_deaths
  load(paste0("../data/expected_death_inla1918.RData"))
  Expected_death_Spanish <- expected_deaths
  load(paste0("../data/expected_death_inla2020.RData"))
  Expected_death_Covid <- expected_deaths
  # 
  
  load(paste0("../data/Hospitals.RData"))
  
  Hospitals_data <- Hospitals %>%
   mutate(Year = as.factor(Year), 
     Year = recode(Year,"1915" = "1918"))
    
    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish,  Expected_death_Covid ) %>%
      ungroup() %>%
      left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join(Hospitals_data) %>%
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
             death_inc = death/population *10000,
             hospitals_inc= hospitals/population *10000)
             
    plot_Hospitals <- ggplot(data=data_excess) +
      geom_point(aes(x=hospitals_inc, y=excess_percentage, shape=Language,col=Language),  lwd=lwd_size_points ) +
      geom_smooth(aes(x=hospitals_inc, y=excess_percentage), method='lm',lwd=lwd_size, col=col_line,se=FALSE) +
      facet_wrap(~Year, nrow = 2,scales = "free") +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17))+
      ggtitle("Number of hospitals")+
      ylab("Relative Excess Mortality")+
      xlab("Number of Hospitals per 10'000 inhabitants") +
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

return(  plot_Hospitals)

}

function_test_hospitals <- function(Year_Pan){
  # 
  #     load(paste0("../data/expected_death_inla1890.RData"))
  #     Expected_death_Russian <-expected_deaths
  #     load(paste0("../data/expected_death_inla1918.RData"))
  #     Expected_death_Spanish <- expected_deaths
  #     load(paste0("../data/expected_death_inla2020.RData"))
  #     Expected_death_Covid <- expected_deaths
  
  
  load(paste0("../data/data_total.RData"))
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  load(paste0("../data/expected_death_inla1890.RData"))
  Expected_death_Russian <-expected_deaths
  load(paste0("../data/expected_death_inla1918.RData"))
  Expected_death_Spanish <- expected_deaths
  load(paste0("../data/expected_death_inla2020.RData"))
  Expected_death_Covid <- expected_deaths
  # 
  
  load(paste0("../data/Hospitals.RData"))
  
  Hospitals_data <- Hospitals %>%
    mutate(Year = as.factor(Year), 
           Year = recode(Year,"1915" = "1918"))
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish,  Expected_death_Covid ) %>%
    ungroup() %>%
    left_join(Canton) %>%
    filter(Year == 1890 | Year == 1918) %>%
    mutate(Year=as.factor(Year)) %>%
    full_join(Hospitals_data) %>%
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
           death_inc = death/population *10000,
           hospitals_inc= hospitals/population *10000) %>%
    filter(Year==Year_Pan)
  
  summary(lm(excess_percentage~hospitals_inc,data_excess))
}

