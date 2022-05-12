function_cor_child_mort <- function(){

    load(paste0("../data/expected_death_inla1890.RData"))
    Expected_death_Russian <- expected_deaths
    load(paste0("../data/expected_death_inla1918.RData"))
    Expected_death_Spanish <- expected_deaths
# 
#     load(paste0("data/expected_death_inla_child1890.RData"))
#     Expected_death_Russian <-expected_deaths
#     load(paste0("data/expected_death_inla_child1918.RData"))
#     Expected_death_Spanish <- expected_deaths

    load(paste0("../data/data_total.RData"))
    Canton <- data_total %>%
      dplyr::select(Canton, Bezirk) %>%
      distinct(Canton,Bezirk)
    
    
    # load(paste0("data/expected_death_inla1918.RData"))
    # Expected_death <- expected_deaths %>%
    #   mutate(excess_percentage_o = ((death-fit)/fit)*100,
    #          excess_percentage = round(((death-fit)/fit)*100,2),
    #          excess_perc_groups =  as.numeric(excess_percentage))

    
    load(paste0("../data/child_mortality.RData"))
    child_mortality <- child_mortality%>%
      mutate(Year = as.factor(Year)) %>%
      select(Year, Bezirk, prop_child_death, prop_norm, death) %>%
      filter(Year=="1889"  | Year =="1917") %>%
      mutate(Year = as.factor(Year), 
             Year = recode(Year,"1889" = "1890",
                           "1917" = "1918")) %>%
      select(-death)
  
    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
      ungroup() %>%
      left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join( child_mortality) %>%
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
             death_inc = death/population *100000)
  
    plot_child_mortality <- ggplot(data=data_excess) +
      geom_point(aes(x=prop_child_death, y=excess_percentage, shape=Language,col=Language), lwd=3) +
      geom_smooth(aes(x=prop_child_death, y=excess_percentage, col=Language), method='lm',lwd=1.5, se=FALSE) +
      facet_wrap(~Year, nrow = 2,scales = "free") +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17))+
      ggtitle("Child Mortality per 100'000 inhabitants")+
      ylab("Relative Excess Mortality")+
      xlab("Child Mortality per 100'000 inhabitants") +
      theme_bw() +
      theme(
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")
    
    # cowplot::save_plot("output/plot_child_mortality .pdf",plot_child_mortality ,base_height=10,base_width=10)
  
return( plot_child_mortality)

}

