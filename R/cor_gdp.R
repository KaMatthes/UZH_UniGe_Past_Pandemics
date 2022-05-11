function_cor_gdp <- function(){

load(paste0("../data/expected_death_inla1890.RData"))
Expected_death_Russian <-expected_deaths
load(paste0("../data/expected_death_inla1918.RData"))
Expected_death_Spanish <- expected_deaths
load(paste0("../data/expected_death_inla2020.RData"))
Expected_death_Covid <- expected_deaths


  load(paste0("../data/data_total.RData"))
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  # 
  # load(paste0("data/expected_death_inla1890.RData"))
  # Expected_death_Russian <-expected_deaths
  # load(paste0("data/expected_death_inla1918.RData"))
  # Expected_death_Spanish <- expected_deaths
  # load(paste0("data/expected_death_inla2020.RData"))
  # Expected_death_Covid <- expected_deaths
  # 
  
  load(paste0("../data/GDP_rel2.RData"))
  
 GDP_data <- GDP%>%
    mutate(Year = recode(Year,
                         "1888" ="1890",
                         "1910" = "1918",
                         "2008" = "2020"))
    
    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish,  Expected_death_Covid ) %>%
      ungroup() %>%
      left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918 | Year == 2020) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join(GDP_data) %>%
      # left_join(prop_school_kids )%>%
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
             Language = as.factor(Language),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage),
             significant_dummy = ifelse(death > LL & death <UL,0,1),
             significant_dummy = as.factor( significant_dummy ),
             death_inc = death/population *100000) %>%
      filter(disp=="rel")

    plot_GDP_rel <- ggplot(data=data_excess) +
      geom_point(aes(x=GDP, y=excess_percentage, shape=Language,col=Language), lwd=3) +
      geom_smooth(aes(x=GDP, y=excess_percentage), col="black") +
      facet_wrap(~Year, ncol = 2,scales = "free") +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17))+
      ggtitle("Relative GDP per capita")+
      ylab("Relative Excess Mortality")+
      xlab("Relative GDP per Capita") +
      theme_bw() +
      theme(
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")
    
# cowplot::save_plot("output/plot_GDP_rel.pdf",plot_GDP_rel,base_height=10,base_width=15)

return(  plot_GDP_rel)

}

