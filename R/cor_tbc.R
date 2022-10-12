function_cor_tbc <- function(){

    # load(paste0("../data/expected_death_inla1890.RData"))
    # Expected_death_Russian <-expected_deaths
    # load(paste0("../data/expected_death_inla1918.RData"))
    # Expected_death_Spanish <- expected_deaths

  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)

  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths

  load("../data/Tbc.RData")

Tbc_data <- Tbc %>%
   mutate(Year = as.factor(Year),
     Year = recode(Year,"1915" = "1918"))

    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
      ungroup() %>%
      left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join(Tbc_data) %>%
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
                               "IR" = "German",
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
             tbc_inc= Tbc_Bezirk/pop_sum *1000) %>%
      group_by(Year) 
    

    plot_tbc <- ggplot(data=data_excess) +
      geom_point(aes(x=tbc_inc, y=excess_percentage, shape=Language,col=Language),  lwd=lwd_size_points ) +
      geom_smooth(aes(x=tbc_inc, y=excess_percentage), method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
      facet_wrap(~Year, ncol=2, scales="free_x") +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17))+
      ggtitle("Tuberculus mortality")+
      ylab("Relative Excess Mortality")+
      xlab("Tbc mortality per 1'000 inhabitants") +
      theme_bw() +
      theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")


    # else if (Davos=="no") {
    # 
    # plot_tbc <- ggplot(data=data_excess[!data_excess$Bezirk=="1849",]) +
    #   geom_point(aes(x=tbc_inc, y=excess_percentage, shape=Language,col=Language),  lwd=lwd_size_points ) +
    #   geom_smooth(aes(x=tbc_inc, y=excess_percentage), method='loess',se=TRUE,lwd=lwd_size, col=col_line) +
    #   facet_wrap(~Year, nrow = 2, scales="free_x") +
    #   scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   scale_shape_manual("Language region: ",values = c(15,16,17))+
    #   ggtitle("Tuberculus mortality")+
    #   ylab("Relative Excess Mortality")+
    #   xlab("Tuberculus mortality per 10'000 inhabitants") +
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
    # }


    # cowplot::save_plot("output/plot_Hospitals.pdf", plot_Hospitals,base_height=10,base_width=10)
    #
return(plot_tbc)

}

function_test_tbc <- function(Year_Pan) {
  # load(paste0("../data/expected_death_inla1890.RData"))
  # Expected_death_Russian <-expected_deaths
  # load(paste0("../data/expected_death_inla1918.RData"))
  # Expected_death_Spanish <- expected_deaths
  
  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  
  load("../data/Tbc.RData")
  
  Tbc_data <- Tbc %>%
    mutate(Year = as.factor(Year), 
           Year = recode(Year,"1915" = "1918"))
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
    ungroup() %>%
    left_join(Canton) %>%
    filter(Year == 1890 | Year == 1918) %>%
    mutate(Year=as.factor(Year)) %>%
    full_join(Tbc_data) %>%
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
                             "IR" = "German",
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
           tbc_inc= Tbc_Bezirk/pop_sum *10000) %>%
    # filter(!Bezirk==1849) %>%
    filter(Year==Year_Pan)
  # 
  # summary(gam(excess_percentage ~ s(tbc_inc),data=data_excess))
  summary(rlm(excess_percentage_o ~ tbc_inc,data=data_excess))
  # summary(lm(excess_percentage ~ tbc_inc,data=data_excess))
}



