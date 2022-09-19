function_cor_area <- function(){

    # load(paste0("../data/expected_death_inla1890.RData"))
    # Expected_death_Russian <-expected_deaths
    # load(paste0("../data/expected_death_inla1918.RData"))
    # Expected_death_Spanish <- expected_deaths

  # load("data/data_total.RData")
  # Canton <- data_total %>%
  #   dplyr::select(Canton, Bezirk) %>%
  #   distinct(Canton,Bezirk)

  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  load("../data/expected_death_inla2020.RData")
   Expected_death_Covid <- expected_deaths
  
  load("../data/Area.RData")
  
area_data <- Area %>%
  mutate(Year = as.factor(Year)) %>%
  filter(Year == 1890 | Year == 1918 | Year == 2020) 

data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) %>%
      ungroup() %>%
      # left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918 | Year == 2020) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join(area_data) %>%
      mutate(
        # Language = Canton,
      #        Language = as.character(Language),
      #        Language = recode(Language,
      #                          "AG" = "German",
      #                          "AI" = "German",
      #                          "AR" = "German",
      #                          "BE" = "German",
      #                          "BL" = "German",
      #                          "BS" = "German",
      #                          "FR" = "French",
      #                          "GE" = "French",
      #                          "GL" = "German",
      #                          "GR" = "German",
      #                          "IR" = "German",
      #                          "JU" = "French",
      #                          "LU" = "German",
      #                          "NE" = "French",
      #                          "NW" = "German",
      #                          "OW" = "German",
      #                          "SG" = "German",
      #                          "SH" = "German",
      #                          "SO" = "German",
      #                          "SZ" = "German",
      #                          "TG" = "German",
      #                          "TI" = "Italian",
      #                          "UR" = "German",
      #                          "VD" = "French",
      #                          "VS" = "French",
      #                          "ZG" = "German",
      #                          "ZH" = "German"),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage),
             significant_dummy = ifelse(death > LL & death <UL,0,1),
             significant_dummy = as.factor( significant_dummy )) %>%
  group_by(Year) %>%
  mutate(median_denspop = median(densPop),
         dens_group = ifelse(densPop <median_denspop, "small", "large" )) %>%
  ungroup() %>%
  mutate(dens_group = factor( dens_group, levels = c("small", "large")))

# excess_rate_total = as.character(cut(excess_rate,
#                                 breaks=quantile(excess_rate,
#                                                 probs = seq(0, 1, length.out = no_classes_map + 1)),
#                                 include.lowest = TRUE)),

             
    # plot_area <- ggplot(data=data_excess) +
    #   geom_point(aes(x= densPop, y=excess_percentage, shape=Language,col=Language),  lwd=lwd_size_points ) +
    #   geom_smooth(aes(x=densPop, y=excess_percentage),  method='loess',se=TRUE,lwd=lwd_size, col=col_line) +
    #   facet_wrap(~Year, nrow = 2,scales = "free") +
    #   scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   scale_shape_manual("Language region: ",values = c(15,16,17))+
    #   ggtitle("Population density")+
    #   ylab("Relative Excess Mortality")+
    #   xlab("Population density") +
    #   theme_bw() +
    #   theme(
    #     strip.text.x=element_text(size=15),
    #     axis.text.x=element_text(color="black",size=10),
    #     axis.title=element_text(size=15),
    #     legend.text=element_text(size=15),
    #     legend.title =element_text(size=15),
    #     plot.title = element_text(size=15),
    #     legend.position = "bottom")
    
    plot_area <- ggplot(data=data_excess,aes(x=Year, y=excess_percentage_o, fill = dens_group)) +
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
      scale_fill_manual("Number of hopsitals: ",values = c(16,15))  +
      ggtitle("Population density")+
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
    
   return(plot_area)

}


function_test_area <- function(Year_Pan){
  
  # load(paste0("../data/expected_death_inla1890.RData"))
  # Expected_death_Russian <-expected_deaths
  # load(paste0("../data/expected_death_inla1918.RData"))
  # Expected_death_Spanish <- expected_deaths
  
  load("../data/data_total.RData")
  # Canton <- data_total %>%
  #   dplyr::select(Canton, Bezirk) %>%
  #   distinct(Canton,Bezirk)
  
  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  load("../data/expected_death_inla2020.RData")
  Expected_death_Covid <- expected_deaths
  
  load("../data/Area.RData")
  
  area_data <- Area %>%
    mutate(Year = as.factor(Year)) %>%
    filter(Year == 1890 | Year == 1918 | Year == 2020) 
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) %>%
    ungroup() %>%
    filter(Year == 1890 | Year == 1918 | Year == 2020) %>%
    mutate(Year=as.factor(Year)) %>%
    full_join(area_data) %>%
    mutate(
      # Language = Canton,
      #      Language = as.character(Language),
      #      Language = recode(Language,
      #                        "AG" = "German",
      #                        "AI" = "German",
      #                        "AR" = "German",
      #                        "BE" = "German",
      #                        "BL" = "German",
      #                        "BS" = "German",
      #                        "FR" = "French",
      #                        "GE" = "French",
      #                        "GL" = "German",
      #                        "GR" = "German",
      #                        "IR" = "German",
      #                        "JU" = "French",
      #                        "LU" = "German",
      #                        "NE" = "French",
      #                        "NW" = "German",
      #                        "OW" = "German",
      #                        "SG" = "German",
      #                        "SH" = "German",
      #                        "SO" = "German",
      #                        "SZ" = "German",
      #                        "TG" = "German",
      #                        "TI" = "Italian",
      #                        "UR" = "German",
      #                        "VD" = "French",
      #                        "VS" = "French",
      #                        "ZG" = "German",
      #                        "ZH" = "German"),
           excess_percentage_o = ((death-fit)/fit)*100,
           excess_percentage = round(((death-fit)/fit)*100,2),
           excess_perc_groups =  as.numeric(excess_percentage),
           significant_dummy = ifelse(death > LL & death <UL,0,1),
           significant_dummy = as.factor( significant_dummy )) %>%
    group_by(Year) %>%
    mutate(median_denspop = median(densPop),
           dens_group = ifelse(densPop <median_denspop, "small", "large" )) %>%
    ungroup() %>%
    mutate(dens_group = factor( dens_group, levels = c("small", "large"))) %>%
    filter(Year==Year_Pan)
  
  # summary(gam(excess_percentage ~ s(densPop),data=data_excess))
  # summary(lm(excess_percentage ~ dens_group,data=data_excess))
  summary(rlm(excess_percentage ~ dens_group,data=data_excess))
  
}

