function_cor_pandemic_age <- function(){

  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
 
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths %>%
    filter(Year==1918) %>%
    mutate( excess_percentage_o = ((death-fit)/fit)*100)
  excess_norm <- c(normalit(Expected_death_Spanish$excess_percentage_o))
  Expected_death_Spanish$excess_norm <- excess_norm 
  

  load("../data/prop_older30.RData")
  prop_older30 <- prop_older30 %>%
    filter(Year==1918)
  
  
  data_excess <-  Expected_death_Spanish %>%
      ungroup() %>%
      left_join(Canton) %>%
    left_join(prop_older30) %>%
      mutate(Year=as.factor(Year)) %>%
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
             Language = as.factor(Language)) %>%
    distinct(Bezirk, .keep_all = TRUE)

  
    plot_age_1918 <- ggplot(data= data_excess) +
      geom_point(aes(x= prop_norm, y=excess_norm, shape=Language,col=Language),  lwd=lwd_size_points ) +
      geom_smooth(aes(x= prop_norm, y=excess_norm),  method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17)) +
      ggtitle("Proportion of people 30 years and older")+
      ylab("Normalized Excess Mortality 1918")+
      xlab("Normalized Proportion >=30 years old")+
      theme_bw() +
      theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")
    
# cowplot::save_plot("output/plot_GDP_rel.pdf",plot_GDP_rel,base_height=10,base_width=15)

return(plot_age_1918)

}


function_test_pandemic_age <- function(){
  
  
  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths %>%
    filter(Year==1918) %>%
    mutate( excess_percentage_o = ((death-fit)/fit)*100)
  excess_norm <- c(normalit(Expected_death_Spanish$excess_percentage_o))
  Expected_death_Spanish$excess_norm <- excess_norm 
  
  
  load("../data/prop_older30.RData")
  prop_older30 <- prop_older30 %>%
    filter(Year==1918) %>%
  
  
  data_excess <-  Expected_death_Spanish %>%
    ungroup() %>%
    left_join(Canton) %>%
    left_join(prop_older30) %>%
    mutate(Year=as.factor(Year)) %>%
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
           Language = as.factor(Language))
  
  summary(lm(excess_norm~ prop_norm,data=  data_excess))
  # summary(lm(excess_percentage ~ GDP,data=data_excess))
}

