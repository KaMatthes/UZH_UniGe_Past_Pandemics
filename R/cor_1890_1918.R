function_cor_pandemic <- function(){

  normalit <-function(m){
    (m - min(m))/(max(m)-min(m))
  }
  
  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)

  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <- expected_deaths %>%
    filter(Year==1890) %>%
    mutate( excess_percentage_o = ((death-fit)/fit)*100)
  excess_norm <- c(normalit(Expected_death_Russian$excess_percentage_o))
  Expected_death_Russian$excess_norm <- excess_norm 
  
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths %>%
    filter(Year==1918) %>%
    mutate( excess_percentage_o = ((death-fit)/fit)*100)
  excess_norm <- c(normalit(Expected_death_Spanish$excess_percentage_o))
  Expected_death_Spanish$excess_norm <- excess_norm 


  # load("../data/data_total.RData")
  # Canton <- data_total %>%
  #   dplyr::select(Canton, Bezirk) %>%
  #   distinct(Canton,Bezirk)
  # 
  # load("../data/expected_death_inla1890.RData")
  # Expected_death_Russian <-expected_deaths
  # load("../data/expected_death_inla1918.RData")
  # Expected_death_Spanish <- expected_deaths

  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
      ungroup() %>%
      left_join(Canton) %>%
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
    select(Region,Year, Language,excess_norm) 
  
  data_excess_1890 <- data_excess %>%
    filter(Year==1890) %>%
    select(-Year)%>%
    rename(excess_norm_1890 = excess_norm)
  
  data_excess_1918 <- data_excess %>%
    filter(Year==1918) %>%
    select(-Year) %>%
    rename(excess_norm_1918 = excess_norm) %>%
    full_join(data_excess_1890)
  
  
  plot_1890_1918 <- ggplot(data= data_excess_1918  ) +
      geom_point(aes(x= excess_norm_1890, y=excess_norm_1918, shape=Language,col=Language),  lwd=lwd_size_points ) +
      geom_smooth(aes(x= excess_norm_1890, y=excess_norm_1918),  method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
    
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17)) +
      ggtitle("Relation between 1890 and 1918 pandemics")+
      ylab("Normalized Excess Mortality 1918")+
      xlab("Normalized Excess Mortality 1890")+
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

return( plot_1890_1918)

}


function_test_pandemic <- function(){
  
  # load("data/data_total.RData")
  # Canton <- data_total %>%
  #   dplyr::select(Canton, Bezirk) %>%
  #   distinct(Canton,Bezirk)
  # 
  # load("data/expected_death_inla1890.RData")
  # Expected_death_Russian <-expected_deaths
  # load("data/expected_death_inla1918.RData")
  # Expected_death_Spanish <- expected_deaths
  # 
  
  
  load("../data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <- expected_deaths %>%
    filter(Year==1890) %>%
    mutate( excess_percentage_o = ((death-fit)/fit)*100)
  excess_norm <- c(normalit(Expected_death_Russian$excess_percentage_o))
  Expected_death_Russian$excess_norm <- excess_norm 
  
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths %>%
    filter(Year==1918) %>%
    mutate( excess_percentage_o = ((death-fit)/fit)*100)
  excess_norm <- c(normalit(Expected_death_Spanish$excess_percentage_o))
  Expected_death_Spanish$excess_norm <- excess_norm 

  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
    ungroup() %>%
    left_join(Canton) %>%
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
    select(Region,Year, Language,excess_norm) %>%
    distinct(Bezirk, .keep_all = TRUE)
  
  
  data_excess_1890 <- data_excess %>%
    filter(Year==1890) %>%
    select(-Year)%>%
    rename(excess_norm_1890 = excess_norm)
  
  data_excess_1918 <- data_excess %>%
    filter(Year==1918) %>%
    select(-Year) %>%
    rename(excess_norm_1918 = excess_norm) %>%
    full_join(data_excess_1890)
  
  summary(rlm(excess_norm_1918 ~ excess_norm_1890,data=  data_excess_1918))
  # summary(lm(excess_percentage ~ GDP,data=data_excess))
}

