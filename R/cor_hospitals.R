function_cor_hospitals <- function(){
# 
#     load(paste0("../data/expected_death_inla1890.RData"))
#     Expected_death_Russian <-expected_deaths
#     load(paste0("../data/expected_death_inla1918.RData"))
#     Expected_death_Spanish <- expected_deaths
#     load(paste0("../data/expected_death_inla2020.RData"))
#     Expected_death_Covid <- expected_deaths

# 
#   load("../data/data_total.RData")
#   Canton <- data_total %>%
#     dplyr::select(Canton, Bezirk) %>%
#     distinct(Canton,Bezirk)
#   
  load("../data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("../data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  
  load("../data/Hospitals.RData")
  
  Hospitals_data <- Hospitals %>%
   mutate(Year = as.factor(Year), 
     Year = recode(Year,"1915" = "1918"))
    
    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish ) %>%
      ungroup() %>%
      # left_join(Canton) %>%
      filter(Year == 1890 | Year == 1918) %>%
      mutate(Year=as.factor(Year)) %>%
      full_join(Hospitals_data) %>%
      mutate(excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage),
             significant_dummy = ifelse(death > LL & death <UL,0,1),
             significant_dummy = as.factor( significant_dummy ),
             death_inc = death/population *10000,
             hospitals_inc= hospitals/population *10000)  %>%
      mutate( hosp_group = ifelse( hospitals>0, ">1",  hospitals_inc),
              hosp_group = factor( hosp_group, levels = c("0", ">1"))) %>%
      group_by(Year) %>%
      mutate(excess_norm = normalit( excess_percentage_o)) %>%
      ungroup()
    
    
    plot_Hospitals <- ggplot(data=data_excess,aes(x=Year, y=excess_percentage_o, fill = hosp_group)) +
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
      ggtitle("Number of hospitals")+
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
  
  
  # load(paste0("../data/data_total.RData"))
  # Canton <- data_total %>%
  #   dplyr::select(Canton, Bezirk) %>%
  #   distinct(Canton,Bezirk)
  
  load(paste0("../data/expected_death_inla1890.RData"))
  Expected_death_Russian <-expected_deaths
  load(paste0("../data/expected_death_inla1918.RData"))
  Expected_death_Spanish <- expected_deaths
  # 
  
  load(paste0("../data/Hospitals.RData"))
  
  Hospitals_data <- Hospitals %>%
    mutate(Year = as.factor(Year), 
           Year = recode(Year,"1915" = "1918"))
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish ) %>%
    ungroup() %>%
    filter(Year == 1890 | Year == 1918) %>%
    mutate(Year=as.factor(Year)) %>%
    full_join(Hospitals_data) %>%
    mutate(excess_percentage_o = ((death-fit)/fit)*100,
           excess_percentage = round(((death-fit)/fit)*100,2),
           excess_perc_groups =  as.numeric(excess_percentage),
           significant_dummy = ifelse(death > LL & death <UL,0,1),
           significant_dummy = as.factor( significant_dummy ),
           death_inc = death/population *10000,
           hospitals_inc= hospitals/population *10000)  %>%
    mutate( hosp_group = ifelse( hospitals>0, ">1",  hospitals_inc),
            hosp_group = factor( hosp_group, levels = c("0", ">1"))) %>%
    group_by(Year) %>%
    mutate(excess_norm = normalit( excess_percentage_o)) %>%
    ungroup()%>%
    filter(Year==Year_Pan)
  
  summary(rlm(excess_percentage_o~ hosp_group,data=data_excess))
 
}

