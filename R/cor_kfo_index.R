function_cor_sep <- function(){

load("data/expected_death_inla2020.RData")
  
Expected_death_Covid <- expected_deaths %>%
  filter(Year ==2020) 
  
data_canton <- read.xlsx("data/data_total.xlsx" ) %>%
  distinct(Bezirk, .keep_all = TRUE) %>%
  select(Canton, Bezirk) 


data_kfo <- read.csv("data_raw/Data2020/kof_data_export.csv", sep=",", fileEncoding="UTF-8-BOM") %>%
  gather(., Canton, kfo, 2:28, factor_key=TRUE) %>%
  mutate(Canton = substr(Canton, 19,20 ),
         Canton = toupper(Canton),
         date = as.Date(date)) %>%
  filter(!Canton=="CH") %>%
  filter(date>="2020-07-01" & date<="2020-12-31") %>%
  group_by(Canton)  %>%
  summarise(kfo_mean = mean(kfo))


data_excess <- Expected_death_Covid %>%
  ungroup() %>%
  left_join(data_canton) %>% 
  left_join(data_kfo) %>%
  mutate(Language = Canton,
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
             Language = as.factor(Language),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage),
             death_inc = death/population *100000)


    plot_kfo <- ggplot(data=data_excess) +
      geom_point(aes(x=kfo_mean , y=excess_percentage, shape=Language,col=Language), lwd=lwd_size_points ) +
      geom_smooth(aes(x=kfo_mean , y=excess_percentage),  method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
      facet_wrap(~Year, nrow = 2, scales="free_x") +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17))+
      ggtitle("KOF Stringency Indices")+
      ylab("Relative Excess Mortality")+
      xlab("Mean KOF Stringency Indices (July - December)") +
      theme_bw() +
      theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")
    
# cowplot::save_plot("output/plot_SEP.pdf",plot_SEP,base_height=7,base_width=11)
# cowplot::save_plot("output/plot_SEP_all.pdf",plot_SEP2,base_height=7,base_width=11)
# 
# summary(lm(excess_percentage~SEP_Bezirk*Language, data=data_excess))

# plot_SEP_both <- list( plot_SEP , plot_SEP2)
return( plot_SEP)

}



function_test_kfo <- function(){
  
  load("data/expected_death_inla2020.RData")
  
  Expected_death_Covid <- expected_deaths %>%
    filter(Year ==2020) 
  
  data_canton <- read.xlsx("data/data_total.xlsx" ) %>%
    distinct(Bezirk, .keep_all = TRUE) %>%
    select(Canton, Bezirk) 
  
  
  data_kfo <- read.csv("data_raw/Data2020/kof_data_export.csv", sep=",", fileEncoding="UTF-8-BOM") %>%
    gather(., Canton, kfo, 2:28, factor_key=TRUE) %>%
    mutate(Canton = substr(Canton, 19,20 ),
           Canton = toupper(Canton),
           date = as.Date(date)) %>%
    filter(!Canton=="CH") %>%
    filter(date>="2020-07-01" & date<="2020-12-31") %>%
    group_by(Canton)  %>%
    summarise(kfo_mean = mean(kfo))
  
  
  data_excess <- Expected_death_Covid %>%
    ungroup() %>%
    left_join(data_canton) %>% 
    left_join(data_kfo) %>%
    mutate(Language = Canton,
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
           Language = as.factor(Language),
           excess_percentage_o = ((death-fit)/fit)*100,
           excess_percentage = round(((death-fit)/fit)*100,2),
           excess_perc_groups =  as.numeric(excess_percentage),
           death_inc = death/population *100000)
  
  # summary(gam(excess_percentage ~ s(SEP_Bezirk),data=data_excess))
  summary(rlm(excess_percentage ~ kfo_mean,data=data_excess))
  # summary(rlm(excess_percentage ~ Language,data=data_excess))
  # summary(rlm(excess_percentage ~ SEP_Bezirk+Language,data=data_excess))
  
}

