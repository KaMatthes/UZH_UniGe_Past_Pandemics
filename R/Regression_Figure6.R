
  
  load("data/expected_death_inla1890.RData")
  Expected_death_Russian <-expected_deaths
  load("data/expected_death_inla1918.RData")
  Expected_death_Spanish <- expected_deaths
  load("data/expected_death_inla2020.RData")
  Expected_death_Covid <- expected_deaths
  
  load("data/data_total.RData")
  Canton <- data_total %>%
    dplyr::select(Canton, Bezirk) %>%
    distinct(Canton,Bezirk)
  
  
  load("data/prop_older40.RData")
  prop_older40 <- prop_older40 %>%
    filter(Year==1918) %>%
    rename(prop40 = prop,
           prop40_norm = prop_norm)
  
  load("data/GDP_rel2.RData")
  GDP_data <- GDP %>%
    mutate(Year = recode(Year,
                         "1888" ="1890",
                         "1910" = "1918",
                         "2008" = "2020"),
           Year = as.character(Year),
           Year= as.numeric(Year)) %>%
    filter(disp=="rel") %>%
    select(-disp, -MapName) %>%
    mutate(gdp_norm = normalit(GDP))
  
  load("data/Swiss_SEP.RData")
  Swiss_SEP <- Swiss_SEP %>%
    select(SEP_Bezirk, Bezirk) %>%
    mutate(Year =2020,
           sep_norm = normalit(SEP_Bezirk))
  
  
  load("data/Hospitals.RData")
  Hospitals_data <- Hospitals %>%
    mutate(Year = as.character(Year), 
           Year = recode(Year,
                         "1915" = "1918"),
           Year= as.numeric(Year)) %>%
    select(-MapName)
  
  load("data/prop_school_kids.RData")
  
  prop_school_kids <- prop_school_kids %>%
    rename(prop_kids=prop,
           prop_kids_norm=prop_norm)
  
  
  load("data/prop_70_year.RData")
  
  prop_70_year <- prop_70_year %>%
    rename(prop_70=prop,
           prop_70_norm=prop_norm)
  
  
  load("data/ratio_sex.RData")
  ratio_sex <- ratio_sex %>%
    select(-MapName) %>%
    filter(Year == 1890 | Year == 1918 | Year == 2020)
    
  load("data/child_mortality.RData")
  child_mortality <- child_mortality%>%
    mutate(Year = as.factor(Year)) %>%
    select(Year, Bezirk, prop_child_death, prop_norm, death) %>%
    filter(Year=="1889"  | Year =="1917") %>%
    mutate(Year = as.factor(Year), 
           Year = recode(Year,"1889" = "1890",
                         "1917" = "1918"),
           Year = as.character(Year),
           Year= as.numeric(Year)) %>%
    rename(prop_child_death_norm=prop_norm) %>%
    select(-death)
  
  load("data/Tbc.RData")
  Tbc_data <- Tbc %>%
    mutate(Year = as.factor(Year),
           Year = recode(Year,"1915" = "1918"),
           Year = as.character(Year),
           Year= as.numeric(Year)) %>%
    select(-MapName)
  
  load("data/Area.RData")
  area_data <- Area %>%
    mutate(Year = as.factor(Year),
           Year = as.character(Year),
           Year= as.numeric(Year)) %>%
    filter(Year == 1890 | Year == 1918 | Year == 2020)  %>%
    select(-MapName)
  
  
  load("data/Urbanity.RData")
  Urbanity1 <- Urbanity %>%
    mutate(Year=1890)
  Urbanity2 <- Urbanity %>%
    mutate(Year=1918)
  Urbanity3 <- Urbanity %>%
    mutate(Year=2020)
  Urbanity <- rbind(Urbanity1,Urbanity2,Urbanity3) %>%
    select(-MapName)
  
  load("data/Stations.RData")
  Stations1 <- Stations %>%
    mutate(Year=1890)
  Stations2 <- Stations %>%
    mutate(Year=1918)
  Stations <- rbind(Stations1, Stations2)
  
  
  # data_kfo <- read.csv("data_raw/Data2020/kof_data_export.csv", sep=",", fileEncoding="UTF-8-BOM") %>%
  #   gather(., Canton, kfo, 2:28, factor_key=TRUE) %>%
  #   mutate(Canton = substr(Canton, 19,20 ),
  #          Canton = toupper(Canton),
  #          date = as.Date(date)) %>%
  #   filter(!Canton=="CH") %>%
  #   filter(date>="2020-07-01" & date<="2020-12-31") %>%
  #   group_by(Canton)  %>%
  #   summarize(kfo_mean = mean(kfo)) %>%
  #   mutate(Year=2020)

    
  
data_all <- rbind(Expected_death_Russian, Expected_death_Spanish,  Expected_death_Covid ) %>%
  ungroup() %>%
  filter(Year == 1890 | Year == 1918 | Year == 2020)  %>%
  left_join(Canton) %>%
  left_join(prop_older40) %>%
  left_join(GDP_data) %>%
  left_join(Swiss_SEP) %>%
  left_join(Hospitals_data ) %>%
  left_join( prop_school_kids ) %>%
  left_join( prop_70_year) %>%
  left_join( ratio_sex) %>%
  left_join(child_mortality) %>%
  left_join(Tbc_data) %>%
  left_join(area_data) %>%
  left_join(Urbanity) %>%
  left_join(Stations) %>%
  left_join(data_kfo) %>%
  filter(!is.na(area_Bezirk)) %>%
  mutate(station_area = n_stat/population*1000,
         hospitals_inc= hospitals/population *1000,
         hosp_group = ifelse( hospitals>0, ">1",  hospitals_inc),
         hosp_group = factor( hosp_group, levels = c("0", ">1")),
         tbc_inc= Tbc_Bezirk/pop_sum *1000,
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage = round(((death-fit)/fit)*100,2),
         prop_70 = prop_70*100,
         prop_kids = prop_kids*100) %>%
  group_by(Year) %>%
  mutate(median_denspop = median(densPop),
         dens_group = ifelse(densPop <median_denspop, "small", "large" ),
         median_train = median(station_area),
         train_group = ifelse(station_area <median_train, "less", "more" )) %>%
  ungroup() %>%
  mutate(city_bezirk = as.factor(city_bezirk),
         dens_group = as.factor(dens_group),
         hosp_group = as.factor(hosp_group),
         dens_group = factor( dens_group, levels=c("small","large")),
         train_group = factor(train_group, levels=c("more","less")))
  
  
  data_reg <- data_all %>%
    filter(Year==1890) %>%
    select(excess_percentage_o, GDP,gdp_norm, hosp_group, prop_kids,prop_70,prop_70_norm,   prop_men_norm,prop_child_death,tbc_inc, dens_group, 
           city_bezirk,train_group,densPop,station_area, n_stat) %>%
    filter(complete.cases(.))

  
  Mod1 <- coef(summary(rlm(excess_percentage_o ~ gdp_norm, data=data_reg)))
  Mod2 <- coef(summary(rlm(excess_percentage_o ~  hosp_group,data=data_reg)))
  Mod3 <- coef(summary(rlm(excess_percentage_o ~  prop_kids,data=data_reg)))
  Mod4 <- coef(summary(rlm(excess_percentage_o ~ prop_70,data=data_reg)))
  Mod5 <- coef(summary(rlm(excess_percentage_o ~ prop_men_norm,data=data_reg)))
  Mod6 <- coef(summary(rlm(excess_percentage_o ~   prop_child_death,data=data_reg)))
  Mod7 <- coef(summary(rlm(excess_percentage_o ~   tbc_inc,data=data_reg)))
  Mod8 <- coef(summary(rlm(excess_percentage_o ~   dens_group,data=data_reg)))
  Mod9 <- coef(summary(rlm(excess_percentage_o ~   city_bezirk,data=data_reg)))
  Mod10 <- coef(summary(rlm(excess_percentage_o ~  station_area,data=data_reg)))
  
  res_uni1 <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6, Mod7, Mod8, Mod9,Mod10) %>%
    data.frame() %>%
    mutate(Cofactor=row.names(.)) %>%
    filter( Cofactor=="gdp_norm" | Cofactor=="hosp_group.1" |   Cofactor=="prop_kids"|  Cofactor=="prop_70"| Cofactor=="prop_men_norm"|
              Cofactor=="prop_child_death" | Cofactor=="tbc_inc" |   Cofactor=="dens_grouplarge"| 
              Cofactor=="city_bezirk1" |   Cofactor=="station_area") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Univariate = paste0(est," (",Cl,"-",Cu, ")"),
           Cofactor=recode(Cofactor, 
                           "gdp_norm" = "GDP / SEP",
                           "hosp_group.1" = "At least 1 hospital (Ref: no hospitals)",
                           "prop_kids" = "Proportion of 5-14 year-olds",
                           "prop_70" = "Proportion of >=70 year-olds",
                           "prop_men_norm" = "Proportion of men",
                           "prop_child_death" = "Proportion of child mortality",
                           "tbc_inc" = "Tuberculosis mortality",
                           "dens_grouplarge" = "Large population density (Ref: Small)",
                           "city_bezirk1" = "Urbanicity (Ref: Rural)",
                           "station_area" = "Railway stations per inhabitant"),
           Cofactor=factor(Cofactor, levels=c("GDP / SEP", "Urbanicity (Ref: Rural)","Large population density (Ref: Small)", "Proportion of 5-14 year-olds",
                                              "Proportion of >=70 year-olds","Proportion of men","Proportion of child mortality",
                                              "At least 1 hospital (Ref: no hospitals)","Railway stations per inhabitant","Tuberculosis mortality"))) %>%
    mutate(Year = "1890")
  
  # OR_plot <- ggplot(res_uni, aes(x=forcats::fct_rev(Cofactor),y=est),position=pd) + 
  #   geom_hline(yintercept=0, colour="grey", lwd=lwdline) + 
  #   geom_pointrange(aes(ymin=Cl, ymax=Cu),lwd=lwd_size,position=pd,fatten=fatten_size)+
  #   # ylim(c(0,2.5))+
  #   labs(x="",y="OR (95% CI)") +
  #   # ggtitle("1890")+
  #   # scale_color_manual(" ",
  #   #                    breaks=c("age_group>=40","age_group<40"),
  #   #                    labels=c("Age >=40 (Ref)","Age <40"),
  #   #                    values = c(mypalette7[3],mypalette7[2]))+
  #   theme_bw(base_family = "Georgia")+
  #   theme(
  #         aspect.ratio=1,
  #         strip.text = element_text(color="black",size= strip_text),
  #         axis.text=element_text(color="black",size= size_axis),
  #         axis.title=element_text(size= size_axis_title),
  #         plot.title = element_text(size=size_plot_title),
  #         legend.text=element_text(size=size_legend_text),
  #         legend.position = "bottom") +
  #   coord_flip(ylim=c(-30, 20))
  # 

  
  # return( OR_plot)
#   cowplot::save_plot(paste0("output/OR_plot_", Year_Pan, ".pdf"), OR_plot,base_height=15,base_width=20)
#   
# }
  
  
  data_reg1890 <- data_all %>%
    filter(Year==1918) %>%
    select(excess_1890 = excess_percentage_o, Bezirk)

  data_reg <- data_all %>%
    filter(Year==1918)  %>%
    left_join(data_reg1890) %>%
    select(excess_percentage_o, excess_1890,GDP,gdp_norm, hosp_group, prop_kids,prop_70, prop_men_norm,
           prop_child_death, tbc_inc, dens_group, city_bezirk,train_group,densPop,station_area) %>%
    filter(complete.cases(.))
  
  load(paste0("data/expected_death_inla20_29_1918.RData"))
  Expected_death_20_29 <- expected_deaths
  load("data/birth.RData")
  birth_20_29 <- birth %>%
    filter(Year==1890 | Year==1889) %>%
    select(Bezirk, birth_inc) %>%
    group_by(Bezirk) %>%
    summarize(mean_birth = mean(birth_inc)) %>%
    left_join(Expected_death_20_29) %>%
    filter(Year==1918)%>%
    mutate(excess_percentage_o = round(((death-fit)/fit)*100,2),
           mean_birth=mean_birth*100)
  
  load(paste0("data/expected_death_inla30_39_1918.RData"))
  Expected_death_30_39 <- expected_deaths
  load("data/birth.RData")
  birth_30_39 <-birth %>%
    filter(Year<1889) %>%
    select(Bezirk, birth_inc) %>%
    group_by(Bezirk) %>%
    summarize(mean_birth = mean(birth_inc)) %>%
    left_join( Expected_death_30_39 ) %>%
    filter(Year==1918)%>%
    mutate(excess_percentage_o = round(((death-fit)/fit)*100,2),
           mean_birth=mean_birth*100)

  
  Mod1 <- coef(summary(rlm(excess_percentage_o ~ gdp_norm, data=data_reg)))
  Mod2 <- coef(summary(rlm(excess_percentage_o ~  hosp_group,data=data_reg)))
  Mod3 <- coef(summary(rlm(excess_percentage_o ~  prop_kids,data=data_reg)))
  Mod4 <- coef(summary(rlm(excess_percentage_o ~  prop_70,data=data_reg)))
  Mod5 <- coef(summary(rlm(excess_percentage_o ~ prop_men_norm,data=data_reg)))
  Mod6 <- coef(summary(rlm(excess_percentage_o ~   prop_child_death,data=data_reg)))
  Mod7 <- coef(summary(rlm(excess_percentage_o ~   tbc_inc,data=data_reg)))
  Mod8 <- coef(summary(rlm(excess_percentage_o ~   dens_group,data=data_reg)))
  Mod9 <- coef(summary(rlm(excess_percentage_o ~   city_bezirk,data=data_reg)))
  Mod10 <- coef(summary(rlm(excess_percentage_o ~  station_area,data=data_reg)))
  # Mod9 <- coef(summary(rlm(excess_percentage_o ~ prop40_norm, data=data_reg)))
  # Mod10 <- coef(summary(rlm(excess_percentage_o ~ excess_1890, data=data_reg)))
  # Mod11 <- coef(summary(rlm(excess_percentage_o ~mean_birth, data=  birth_20_29)))
  # Mod11 <- coef(summary(rlm(excess_percentage_o ~mean_birth, data=  data_reg)))
  # Mod12 <- coef(summary(rlm(excess_percentage_o ~mean_birth, data=  birth_30_39)))
  
  res_uni2 <-  rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6, Mod7, Mod8, Mod9, Mod10) %>%
    data.frame() %>%
    mutate(Cofactor=row.names(.)) %>%
    filter( Cofactor=="gdp_norm" | Cofactor=="hosp_group.1" |   Cofactor=="prop_kids"| Cofactor=="prop_70"| Cofactor=="prop_men_norm"| 
              Cofactor=="prop_child_death" | Cofactor=="tbc_inc" |   Cofactor=="dens_grouplarge"| 
              Cofactor=="city_bezirk1" |   Cofactor=="station_area") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Univariate = paste0(est," (",Cl,"-",Cu, ")"),
           Cofactor=recode(Cofactor, 
                           "gdp_norm" = "GDP / SEP",
                           "hosp_group.1" = "At least 1 hospital (Ref: no hospitals)",
                           "prop_kids" = "Proportion of 5-14 year-olds",
                           "prop_70" = "Proportion of >=70 year-olds",
                           "prop_men_norm" = "Proportion of men",
                           "prop_child_death" = "Proportion of child mortality",
                           "tbc_inc" = "Tuberculosis mortality",
                           "dens_grouplarge" = "Large population density (Ref: Small)",
                           "city_bezirk1" = "Urbanicity (Ref: Rural)",
                           "station_area" = "Railway stations per inhabitant",
                           "excess_1890" = "Excess mortality 1890"),
           Cofactor=factor(Cofactor, levels=c("GDP" = "GDP / SEP", "Urbanicity (Ref: Rural)","Large population density (Ref: Small)",
                                              "Proportion of 5-14 year-olds","Proportion of >=70 year-olds","Proportion of men",
                                              "Proportion of child mortality",
                                              "At least 1 hospital (Ref: no hospitals)","Railway stations per inhabitant",
                                              "Tuberculosis mortality"))) %>%
    mutate(Year = "1918")
  
# 
#   OR_plot <- ggplot(res_uni, aes(x=forcats::fct_rev(Cofactor),y=est),position=pd) + 
#       geom_hline(yintercept=0, colour="grey", lwd=lwdline) + 
#       geom_pointrange(aes(ymin=Cl, ymax=Cu),lwd=lwd_size,position=pd,fatten=fatten_size)+
#       # ylim(c(0,2.5))+
#       labs(x="",y="OR (95% CI)") +
#       # ggtitle("1918")+
#       # scale_color_manual(" ",
#       #                    breaks=c("age_group>=40","age_group<40"),
#       #                    labels=c("Age >=40 (Ref)","Age <40"),
#       #                    values = c(mypalette7[3],mypalette7[2]))+
#     theme_bw(base_family = "Georgia")+
#       theme(
#             aspect.ratio=1,
#             strip.text = element_text(color="black",size= strip_text),
#             axis.text=element_text(color="black",size= size_axis),
#             axis.title=element_text(size= size_axis_title),
#             plot.title = element_text(size=size_plot_title),
#             legend.text=element_text(size=size_legend_text),
#             legend.position = "bottom") +
#       coord_flip(ylim=c(-45, 20))
#      
#     
#     # return( OR_plot)
#     
#     cowplot::save_plot(paste0("output/OR_plot_", Year_Pan, ".pdf"), OR_plot,base_height=15,base_width=20)
#     
# 
# }

  
  data_reg <- data_all %>%
    filter(Year==2020) %>%
    select(excess_percentage_o, sep_norm, prop_kids,prop_70, prop_men_norm, dens_group, city_bezirk) %>%
    filter(complete.cases(.))
  
  Mod1 <- coef(summary(rlm(excess_percentage_o ~ sep_norm, data=data_reg)))
  Mod2 <- coef(summary(rlm(excess_percentage_o ~  prop_kids,data=data_reg)))
  Mod3 <- coef(summary(rlm(excess_percentage_o ~  prop_70,data=data_reg)))
  Mod4 <- coef(summary(rlm(excess_percentage_o ~  prop_men_norm,data=data_reg)))
  Mod5 <- coef(summary(rlm(excess_percentage_o ~   dens_group,data=data_reg)))
  Mod6 <- coef(summary(rlm(excess_percentage_o ~   city_bezirk,data=data_reg)))
  # Mod5 <- coef(summary(rlm(excess_percentage_o ~   kfo_mean,data=data_reg)))
  
  res_uni3 <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6) %>%
    data.frame() %>%
    mutate(Cofactor=row.names(.)) %>%
    filter( Cofactor=="sep_norm" | Cofactor=="prop_kids" | Cofactor=="prop_70"| Cofactor=="prop_men_norm" |    Cofactor=="dens_grouplarge"| 
              Cofactor=="city_bezirk1") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Univariate = paste0(est," (",Cl,"-",Cu, ")"),
           Cofactor=recode(Cofactor, 
                           "sep_norm" = "GDP / SEP",
                           "prop_kids" = "Proportion of 5-14 year-olds",
                           "prop_70" = "Proportion of >=70 year-olds",
                           "prop_men_norm" = "Proportion of men",
                           "dens_grouplarge" = "Large population density (Ref: Small)",
                           "city_bezirk1" = "Urbanicity (Ref: Rural)"),
           Cofactor=factor(Cofactor, levels=c("GDP" = "GDP / SEP", "Urbanicity (Ref: Rural)",
                                              "Large population density (Ref: Small)", "Proportion of 5-14 year-olds",
                                              "Proportion of >=70 year-olds","Proportion of men"))) %>%
    mutate(Year = "2020")
  
  data_res <- res_uni1 %>%
    rbind(res_uni2) %>%
    rbind(res_uni3) %>%
    select( Year, Univariate, Cofactor,est, Cl, Cu)
  
  # write.xlsx( data_res ,"output/data_res2.xlsx", row.names=FALSE, overwrite = TRUE)
  
  
  OR_plot <- ggplot(data_res, aes(x=forcats::fct_rev(Cofactor),ymin=Cl, ymax=Cu,y=est, col=Year),position=pd) + 
    geom_hline(yintercept=0, colour="grey", lwd=lwdline) + 
    geom_pointrange(lwd=3,position=pd, fatten=12) +
    xlab("")+
    ylab("Regression coefficients (percentage change) and 95% CI") +
    scale_color_manual("Year:",
                      values=cbp1) +
    # scale_y_continuous(trans = "log10") + 
    # guides(linetype = guide_legend(override.aes = list(size = 50)))+
    # ggtitle("2020")+
    # scale_color_manual(" ",
    #                    breaks=c("age_group>=40","age_group<40"),
    #                    labels=c("Age >=40 (Ref)","Age <40"),
    #                    values = c(mypalette7[3],mypalette7[2]))+
    # theme_bw(base_family = "Georgia")+
    theme_bw()+
    theme(
          aspect.ratio=1,
          strip.text = element_text(color="black",size= strip_text),
          axis.text=element_text(color="black",size= 30),
          axis.text.x=element_text(color="black",size= 30),
          axis.title=element_text(size= 30),
          plot.title = element_text(size=size_plot_title),
          legend.text=element_text(size=35),
          legend.title=element_text(size=35),
          legend.position = "bottom") +
    coord_flip(ylim=c(-30, 30))
  
  
  # return( OR_plot)
  
  cowplot::save_plot(paste0("output/Figure6.pdf"), OR_plot,base_height=15,base_width=20)
  
  
  