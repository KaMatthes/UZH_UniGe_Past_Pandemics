function_regression <- function(Year_Pan) {
  
  
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
  
  
  load("data/prop_older30.RData")
  prop_older30 <- prop_older30 %>%
    filter(Year==1918) %>%
    rename(prop30 = prop,
           prop30_norm = prop_norm)
  
  load("data/GDP_rel2.RData")
  GDP_data <- GDP %>%
    mutate(Year = recode(Year,
                         "1888" ="1890",
                         "1910" = "1918",
                         "2008" = "2020"),
           Year = as.character(Year),
           Year= as.numeric(Year)) %>%
    filter(disp=="rel") %>%
    select(-disp, -MapName)
  
  load("data/Swiss_SEP.RData")
  Swiss_SEP <- Swiss_SEP %>%
    select(SEP_Bezirk, Bezirk) %>%
    mutate(Year =2020)
  
  
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
  
  
  data_kfo <- read.csv("data_raw/Data2020/kof_data_export.csv", sep=",", fileEncoding="UTF-8-BOM") %>%
    gather(., Canton, kfo, 2:28, factor_key=TRUE) %>%
    mutate(Canton = substr(Canton, 19,20 ),
           Canton = toupper(Canton),
           date = as.Date(date)) %>%
    filter(!Canton=="CH") %>%
    filter(date>="2020-07-01" & date<="2020-12-31") %>%
    group_by(Canton)  %>%
    summarise(kfo_mean = mean(kfo)) %>%
    mutate(Year=2020)
  
  
data_all <- rbind(Expected_death_Russian, Expected_death_Spanish,  Expected_death_Covid ) %>%
  ungroup() %>%
  filter(Year == 1890 | Year == 1918 | Year == 2020)  %>%
  left_join(Canton) %>%
  left_join(prop_older30) %>%
  left_join(GDP_data) %>%
  left_join(Swiss_SEP) %>%
  left_join(Hospitals_data ) %>%
  left_join( prop_school_kids ) %>%
  left_join(child_mortality) %>%
  left_join(Tbc_data) %>%
  left_join(area_data) %>%
  left_join(Urbanity) %>%
  left_join(Stations) %>%
  left_join(data_kfo) %>%
  filter(!is.na(area_Bezirk)) %>%
  mutate(station_area = n_stat/area_Bezirk,
         hospitals_inc= hospitals/population *1000,
         hosp_group = ifelse( hospitals>0, ">1",  hospitals_inc),
         hosp_group = factor( hosp_group, levels = c("0", ">1")),
         tbc_inc= Tbc_Bezirk/pop_sum *1000,
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage = round(((death-fit)/fit)*100,2)) %>%
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
  

if(Year_Pan==1890) {
  
  data_reg <- data_all %>%
    filter(Year==Year_Pan) %>%
    select(excess_percentage, GDP, hosp_group, prop_kids_norm, prop_child_death_norm, tbc_inc, dens_group, city_bezirk,train_group,densPop,station_area) %>%
    filter(complete.cases(.))

  
  Mod1 <- coef(summary(rlm(excess_percentage ~ GDP, data=data_reg)))
  Mod2 <- coef(summary(rlm(excess_percentage ~  hosp_group,data=data_reg)))
  Mod3 <- coef(summary(rlm(excess_percentage ~  prop_kids_norm,data=data_reg)))
  Mod4 <- coef(summary(rlm(excess_percentage ~   prop_child_death_norm,data=data_reg)))
  Mod5 <- coef(summary(rlm(excess_percentage ~   tbc_inc,data=data_reg)))
  Mod6 <- coef(summary(rlm(excess_percentage ~   dens_group,data=data_reg)))
  Mod7 <- coef(summary(rlm(excess_percentage ~   city_bezirk,data=data_reg)))
  Mod8 <- coef(summary(rlm(excess_percentage ~  station_area,data=data_reg)))
  
  res_uni <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6, Mod7, Mod8) %>%
    data.frame() %>%
    mutate(Cofactor=row.names(.)) %>%
    filter( Cofactor=="GDP" | Cofactor=="hosp_group.1" |   Cofactor=="prop_kids_norm"| 
              Cofactor=="prop_child_death_norm" | Cofactor=="tbc_inc" |   Cofactor=="dens_grouplarge"| 
              Cofactor=="city_bezirk1" |   Cofactor=="station_area") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Univariate = paste0(est," (",Cl,"-",Cu, ")"),
           Cofactor=recode(Cofactor, 
                           "hosp_group.1" = "hosp_group>1")) %>%
    select(Cofactor, Univariate)
  
  
  res_mul <- coef(summary(rlm(excess_percentage ~ GDP + hosp_group +  prop_kids_norm+ prop_child_death_norm+tbc_inc+dens_group+station_area
                              ,data=   data_reg))) %>%
    data.frame(.) %>%
    mutate( Cofactor = row.names(.)) %>%
    filter(!Cofactor == "(Intercept)") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Multivariate = paste0(est," (",Cl,"-",Cu, ")"),
           Cofactor=recode(Cofactor, 
                            "hosp_group.1" = "hosp_group>1")) %>%
    select(Cofactor,  Multivariate) %>%
    full_join(res_uni) %>%
    select(Cofactor, Univariate, Multivariate)
  
  return(res_mul)

}


else if(Year_Pan==1918) {
  
  data_reg1890 <- data_all %>%
    filter(Year==1890) %>%
    select(excess_1890 = excess_percentage, Bezirk)
  
  data_reg <- data_all %>%
    filter(Year==Year_Pan)  %>%
    left_join(data_reg1890)%>%
    select(excess_percentage,prop30_norm, excess_1890,GDP, hosp_group, prop_kids_norm, prop_child_death_norm, tbc_inc, dens_group, city_bezirk,train_group,densPop,station_area) %>%
  filter(complete.cases(.))
  
  Mod1 <- coef(summary(rlm(excess_percentage ~ GDP, data=data_reg)))
  Mod2 <- coef(summary(rlm(excess_percentage ~  hosp_group,data=data_reg)))
  Mod3 <- coef(summary(rlm(excess_percentage ~  prop_kids_norm,data=data_reg)))
  Mod4 <- coef(summary(rlm(excess_percentage ~   prop_child_death_norm,data=data_reg)))
  Mod5 <- coef(summary(rlm(excess_percentage ~   tbc_inc,data=data_reg)))
  Mod6 <- coef(summary(rlm(excess_percentage ~   dens_group,data=data_reg)))
  Mod7 <- coef(summary(rlm(excess_percentage ~   city_bezirk,data=data_reg)))
  Mod8 <- coef(summary(rlm(excess_percentage ~  station_area,data=data_reg)))
  Mod9 <- coef(summary(rlm(excess_percentage ~ prop30_norm, data=data_reg)))
  Mod10 <- coef(summary(rlm(excess_percentage ~ excess_1890, data=data_reg)))
  
  res_uni <-  rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6, Mod7, Mod8, Mod9, Mod10) %>%
    data.frame() %>%
    mutate(Cofactor=row.names(.)) %>%
    filter( Cofactor=="GDP" | Cofactor=="hosp_group.1" |   Cofactor=="prop_kids_norm"| 
              Cofactor=="prop_child_death_norm" | Cofactor=="tbc_inc" |   Cofactor=="dens_grouplarge"| 
              Cofactor=="city_bezirk1" |   Cofactor=="station_area"  |   Cofactor=="prop30_norm" |   Cofactor=="excess_1890") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Univariate = paste0(est," (",Cl,"-",Cu, ")"),
           Cofactor=recode(Cofactor, 
                           "hosp_group.1" = "hosp_group>1")) %>%
    select(Cofactor, Univariate)
  
  res_mul <- coef(summary(rlm(excess_percentage ~ excess_1890 + GDP+prop30_norm+ hosp_group +  prop_kids_norm+ prop_child_death_norm+tbc_inc+station_area+dens_group,
             data=   data_reg)))  %>%
    data.frame(.) %>%
    mutate( Cofactor = row.names(.)) %>%
    filter(!Cofactor == "(Intercept)") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Multivariate = paste0(est," (",Cl,"-",Cu, ")"),
           Cofactor=recode(Cofactor, 
                           "hosp_group.1" = "hosp_group>1")) %>%
    select(Cofactor,  Multivariate) %>%
    full_join(res_uni) %>%
    select(Cofactor, Univariate, Multivariate)
  
  return(results)


}

else if(Year_Pan==2020) {
  
  data_reg <- data_all %>%
    filter(Year==Year_Pan) %>%
    select(excess_percentage, SEP_Bezirk, prop_kids_norm, dens_group, city_bezirk,kfo_mean) %>%
    filter(complete.cases(.))
  
  Mod1 <- coef(summary(rlm(excess_percentage ~ SEP_Bezirk, data=data_reg)))
  Mod2 <- coef(summary(rlm(excess_percentage ~  prop_kids_norm,data=data_reg)))
  Mod3 <- coef(summary(rlm(excess_percentage ~   dens_group,data=data_reg)))
  Mod4 <- coef(summary(rlm(excess_percentage ~   city_bezirk,data=data_reg)))
  Mod5 <- coef(summary(rlm(excess_percentage ~   kfo_mean,data=data_reg)))
  
  res_uni <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5) %>%
    data.frame() %>%
    mutate(Cofactor=row.names(.)) %>%
    filter( Cofactor=="SEP_Bezirk" | Cofactor=="prop_kids_norm"|    Cofactor=="dens_grouplarge"| 
              Cofactor=="city_bezirk1" | Cofactor=="kfo_mean") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Univariate = paste0(est," (",Cl,"-",Cu, ")")) %>%
    select(Cofactor, Univariate)
  
  res_mul <- coef(summary(rlm(excess_percentage ~ SEP_Bezirk+dens_group +  prop_kids_norm + kfo_mean
                             ,data=    data_reg))) %>%
    data.frame(.) %>%
    mutate( Cofactor = row.names(.)) %>%
    filter(!Cofactor == "(Intercept)") %>%
    mutate(est= round(Value,2),
           Cl = round(Value - 1.96* Std..Error,2),
           Cu = round(Value + 1.96* Std..Error,2),
           Multivariate = paste0(est," (",Cl,"-",Cu, ")")) %>%
    select(Cofactor,  Multivariate) %>%
    full_join(res_uni) %>%
    select(Cofactor, Univariate, Multivariate)
  
  return(results)
}

}

