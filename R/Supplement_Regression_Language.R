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


load("data/GDP_rel2.RData")
GDP_data <- GDP %>%
  mutate(Year = recode(Year,
                       "1888" ="1890",
                       "1910" = "1918",
                       "2008" = "2020"),
         Year = as.character(Year),
         Year= as.numeric(Year)) %>%
  filter(disp=="rel") %>%
  mutate(sep_z = normalit(GDP)) %>%
  select(Year, Bezirk, sep_z) 

load("data/Swiss_SEP.RData")
Swiss_SEP <- Swiss_SEP %>%
  select(SEP_Bezirk, Bezirk) %>%
  mutate(Year =2020) %>%
  mutate(sep_z = normalit(SEP_Bezirk)) %>%
  select(Year, Bezirk, sep_z) 

SEP_normalised <- rbind(GDP_data, Swiss_SEP)

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


load("data/ratio_sex.RData")
ratio_sex <- ratio_sex %>%
  select(-MapName) %>%
  filter(Year == 1890 | Year == 1918 | Year == 2020)

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


data_all <- rbind(Expected_death_Russian, Expected_death_Spanish,  Expected_death_Covid ) %>%
  ungroup() %>%
  filter(Year == 1890 | Year == 1918 | Year == 2020)  %>%
  left_join(Canton) %>%
  left_join(SEP_normalised) %>%
  left_join(Hospitals_data ) %>%
  left_join( prop_school_kids ) %>%
  left_join( prop_70_year) %>%
  left_join(  ratio_sex) %>%
  left_join(child_mortality) %>%
  left_join(Tbc_data) %>%
  left_join(area_data) %>%
  left_join(Urbanity) %>%
  left_join(Stations) %>%
  filter(!is.na(area_Bezirk)) %>%
  mutate(station_area = n_stat/densPop,
         hospitals_inc= hospitals/population *1000,
         hosp_group = ifelse( hospitals>0, ">=1",  hospitals_inc),
         hosp_group = factor( hosp_group, levels = c("0", ">=1")),
         tbc_inc= Tbc_Bezirk/pop_sum *1000,
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage = round(((death-fit)/fit)*100,2)) %>%
  group_by(Year) %>%
  mutate(median_denspop = median(densPop),
         dens_group = ifelse(densPop <median_denspop, "small", "large" ),
         median_train = median(station_area)) %>%
  ungroup() %>%
  mutate(Year = as.factor(Year),
         city_bezirk = as.factor(city_bezirk),
         dens_group = as.factor(dens_group),
         hosp_group = as.factor(hosp_group),
         dens_group = factor( dens_group, levels=c("small","large")),
         Language = Canton,
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
         Language = factor( Language, levels = c("German", "French", "Italian")),
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage = round(((death-fit)/fit)*100,2),
         excess_perc_groups =  as.numeric(excess_percentage),
         significant_dummy = ifelse(death > LL & death <UL,0,1),
         significant_dummy = as.factor( significant_dummy ),
         death_inc = death/population *100000)


data_res <- data_all %>%
  filter(Year==1890)

Mod_1890_1 <- coef(summary(rlm(excess_percentage_o ~ Language, data=data_res))) %>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))


         
Mod_1890_2 <- coef(summary(rlm(excess_percentage_o ~  hosp_group + Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1890_3 <- coef(summary(rlm(excess_percentage_o ~  prop_kids + Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1890_4 <- coef(summary(rlm(excess_percentage_o ~  prop_70 + Language ,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1890_5 <- coef(summary(rlm(excess_percentage_o ~   prop_child_death + Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1890_6 <- coef(summary(rlm(excess_percentage_o ~   tbc_inc+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1890_7 <- coef(summary(rlm(excess_percentage_o ~   dens_group+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1890_8 <- coef(summary(rlm(excess_percentage_o ~   city_bezirk+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1890_9 <- coef(summary(rlm(excess_percentage_o ~  station_area+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))


Mod_1890_10 <- coef(summary(rlm(excess_percentage_o ~  prop_men + Language ,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))


data_res <- data_all %>%
  filter(Year==1918)

Mod_1918_1 <- coef(summary(rlm(excess_percentage_o ~ Language, data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_2 <- coef(summary(rlm(excess_percentage_o ~  hosp_group + Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_3 <- coef(summary(rlm(excess_percentage_o ~  prop_kids + Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_4 <- coef(summary(rlm(excess_percentage_o ~  prop_70 + Language ,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_5 <- coef(summary(rlm(excess_percentage_o ~   prop_child_death + Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_6 <- coef(summary(rlm(excess_percentage_o ~   tbc_inc+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_7 <- coef(summary(rlm(excess_percentage_o ~   dens_group+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_8 <- coef(summary(rlm(excess_percentage_o ~   city_bezirk+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_1918_9 <- coef(summary(rlm(excess_percentage_o ~  station_area+ Language,data=data_res))) %>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))


Mod_1918_10 <- coef(summary(rlm(excess_percentage_o ~  prop_men + Language ,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))


data_res <- data_all %>%
  filter(Year==2020)

Mod_2020_1 <- coef(summary(rlm(excess_percentage_o ~ Language, data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_2020_2 <- coef(summary(rlm(excess_percentage_o ~  prop_kids +Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_2020_3 <- coef(summary(rlm(excess_percentage_o ~  prop_70 + Language ,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_2020_4 <- coef(summary(rlm(excess_percentage_o ~   dens_group+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))

Mod_2020_5 <- coef(summary(rlm(excess_percentage_o ~   city_bezirk+ Language,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))


Mod_2020_6 <- coef(summary(rlm(excess_percentage_o ~  prop_men + Language ,data=data_res)))%>%
  data.frame() %>%
  mutate(Cofactor=row.names(.)) %>%
  mutate(Value= round(Value,2),
         Cl = round(Value - 1.96* Std..Error,2),
         Cu = round(Value + 1.96* Std..Error,2),
         Univariate = paste0(Value," (",Cl,"-",Cu, ")"))


