function_regression_mortality <- function(VarTest) {
  if(VarTest == "Sex"){ 
  
    load(paste0("../data/expected_death_inlaf_1890.RData"))
    death_1890_f <- expected_deaths

    load(paste0("../data/expected_death_inlaf_1918.RData"))
    death_1918_f <- expected_deaths

    load(paste0("../data/expected_death_inlaf_2020.RData"))
    death_2020_f <- expected_deaths

    load(paste0("../data/expected_death_inlam_1890.RData"))
    death_1890_m <- expected_deaths

    load(paste0("../data/expected_death_inlam_1918.RData"))
    death_1918_m <- expected_deaths

    load(paste0("../data/expected_death_inlam_2020.RData"))
    death_2020_m <- expected_deaths
  # 
  # load(paste0("data/expected_death_inlaf_1890.RData"))
  # death_1890_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_inlaf_1918.RData"))
  # death_1918_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_inlaf_2020.RData"))
  # death_2020_f <- expected_deaths
  # 
  # load(paste0("data/expected_death_inlam_1890.RData"))
  # death_1890_m <- expected_deaths
  # 
  # load(paste0("data/expected_death_inlam_1918.RData"))
  # death_1918_m <- expected_deaths
  # 
  # load(paste0("data/expected_death_inlam_2020.RData"))
  # death_2020_m <- expected_deaths
  
  data_excess <- rbind(death_1890_m,death_1918_m, death_2020_m,
                       death_1890_f,death_1918_f, death_2020_f )%>%
    select(-column_label) %>%
    rename(sex=sex.x) %>%
    ungroup() %>%
    mutate(excess_death = death - fit,
           excess_rate = (excess_death/population)*10000,
           Bezirk = as.factor(Bezirk),
           excess_percentage_o = ((death-fit)/fit)*100,
           excess_percentage = round(((death-fit)/fit)*100,2),
           excess_perc_groups =  as.numeric(excess_percentage)) %>%
    filter(Year==1890 | Year==1918 | Year==2020) 
  
  m1 <- summary(lm(excess_percentage ~ sex, data_excess[data_excess$Year==1890,]))
  m2 <- summary(lm(excess_percentage ~ sex, data_excess[data_excess$Year==1918,]))
  m3 <- summary(lm(excess_percentage ~ sex, data_excess[data_excess$Year==2020,]))
  m <- list(m1,m2,m3)
  return(m)
  
  }
  
  else if(VarTest == "Age"){ 
    
    # load(paste0("data/expected_death_inla0_69_1890.RData"))
    # death_1890_0_69 <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla70_1890.RData"))
    # death_1890_70 <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla70_1918.RData"))
    # death_1918_70 <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla0_69_1918.RData"))
    # death_1918_0_69 <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla0_69_2020.RData"))
    # death_2020_0_69<- expected_deaths
    # 
    # load(paste0("data/expected_death_inla70_2020.RData"))
    # death_2020_70  <- expected_deaths
    
    
    load(paste0("../data/expected_death_inla0_69_1890.RData"))
    death_1890_0_69 <- expected_deaths

    load(paste0("../data/expected_death_inla70_1890.RData"))
    death_1890_70 <- expected_deaths

    load(paste0("../data/expected_death_inla70_1918.RData"))
    death_1918_70 <- expected_deaths

    load(paste0("../data/expected_death_inla0_69_1918.RData"))
    death_1918_0_69 <- expected_deaths

    load(paste0("../data/expected_death_inla0_69_2020.RData"))
    death_2020_0_69<- expected_deaths

    load(paste0("../data/expected_death_inla70_2020.RData"))
    death_2020_70  <- expected_deaths
    # 
    data_excess <- rbind(death_1890_0_69,death_1918_0_69,death_2020_0_69,
                         death_1890_70, death_1918_70, death_2020_70)%>%
      select(-column_label) %>%
      mutate(excess_death = death - fit,
             excess_rate = (excess_death/population)*10000,
             Bezirk = as.factor(Bezirk),
             age_group =  age_group.x,
             excess_death = death - fit,
             excess_rate = (excess_death/population)*10000,
             Bezirk = as.factor(Bezirk),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage)) %>%
      filter(Year==1890 | Year==1918 | Year==2020) 
    
    m1 <- summary(lm(excess_percentage ~ age_group.y , data_excess[data_excess$Year==1890,]))
    m2 <- summary(lm(excess_percentage ~ age_group.y , data_excess[data_excess$Year==1918,]))
    m3 <- summary(lm(excess_percentage ~ age_group.y , data_excess[data_excess$Year==2020,]))
    m <- list(m1,m2,m3)
    return(m)
    
  }
  
  else if(VarTest == "Age_1918"){ 
    
    # load(paste0("data/expected_death_inla0_19_1918.RData"))
    # death_1918_0_19 <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla20_39_1918.RData"))
    # death_1918_20_39 <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla40_69_1918.RData"))
    # death_1918_40_69 <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla70_1918.RData"))
    # death_1918_70 <- expected_deaths

    load(paste0("../data/expected_death_inla0_19_1918.RData"))
    death_1918_0_19 <- expected_deaths

    load(paste0("../data/expected_death_inla20_39_1918.RData"))
    death_1918_20_39 <- expected_deaths

    load(paste0("../data/expected_death_inla40_69_1918.RData"))
    death_1918_40_69 <- expected_deaths

    load(paste0("../data/expected_death_inla70_1918.RData"))
    death_1918_70 <- expected_deaths

    
    data_excess <- rbind(death_1918_0_19,death_1918_20_39, death_1918_40_69,death_1918_70)%>%
      select(-column_label) %>%
      mutate(excess_death = death - fit,
             excess_rate = (excess_death/population)*10000,
             Bezirk = as.factor(Bezirk),
             age_group =  age_group.x) %>%
      filter(Year==1918) %>%
      mutate(significant_dummy = ifelse(death > LL & death < UL,0,1),
             significant_dummy = as.factor( significant_dummy ),
             age_group=factor(age_group, levels=c("0_19","20_39","40_69", ">70")),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             age_group= factor(age_group, levels=c(">70","0_19","20_39","40_69")))
    # sf::sf_use_s2(TRUE)
    
     summary(lm(excess_percentage ~ age_group , data_excess))
    
  }
  else if(VarTest == "Age_Sex_1918"){ 
    
    load(paste0("../data/expected_death_inla_20_39_f_1918.RData"))
    death_1918_20_39_f <- expected_deaths

    load(paste0("../data/expected_death_inla_20_39_m_1918.RData"))
    death_1918_20_39_m <- expected_deaths
    
    # load(paste0("data/expected_death_inla_20_39_f_1918.RData"))
    # death_1918_20_39_f <- expected_deaths
    # 
    # load(paste0("data/expected_death_inla_20_39_m_1918.RData"))
    # death_1918_20_39_m <- expected_deaths
    
    
    data_excess <- rbind(death_1918_20_39_f,death_1918_20_39_m) %>%
      select(-column_label) %>%
      mutate(excess_death = death - fit,
             excess_rate = (excess_death/population)*10000,
             Bezirk = as.factor(Bezirk),
             age_group =  age_group.x,
             significant_dummy = ifelse(death > LL & death < UL,0,1),
             significant_dummy = as.factor( significant_dummy),
             xcess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_percentage = ifelse(excess_percentage >400, 400, excess_percentage))
    
    summary(lm(excess_percentage ~ sex , data_excess))
    
  }
}