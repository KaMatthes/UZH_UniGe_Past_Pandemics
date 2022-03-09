function_mortality_rate_sex <- function() {


load(paste0("data/data_total.RData"))
load(paste0("data/pop_total_age.RData"))

pop_total_age2 <- pop_total_age %>%
  mutate(age_group= recode(age_group, 
                           "0_19" = "0_69",
                           "20_29" = "0_69",
                           "30_39" = "0_69",
                           "40_49" = "0_69",
                           "50_59" = "0_69",
                           "60_69" = "0_69")) %>%
  group_by(Year,Bezirk, age_group, sex) %>%
 summarize(population=sum(population))

data_mortality_rate_age2groups_sex <- data_total %>%
  filter(sex =="f" | sex =="m") %>%
  filter(!age_group=="total") %>%
  mutate(age_group= recode(age_group, 
                           "0_19" = "0_69",
                           "20_29" = "0_69",
                           "30_39" = "0_69",
                           "40_49" = "0_69",
                           "50_59" = "0_69",
                           "60_69" = "0_69")) %>%
  mutate(Year = as.character(Year),
         Year = as.numeric(Year))%>%
  filter(Year > 1885) %>%
  dplyr::select(Year, Bezirk, death, age_group, sex) %>%
  group_by(Year,Bezirk, age_group, sex) %>%
  summarise(death=sum(death)) %>%
  ungroup() %>%
  mutate(Year=as.character(Year),
         Year = as.numeric(Year)) %>%
  full_join(pop_total_age2, by=c("Year", "Bezirk", "age_group", "sex")) %>%
  filter(!is.na(population)) %>%
  mutate(death =ifelse(is.na(death), 0, death),
         mortality_rate = (death/population)*mort_pop)%>%
  filter(Year > 1885)
         


save(data_mortality_rate_age2groups_sex ,file=paste0("data/data_mortality_rate_age2groups_sex.RData"))
write.xlsx(data_mortality_rate_age2groups_sex,file=paste0("data/data_mortality_rate_age2groups_sex.xlsx"),rowNames=FALSE, overwrite = TRUE)

}