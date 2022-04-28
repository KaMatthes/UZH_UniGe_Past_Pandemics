function_mortality_rate_sex_age <- function() {


load(paste0("data/data_total.RData"))
load(paste0("data/pop_total_age.RData"))

pop_total_age2 <- pop_total_age %>%
  mutate(age_group= recode(age_group, 
                           "20_29" = "20_39",
                           "30_39" = "20_39",
                           "40_49" = "40_69",
                           "50_59" = "40_69",
                           "60_69" = "40_69")) %>%
  group_by(Year,Bezirk, age_group, sex) %>%
 summarize(population=sum(population))

data_mortality_rate_age_sex <- data_total %>%
  filter(sex =="f" | sex =="m") %>%
  filter(!age_group=="total") %>%
  mutate(age_group= recode(age_group, 
                           "20_29" = "20_39",
                           "30_39" = "20_39",
                           "40_49" = "40_69",
                           "50_59" = "40_69",
                           "60_69" = "40_69")) %>%
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
         


save(data_mortality_rate_age_sex ,file=paste0("data/data_mortality_rate_age_sex.RData"))
write.xlsx(data_mortality_rate_age_sex,file=paste0("data/data_mortality_rate_age_sex.xlsx"),rowNames=FALSE, overwrite = TRUE)

}