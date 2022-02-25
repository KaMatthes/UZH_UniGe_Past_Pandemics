function_mortality_rate_sex <- function() {


load(paste0("data/data_total.RData"))
load(paste0("data/pop_total_age.RData"))

pop_total_age2 <- pop_total_age %>%
  group_by(Year,Bezirk, sex) %>%
 summarize(population=sum(population))

data_mortality_rate_sex <- data_total %>%
  filter(age_group =="total") %>%
  filter(!sex=="both") %>%
  dplyr::select(Year, Bezirk, death, sex) %>%
  mutate(Year=as.character(Year),
         Year = as.numeric(Year)) %>%
  full_join(pop_total_age2, by=c("Year", "Bezirk", "sex")) %>%
  filter(!is.na(population)) %>%
  mutate(mortality_rate = (death/population)*mort_pop)


save(data_mortality_rate_sex ,file=paste0("data/data_mortality_rate_sex.RData"))
write.xlsx(data_mortality_rate_sex,file=paste0("data/data_mortality_rate_sex.xlsx"),rowNames=FALSE, overwrite = TRUE)

}