function_mortality_rate <- function() {

  
  load(paste0("data/data_total.RData"))
  load(paste0("data/pop_total.RData"))

data_mortality_rate <- data_excess %>%
  filter(age_group =="total") %>%
  filter(sex=="both") %>%
  dplyr::select(Year, Bezirk, death) %>%
  mutate(Year=as.character(Year),
         Year = as.numeric(Year)) %>%
  full_join(pop_total, by=c("Year", "Bezirk")) %>%
  filter(!is.na(population)) %>%
  mutate(mortality_rate = (death/population)*mort_pop)


save(data_mortality_rate ,file=paste0("data/data_mortality_rate.RData"))
write.xlsx(data_mortality_rate,file=paste0("data/data_mortality_rate.xlsx"),rowNames=FALSE, overwrite = TRUE)

}