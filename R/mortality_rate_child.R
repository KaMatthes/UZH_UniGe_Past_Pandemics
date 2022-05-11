function_mortality_rate_child <- function() {


load(paste0("data/child_mortality.RData"))

  child_mortality <- child_mortality %>%
    select(-prop_child_death, -prop_norm, -population)
  
load(paste0("data/pop_child_mortality.RData"))

pop_child_mortality <- pop_child_mortality %>%
  select(-age_group)

data_mortality_rate_child <- child_mortality %>%
  mutate(Year = as.character(Year),
         Year = as.numeric(Year))%>%
  filter(Year > 1885) %>%
  group_by(Year,Bezirk) %>%
  summarise(death=sum(death)) %>%
  ungroup() %>%
  mutate(Year=as.character(Year),
         Year = as.numeric(Year)) %>%
  full_join( pop_child_mortality, by=c("Year", "Bezirk")) %>%
  filter(!is.na(population)) %>%
  mutate(death =ifelse(is.na(death), 0, death),
         mortality_rate = (death/population)*mort_pop)%>%
  filter(Year > 1885)
         


save(data_mortality_rate_child ,file=paste0("data/data_mortality_rate_child.RData"))
write.xlsx(data_mortality_rate_child,file=paste0("data/data_mortality_rate_child.xlsx"),rowNames=FALSE, overwrite = TRUE)

}