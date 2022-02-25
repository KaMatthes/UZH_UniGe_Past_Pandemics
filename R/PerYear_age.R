function_year_age <- function(Age){
# load(paste0("../data/expected_death_f_1890.RData"))
# load(paste0("../data/expected_death_m_1890.RData"))

load(paste0("../data/data_total.RData"))
  
if(Age == "0_19") {
load(paste0("../data/expected_death_0_19_1890.RData"))
death_1890_0_19 <- expected_deaths

load(paste0("../data/expected_death_0_19_1918.RData"))
death_1918_0_19 <- expected_deaths

load(paste0("../data/expected_death_0_19_2020.RData"))
death_2020_0_19 <- expected_deaths

data_excess <- rbind(death_1890_0_19,death_1918_0_19, death_2020_0_19 )%>%
  select(-column_label)
}

else if (Age == "20_49"){
  load(paste0("../data/expected_death_20_49_1890.RData"))
  death_1890_20_49 <- expected_deaths
  
  load(paste0("../data/expected_death_20_49_1918.RData"))
  death_1918_20_49 <- expected_deaths
  
  load(paste0("../data/expected_death_20_49_2020.RData"))
  death_2020_20_49<- expected_deaths
  
  data_excess <- rbind(death_1890_20_49,death_1918_20_49, death_2020_20_49 )%>%
    select(-column_label)

}
  
  else if (Age == "50_69"){
    load(paste0("../data/expected_death_50_69_1890.RData"))
    death_1890_50_69 <- expected_deaths

    load(paste0("../data/expected_death_50_69_1918.RData"))
    death_1918_50_69 <- expected_deaths
    
    load(paste0("../data/expected_death_50_69_2020.RData"))
    death_2020_50_69<- expected_deaths
    
    data_excess <- rbind(death_1890_50_69,death_1918_50_69, death_2020_50_69 )%>%
      select(-column_label)
    
  }

  
  else if (Age == ">70"){
    load(paste0("../data/expected_death_70_1890.RData"))
    death_1890_70 <- expected_deaths
    
    load(paste0("../data/expected_death_70_1918.RData"))
    death_1918_70 <- expected_deaths
    
    load(paste0("../data/expected_death_70_2020.RData"))
    death_2020_70<- expected_deaths
    
    data_excess <- rbind(death_1890_70,death_1918_70, death_2020_70 )%>%
      select(-column_label)
    
  }
  
NameBezirk <- data_total %>%
  distinct(MapName, .keep_all = TRUE)%>%
  select(MapName, Bezirk) %>%
  mutate(Bezirk = as.numeric(Bezirk))

vline_pandemic <- tibble(Year=c("1890", "1918","2020")) 

data_excess_year <- data_excess %>%
  mutate(
         excess_death = death - fit,
         excess_death_pop = excess_death/population*100000,
         excess_death_pop = ifelse( excess_death_pop  > 2000, 2000,  excess_death_pop ),
         excess_death_pop = ifelse( excess_death_pop  < -2000, -1200,  excess_death_pop ),
         excess_percentage= ((death-fit)/fit)*100,
         excess_percentage= ifelse(excess_percentage > 120, 120, excess_percentage),
         Difference = ifelse(excess_percentage > 0, "More than expected", "Fewer than expected"),
         Difference_pop = ifelse(excess_death_pop > 0 & (Year == "1890"  | Year == "1918" | Year == "2020"),
                                 "Pandemic", "No-Pandemic"),
         Difference_pop = replace( Difference_pop, excess_death_pop > 0 & Difference_pop=="No-Pandemic",
                                 "More than expected"),
         Difference_pop = replace( Difference_pop, excess_death_pop < 0 & Difference_pop=="No-Pandemic",
                                 "Fewer than expected"))%>%
  left_join(NameBezirk) %>%
  arrange(Bezirk) %>%
  mutate(Year = as.factor(Year))
         
  # filter(Bezirk==113 | Bezirk==2500) 

 
# plot_year <- ggplot()+
#   geom_vline(data = vline_pandemic, 
#              aes(xintercept = Year), 
#              size = 2, colour = "grey80", alpha=0.8) +  
#   geom_hline(yintercept = 0, colour = "grey80") +
#   geom_col(data= data_excess_year,aes(x=Year,y = excess_death_pop, fill= Difference_pop)) +
#   scale_fill_manual(breaks=c("Fewer than expected","More than expected","Pandemic"),
#                     values =c("#a6d96a", "#fdae61","#ca0020")) +
#   facet_wrap(~MapName, ncol=6)+
#   xlab("") + 
#   ylab("Excess death in percentages")+
#   theme_bw() +
#   theme(
#     panel.background = element_blank(), 
#     axis.line = element_line(colour = "grey40"),
#     axis.ticks = element_line(),
#     axis.text.x = element_text(size=5, angle=90, hjust = .5, vjust = .5,),
#     legend.position = c("none"),
#     legend.background = element_blank(),
#     legend.title = element_blank())
# 
# cowplot::save_plot("output/plot_year.pdf",plot_year,base_height=30,base_width=10)


plot_year_excess <-  ggplot()+
  geom_vline(data = vline_pandemic, 
             aes(xintercept = Year), 
             size = 2, colour = "grey80", alpha=0.8) +  
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_col(data= data_excess_year,aes(x=Year,y = excess_percentage, fill= Difference_pop)) +
  scale_fill_manual(breaks=c("Fewer than expected","More than expected","Pandemic"),
                    values =c("#a6d96a", "#fdae61","#ca0020")) +
  facet_wrap(~ MapName, ncol=6)+
  xlab("") + 
  ylab("Excess death per 100,000 population")+
  theme_bw() +
  theme(
    panel.background = element_blank(), 
    axis.line = element_line(colour = "grey40"),
    axis.ticks = element_line(),
    axis.text.x = element_text(size=5, angle=90, hjust = .5, vjust = .5,),
    legend.position = c("none"),
    legend.background = element_blank(),
    legend.title = element_blank())
# 
# cowplot::save_plot("output/plot_year_excess_30_39.pdf",plot_year_excess,base_height=30,base_width=10)
return(plot_year_excess)
}