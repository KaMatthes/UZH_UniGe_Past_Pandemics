load("data/expected_death_inla1890.RData")
Expected_death_Russian <-expected_deaths
load("data/expected_death_inla1918.RData")
Expected_death_Spanish <- expected_deaths
load("data/expected_death_inla2020.RData")
Expected_death_Covid <- expected_deaths
  
  
data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) 
  
load("data/data_total.RData")

NameBezirk <- data_total %>%
  distinct(MapName, .keep_all = TRUE)%>%
  select(MapName, Bezirk) %>%
  mutate(Bezirk = as.numeric(Bezirk))

vline_pandemic <- tibble(Year=c("1890", "1918","2020")) 
vline_breaks <-  tibble(Year=c("1895", "1920")) 

data_excess_year <- data_excess %>%
  ungroup() %>%
  mutate( Year_group = replace(Year, Year <1896, "1885-1895"),
          Year = as.factor(Year)) %>%
  mutate(Year_group = recode(Year_group, 
                              "1913" = "1913-1920",
                              "1914" = "1913-1920",
                              "1915" = "1913-1920",
                              "1916" = "1913-1920",
                              "1917" = "1913-1920",
                              "1918" = "1913-1920",
                              "1919" = "1913-1920",
                              "1920" = "1913-1920",
                              "2019" = "2019-2020",
                              "2018" = "2019-2020"),
         excess_death = death - fit,
         excess_death_pop = excess_death/population*100000,
         excess_death_pop = ifelse( excess_death_pop  > 2000, 2000,  excess_death_pop ),
         excess_death_pop = ifelse( excess_death_pop  < -2000, -1200,  excess_death_pop ),
         excess_percentage= ((death-fit)/fit)*100,
         excess_percentage= ifelse(excess_percentage > 120, 120, excess_percentage),
         significant_dummy = ifelse(death > LL & death < UL,"non-significant","significant"),
         Difference = ifelse(excess_percentage > 0, "More than expected", "Fewer than expected"),
         Difference_pop = ifelse(excess_death_pop > 0 & (significant_dummy == "significant"),
                                  "significant more",       Difference ),
         # Difference_pop = replace( Difference_pop, excess_death_pop > 0 & Difference_pop=="No-Pandemic",
         #                         "More than expected"),
         # Difference_pop = replace( Difference_pop, excess_death_pop < 0 & Difference_pop=="No-Pandemic",
         #                         "Fewer than expected"),
         
         Bezirk = as.numeric(Bezirk))%>%
  left_join(NameBezirk) %>%
  arrange(Bezirk) 
         
Supplement1 <-  ggplot()+
  geom_vline(data = vline_pandemic, 
             aes(xintercept = Year), 
             size = 6, colour = "grey80", alpha=0.8) +  
  geom_vline(data =vline_breaks, 
             aes(xintercept = Year), 
             size = 0.2, colour = "grey40", linetype="dashed") +  
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_col(data= data_excess_year,aes(x=Year,y = excess_percentage, fill= Difference_pop)) +
  scale_fill_manual(breaks=c("Fewer than expected","More than expected","significant more"),
            values =c("#a6d96a", "#fdae61","#ca0020")) +
  facet_wrap(~ MapName, ncol=10)+
  xlab("") + 
  ylab("Excess death per 100,000 population")+
  theme_bw() +
  theme(
    panel.background = element_blank(), 
    panel.grid = element_line(size=0.2),
    strip.text = element_text(size=20),
    axis.line = element_line(colour = "grey40"),
    axis.ticks = element_line(),
    axis.text.y= element_text(size=14),
    axis.title = element_text(size=20),
    axis.text.x = element_text(size=14, angle=90, hjust = .5, vjust = .5,),
    legend.position = c("none"),
    legend.background = element_blank(),
    legend.title = element_blank())

cowplot::save_plot("output/Supplement1.pdf",Supplement1,base_height=30,base_width=45)
