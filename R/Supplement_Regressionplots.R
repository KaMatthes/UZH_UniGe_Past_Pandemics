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

load("data/ratio_sex.RData")
ratio_sex <- ratio_sex %>%
  select(-MapName) %>%
  filter(Year == 1890 | Year == 1918 | Year == 2020)


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


data_all <- rbind(Expected_death_Russian, Expected_death_Spanish,  Expected_death_Covid ) %>%
  ungroup() %>%
  filter(Year == 1890 | Year == 1918 | Year == 2020)  %>%
  left_join(Canton) %>%
  # left_join(GDP_data) %>%
  left_join(SEP_normalised) %>%
  left_join(Hospitals_data ) %>%
  left_join( prop_school_kids ) %>%
  left_join( prop_70_year) %>%
  left_join( ratio_sex) %>%
  left_join(child_mortality) %>%
  left_join(Tbc_data) %>%
  left_join(area_data) %>%
  left_join(Urbanity) %>%
  left_join(Stations) %>%
  filter(!is.na(area_Bezirk)) %>%
  mutate(station_area = n_stat/population*1000,
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

# GDP

plot_sep_normalised <- ggplot(data=data_all) +
  geom_point(aes(x= sep_z, y=excess_percentage_o, shape=Language,col=Language),  size=lwd_size_points ) +
  geom_smooth(aes(x= sep_z, y=excess_percentage_o),  method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
  facet_wrap(~Year, nrow = 1) +
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("GDP/SEP")+
  ylab("Relative Excess Mortality")+
  xlab("GDP/SEP normalized") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_sep_normalised.pdf",plot_sep_normalised,base_height=6,base_width=15)

ann_text <- data.frame(Language = c("German","German","German"),sep_z = c(0.95,0.95,0.95),
                       Year = c("1890","1918","2020"))

kruskal.test(sep_z~Language, data=data_all[data_all$Year==1890,])
kruskal.test(sep_z~Language, data=data_all[data_all$Year==1918,])
kruskal.test(sep_z~Language, data=data_all[data_all$Year==2020,])

plot_sep_boxplot <- ggplot(data=data_all,aes(y=sep_z,x=Language, fill=Language)) +
  geom_violin() +
  facet_wrap(~Year, nrow = 1) +
  stat_summary(fun = mean,
               width = 0.4,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  geom_text(data = ann_text,label=c("p-value = 0.0013","p-value = 0.0411","p-value = 0.0007"),size=5)+
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("GDP/SEP")+
  ylab("GDP/SEP normalized")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_sep_boxplot.pdf",plot_sep_boxplot,base_height=6,base_width=15)

plot_urbanity <- ggplot(data=data_all,aes(x=Year, y=excess_percentage_o, fill = city_bezirk)) +
  geom_split_violin() +
  stat_summary(fun = median,
               width = 0.25,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  scale_shape_manual("", 
                     values=c("median")) +
  scale_fill_manual("Urbanity: ",
                    values = c(16,15), 
                    labels=c("rural","urban"))  +
  ggtitle("Urbanity")+
  ylab("Relative Excess Mortality")+
  xlab("Year") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title =element_text(size=25),
        plot.title = element_text(size=25),
        legend.position = "bottom")

cowplot::save_plot("output/plot_urbanity.pdf", plot_urbanity,base_height=15,base_width=15)


Urbanitytab <- table(data_all$city_bezirk,data_all$Language)
Urbanitytabpro <- prop.table(Urbanitytab,2)
Urbanitytabpro <-melt(Urbanitytabpro)
colnames(Urbanitytabpro) <- c("Urbanity","Language","prop")

ann_text <- data.frame(Language = "German",prop = 0.95)
chisq.test(Urbanitytab)

plot_urbanity_freq <- ggplot() +
  geom_bar(data=Urbanitytabpro,aes(x =  Language , y = prop,fill = as.factor(Urbanity)),stat="identity")+
  geom_text(data = ann_text,aes(x=Language, y=prop,label=c("p-value < 0.0000")),size=5)+
  scale_y_continuous(labels=percent)+
  scale_fill_manual("Urbanity: ",
                    values = c(16,15), 
                    labels=c("rural","urban"))  +
  
  ggtitle("Urbanity")+
  ylab("Percent")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title =element_text(size=25),
        plot.title = element_text(size=25),
        legend.position = "bottom")

cowplot::save_plot("output/plot_urbanity_freq.pdf", plot_urbanity_freq,base_height=15,base_width=15)


plot_density <- ggplot(data=data_all,aes(x=Year, y=excess_percentage_o, fill = dens_group)) +
  geom_split_violin() +
  stat_summary(fun = median,
               width = 0.25,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  scale_shape_manual("", 
                     values=c("median")) +
  scale_fill_manual("",values = c(16,15))  +
  ggtitle("Population density")+
  ylab("Relative Excess Mortality")+
  xlab("Year") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title =element_text(size=25),
        plot.title = element_text(size=25),
        legend.position = "bottom")


cowplot::save_plot("output/plot_density.pdf", plot_density ,base_height=15,base_width=15)

Density_1890 <- data_all %>%
  filter(Year==1890)
Densitytab1890 <- 
Densitytab1890 <- table(Density_1890$dens_group,Density_1890$Language)
Densitytabpro1890 <- prop.table(Densitytab1890,2)
Densitytabpro1890 <-melt(Densitytabpro1890)
colnames(Densitytabpro1890) <- c("Density","Language","prop") 
Densitytabpro1890 <- Densitytabpro1890%>%
  mutate(Year=1890)


Density_1918 <- data_all %>%
  filter(Year==1918)
Densitytab1918 <- 
  Densitytab1918 <- table(Density_1918$dens_group,Density_1918$Language)
Densitytabpro1918 <- prop.table(Densitytab1918,2)
Densitytabpro1918 <-melt(Densitytabpro1918)
colnames(Densitytabpro1918) <- c("Density","Language","prop")
Densitytabpro1918 <- Densitytabpro1918 %>%
  mutate(Year=1918)

Density_2020 <- data_all %>%
  filter(Year==2020)
Densitytab2020 <- 
  Densitytab2020 <- table(Density_2020$dens_group,Density_2020$Language)
Densitytabpro2020 <- prop.table(Densitytab2020,2)
Densitytabpro2020 <-melt(Densitytabpro2020)
colnames(Densitytabpro2020) <- c("Density","Language","prop")
  Densitytabpro2020 <- Densitytabpro2020 %>%
  mutate(Year=2020)

density_all <- rbind(Densitytabpro1890,Densitytabpro1918,Densitytabpro2020) %>%
  mutate(Year=as.factor(Year))

chisq.test(Densitytab1890)
chisq.test(Densitytab1918)
chisq.test(Densitytab2020)

ann_text <- data.frame(Language = c("German","German","German"),prop = c(0.95,0.95,0.95),
                       Year = c("1890","1918","2020"), Density=c("small","small","small"))

plot_Density_freq <- ggplot(data=density_all,aes(x =  Language , y = prop,fill = Density)) +
  geom_bar(stat="identity")+
  facet_wrap(~Year, nrow = 1) +
  geom_text(data = ann_text,label=c("p-value = 0.0040","p-value = 0.0040","p-value = 0.0016"),size=5)+
  scale_y_continuous(labels=percent)+
 
  scale_fill_manual("Population density: ",
                    values = c(16,15), 
                    labels=c("small","large"))  +
  ggtitle("Population Density")+
  ylab("Percent")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=18),
        axis.text=element_text(color="black",size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=18),
        legend.position = "bottom")


cowplot::save_plot("output/plot_Density_freq.pdf", plot_Density_freq,base_height=6,base_width=15)


plot_schoolkids <- ggplot(data=data_all) +
  geom_point(aes(x= prop_kids, y=excess_percentage_o, shape=Language,col=Language),  size=3.5 ) +
  geom_smooth(aes(x= prop_kids, y=excess_percentage_o), method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
  facet_wrap(~Year, nrow = 1, scales="free_x") +
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Children aged 5-14 years")+
  ylab("Relative Excess Mortality")+
  xlab("Proportion of Children aged 5-14 years") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=18),
        axis.text=element_text(color="black",size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=18),
        legend.position = "bottom")
# 
cowplot::save_plot("output/plot_schoolkids.pdf", plot_schoolkids,base_height=8,base_width=15)


kruskal.test(prop_kids~Language, data=data_all[data_all$Year==1890,])
kruskal.test(prop_kids~Language, data=data_all[data_all$Year==1918,])
kruskal.test(prop_kids~Language, data=data_all[data_all$Year==2020,])

ann_text <- data.frame(Language = c("German","German","German"),prop_kids = c(0.3,0.3,0.3),
                       Year = c("1890","1918","2020"))

plot_schoolkids_boxplot <- ggplot(data=data_all,aes(y= prop_kids,x=Language, fill=Language)) +
  geom_violin() +
  facet_wrap(~Year, nrow = 1) +
  stat_summary(fun = mean,
               width = 0.4,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  geom_text(data = ann_text,label=c("p-value = 0.6033","p-value = 0.3177","p-value < 0.0000"),size=5)+
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Children aged 5-14 years")+
  ylab("Proportion of Children aged 5-14 years")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_schoolkids_boxplot.pdf", plot_schoolkids_boxplot,base_height=6,base_width=15)


plot_age70 <- ggplot(data=data_all) +
  geom_point(aes(x= prop_70, y=excess_percentage_o, shape=Language,col=Language),  size=3.5 ) +
  geom_smooth(aes(x= prop_70, y=excess_percentage_o), method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
  facet_wrap(~Year, nrow = 1, scales="free_x") +
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Population aged >=70 years")+
  ylab("Relative Excess Mortality")+
  xlab("Proportion of population aged >=70 years") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=18),
        axis.text=element_text(color="black",size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=18),
        legend.position = "bottom")
# 
cowplot::save_plot("output/plot_age70.pdf",  plot_age70,base_height=8,base_width=15)

ann_text <- data.frame(Language = c("German","German","German"),prop_70 = c(0.20,0.20,0.20),
                       Year = c("1890","1918","2020"))

kruskal.test(prop_70~Language, data=data_all[data_all$Year==1890,])
kruskal.test(prop_70~Language, data=data_all[data_all$Year==1918,])
kruskal.test(prop_70~Language, data=data_all[data_all$Year==2020,])

plot_age70_boxplot <- ggplot(data=data_all,aes(y= prop_70,x=Language, fill=Language)) +
  geom_violin() +
  facet_wrap(~Year, nrow = 1) +
  stat_summary(fun = mean,
               width = 0.4,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  geom_text(data = ann_text,label=c("p-value = 0.0056","p-value = 0.0820","p-value < 0.0000"),size=5)+
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Population aged >=70 years")+
  ylab("Proportion of population aged >=70 years")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_age70_boxplot.pdf", plot_age70_boxplot,base_height=6,base_width=15)

plot_prop_men <- ggplot(data=data_all) +
  geom_point(aes(x= prop_men, y=excess_percentage_o, shape=Language,col=Language),  size=3.5 ) +
  geom_smooth(aes(x=prop_men, y=excess_percentage_o), method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
  facet_wrap(~Year, nrow = 1, scales="free_x") +
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Proportion of men")+
  ylab("Relative Excess Mortality")+
  xlab("Proportion of men") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=18),
        axis.text=element_text(color="black",size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=18),
        legend.position = "bottom")
# 
cowplot::save_plot("output/plot_prop_men.pdf",  plot_prop_men,base_height=8,base_width=15)

ann_text <- data.frame(Language = c("German","German","German"),prop_men = c(0.60,0.60,0.60),
                       Year = c("1890","1918","2020"))

kruskal.test(prop_men~Language, data=data_all[data_all$Year==1890,])
kruskal.test(prop_men~Language, data=data_all[data_all$Year==1918,])
kruskal.test(prop_men~Language, data=data_all[data_all$Year==2020,])

plot_prop_men_boxplot <- ggplot(data=data_all,aes(y= prop_men,x=Language, fill=Language)) +
  geom_violin() +
  facet_wrap(~Year, nrow = 1) +
  stat_summary(fun = mean,
               width = 0.4,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  geom_text(data = ann_text,label=c("p-value < 0.0000","p-value < 0.0000","p-value < 0.0000"),size=5)+
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Proportion of men")+
  ylab("Proportion of men")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_prop_men_boxplot.pdf", plot_prop_men_boxplot ,base_height=6,base_width=15)

plot_child_mortality <- ggplot(data=data_all[!data_all$Year==2020,]) +
  geom_point(aes(x= prop_child_death, y=excess_percentage_o, shape=Language,col=Language),  size=3.5) +
  geom_smooth(aes(x= prop_child_death, y=excess_percentage_o),  method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
  facet_wrap(~Year, ncol = 2,scales = "free_x") +
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Child Mortality")+
  ylab("Relative Excess Mortality")+
  xlab("Child Mortality per 10'000 inhabitants") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=18),
        axis.text=element_text(color="black",size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title =element_text(size=18),
        plot.title = element_text(size=18),
        legend.position = "bottom")

cowplot::save_plot("output/plot_child_mortality.pdf",plot_child_mortality ,base_height=10,base_width=15)


kruskal.test(prop_child_death~Language, data=data_all[data_all$Year==1890,])
kruskal.test(prop_child_death~Language, data=data_all[data_all$Year==1918,])

ann_text <- data.frame(Language = c("German","German"),prop_child_death = c(15,15),
                       Year = c("1890","1918"))

plot_child_mortality_boxplot <- ggplot(data=data_all[!data_all$Year==2020,],aes(y= prop_child_death,x=Language, fill=Language)) +
  geom_violin() +
  facet_wrap(~Year, nrow = 1) +
  stat_summary(fun = mean,
               width = 0.4,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  geom_text(data = ann_text,label=c("p-value < 0.0000","p-value < 0.0000"),size=5)+
  scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Child Mortality")+
  ylab("Child Mortality per 10'000 inhabitants")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_child_mortality_boxplot.pdf", plot_child_mortality_boxplot,base_height=6,base_width=15)


plot_hospitals <- ggplot(data=data_all[!data_all$Year==2020,],aes(x=Year, y=excess_percentage_o, fill = hosp_group)) +
  geom_split_violin() +
  stat_summary(fun = median,
               width = 0.25,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  scale_shape_manual("", 
                     values=c("median")) +
  scale_fill_manual("Number of hopsitals: ",values = c(16,15))  +
  ggtitle("Number of hospitals")+
  ylab("Relative Excess Mortality")+
  xlab("Year") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title =element_text(size=25),
        plot.title = element_text(size=25),
        legend.position = "bottom")

cowplot::save_plot("output/plot_hospitals.pdf", plot_hospitals ,base_height=15,base_width=15)  

Hospital_1890 <- data_all %>%
  filter(Year==1890)

Hospitaltab1890 <- table(Hospital_1890$hosp_group,Hospital_1890$Language)
Hospitaltabpro1890 <- prop.table(Hospitaltab1890,2)
Hospitaltabpro1890 <-melt(Hospitaltabpro1890)
colnames(Hospitaltabpro1890) <- c("Hospital","Language","prop") 
Hospitaltabpro1890 <- Hospitaltabpro1890 %>%
  mutate(Year=1890)


Hospital_1918 <- data_all %>%
  filter(Year==1918)
Hospitaltab1918 <- 
  Hospitaltab1918 <- table(Hospital_1918$hosp_group,Hospital_1918$Language)
Hospitaltabpro1918 <- prop.table(Hospitaltab1918,2)
Hospitaltabpro1918 <-melt(Hospitaltabpro1918)
colnames(Hospitaltabpro1918) <- c("Hospital","Language","prop")
Hospitaltabpro1918 <- Hospitaltabpro1918 %>%
  mutate(Year=1918)

Hospital_all <- rbind(Hospitaltabpro1890,Hospitaltabpro1918) %>%
  mutate(Year=as.factor(Year))


chisq.test(Hospitaltab1890)
chisq.test(Hospitaltab1918)


ann_text <- data.frame(Language = c("German","German"),prop = c(0.95,0.95),
                       Year = c("1890","1918"), Hospital=c("0",">=1"))

plot_Hospital_freq <- ggplot(data=Hospital_all,aes(x =  Language , y = prop,fill = as.factor(Hospital))) +
  geom_bar(stat="identity")+
  facet_wrap(~Year, nrow = 1) +
  scale_y_continuous(labels=percent) +
  scale_fill_manual("Number of Hospitals: ",
                    values = c(16,15), 
                    labels=c("0",">=1"))  +
  geom_text(data = ann_text,label=c("p-value = 0.3823","p-value = 0.0702"),size=5)+
  ggtitle("Number of hospitals")+
  ylab("Percent")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title =element_text(size=25),
        plot.title = element_text(size=25),
        legend.position = "bottom")


cowplot::save_plot("output/plot_Hospital_freq.pdf", plot_Hospital_freq,base_height=10,base_width=15)

plot_stations_area <- ggplot(data=data_all[!data_all$Year==2020,]) +
  geom_point(aes(x=station_area, y=excess_percentage, shape=Language,col=Language),  size=3.5) +
  geom_smooth(aes(x=station_area, y=excess_percentage),  method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
  facet_wrap(~Year, nrow = 1) +
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Number of railway stations per 1'000 inhabitants")+
  ylab("Relative Excess Mortality")+
  xlab("Number of railway stations per 1'000 inhabitants") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")
# 
cowplot::save_plot("output/plot_stations_area.pdf", plot_stations_area,base_height=10,base_width=15)


ann_text <- data.frame(Language = c("German","German"),station_area = c(0.9,0.9),
                       Year = c("1890","1918"), Hospital=c("0",">=1"))

kruskal.test(station_area~Language, data=data_all[data_all$Year==1890,])
kruskal.test(station_area~Language, data=data_all[data_all$Year==1918,])

plot_station_area_boxplot <- ggplot(data=data_all[!data_all$Year==2020,],aes(y= station_area,x=Language, fill=Language)) +
  geom_violin() +
  facet_wrap(~Year, nrow = 1) +
  stat_summary(fun = mean,
               width = 0.4,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  geom_text(data = ann_text,label=c("p-value = 0.7855","p-value = 0.7966"),size=5)+
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Number of railway stations per 1'000 inhabitants")+
  ylab("Number of train stations per 1'000 inhabitants")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_station_area_boxplot.pdf", plot_station_area_boxplot,base_height=10,base_width=15)


plot_tbc <- ggplot(data=data_all[!data_all$Year==2020,]) +
  geom_point(aes(x=tbc_inc, y=excess_percentage, shape=Language,col=Language),  size=3.5) +
  geom_smooth(aes(x=tbc_inc, y=excess_percentage), method='rlm',se=TRUE,lwd=lwd_size, col=col_line) +
  facet_wrap(~Year, ncol=2, scales="free_x") +
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Tuberculus mortality")+
  ylab("Relative Excess Mortality")+
  xlab("Tb mortality per 1'000 inhabitants") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_tbc.pdf", plot_tbc,base_height=10,base_width=15)


ann_text <- data.frame(Language = c("German","German"),tbc_inc = c(5,5),
                       Year = c("1890","1918"), Hospital=c("0",">=1"))

kruskal.test(tbc_inc~Language, data=data_all[data_all$Year==1890,])
kruskal.test(tbc_inc~Language, data=data_all[data_all$Year==1918,])

plot_tbc_boxplot <- ggplot(data=data_all[!data_all$Year==2020,],aes(y= tbc_inc,x=Language, fill=Language)) +
  geom_violin() +
  facet_wrap(~Year, nrow = 1) +
  stat_summary(fun = mean,
               width = 0.4,
               position = position_dodge(width = .25),
               aes(shape="median"), 
               colour = "black",
               geom = "crossbar",
               show.legend=FALSE) +
  geom_text(data = ann_text,label=c("p-value = 0.0002","p-value = 0.0773"),size=5)+
  scale_color_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_fill_manual("Language region: ",values =  c(cbp1[1],cbp1[2],cbp1[3])) +
  scale_shape_manual("Language region: ",values = c(15,16,17))+
  ggtitle("Tuberculus mortality")+
  ylab("Tb mortality per 1'000 inhabitants")+
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.text.x=element_text(size=15),
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")

cowplot::save_plot("output/plot_tbc_boxplot.pdf", plot_tbc_boxplot,base_height=10,base_width=15)



