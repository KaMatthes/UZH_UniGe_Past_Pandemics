function_maps_crude <- function(Year_Pan){

    load(paste0("data/expected_death_inla1890.RData"))
    Expected_death_Russian <-expected_deaths
    load(paste0("data/expected_death_inla1918.RData"))
    Expected_death_Spanish <- expected_deaths
    load(paste0("data/expected_death_inla2020.RData"))
    Expected_death_Covid <- expected_deaths

    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid)

data_excess <- data_excess %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         mortality = death/population*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890") %>%
  mutate(significant_dummy = ifelse(death > LL & death < UL,"non-significant","significant"),
         significant_dummy = as.factor( significant_dummy )) %>%
  group_by(Year) %>%
  mutate(Total_death = sum(death),
         Total_population = sum(population)) %>%
  ungroup() %>%
  mutate(Total_mortality = Total_death/Total_population *10000)

data_mort <- data_excess %>%
  distinct(Year, .keep_all = TRUE) %>%
  select(Year, Total_mortality)

# sf::sf_use_s2(TRUE)

bezirk_geo <- read_sf("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
           | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
           | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
           | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  dplyr::rename(Bezirk = BEZIRKSNUM) %>%
  dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
  dplyr::select(Bezirk, geometry) %>%
  full_join(data_excess) %>%
  mutate(
          excess_percentage_o = ((death-fit)/fit)*100,
          excess_percentage = round(((death-fit)/fit)*100,2),
          excess_percentage_o = ifelse(excess_percentage_o <0,0, excess_percentage_o))
    

jenks_breaks <- c(0,natural_breaks(k=5, bezirk_geo['mortality']),max(bezirk_geo$mortality))
bezirk_geo <- bezirk_geo %>%
  mutate(mortality_jenk = cut(mortality,
                           breaks=jenks_breaks,
                           include.lowest = TRUE, right = FALSE)) %>%
  
  filter(Year==Year_Pan)

data_sig <- bezirk_geo  %>%
  st_centroid() 

sfc <- st_sfc(st_point(c(2702460,1130540)))

data_locarno <- data_sig %>%
  filter(Bezirk==2104) 
  
st_geometry(data_locarno)  <- sfc 

data_sig <- data_sig %>%
  filter(!Bezirk==2104) %>%
  as.data.frame() %>%
  rbind(data_locarno) %>%
  st_as_sf()

plot_excess <- 
  tm_shape(data_canton)  +
  tm_fill()+
  tm_shape( bezirk_geo  ) + 
  tm_fill("mortality_jenk", title = "Mortality",
           palette = "YlOrBr", 
          style = "fixed",
          legend.format=)+

  tm_borders(alpha = 1, lwd=0.8, lty="dashed",col="grey20") +

  tm_shape(data_canton)  +
  tm_borders(col = "grey20", lwd = 1.5) +
  # tm_shape(data_sig) + 
  # tm_dots(title = "",col="significant_dummy", palette=c(non='#00FF00', significant='#6666FF'),size =0.5, shape=21)+
  # tm_dots(title = "",col="significant_dummy", palette=c(non=NA, significant='#373737'),border.lwd = 1.5,border.col = "black",size =0.4, shape=21)+
  tm_add_legend(title = "",labels ="District boundaries",type = "line", lty="dashed", col="black") +
  tm_add_legend(title = "",labels ="Cantonal boundaries",type = "line", lty="solid", col="black") +
  # tm_add_legend(title = "",labels ="Cantonal capital",type = "symbol", shape = 23, col="black") +
  tm_layout(
    frame =TRUE,
    legend.show=FALSE,
    main.title=Year_Pan,
    main.title.size = 2,
    main.title.position = c(0.32),
    legend.outside = TRUE,
    legend.outside.position = "left",
    # legend.outside.size = 2,
    legend.position = c(0.4,0.1),
    legend.title.size=2.0,
    legend.text.size =1.6)


return(plot_excess)

}

