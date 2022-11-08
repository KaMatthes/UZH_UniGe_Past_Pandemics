function_maps_inla <- function(Year_Pan){

    load(paste0("data/expected_death_inla1890.RData"))
    Expected_death_Russian <-expected_deaths
    load(paste0("data/expected_death_inla1918.RData"))
    Expected_death_Spanish <- expected_deaths
    load(paste0("data/expected_death_inla2020.RData"))
    Expected_death_Covid <- expected_deaths

    # load(paste0("data/expected_death_inla1890.RData"))
    # Expected_death_Russian <-expected_deaths
    # load(paste0("data/expected_death_inla1918.RData"))
    # Expected_death_Spanish <- expected_deaths
    # load(paste0("data/expected_death_inla2020.RData"))
    # Expected_death_Covid <- expected_deaths

    data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid)
    

# save(data_excess ,file=paste0("data/data_excess.RData"))
# write.xlsx(data_excess,file=paste0("data/data_excess.xlsx"),row.names=FALSE, overwrite = TRUE)

data_excess <- data_excess %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890") %>%
  mutate(significant_dummy = ifelse(death > LL & death < UL,0,1),
         significant_dummy = as.factor( significant_dummy ))

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
        
              #                           breaks = c(-45, 0, 5, 10, 20, 40, 60,80,100,125), 
      
          # excess_percentage = ifelse(excess_percentage<0, 0, excess_percentage),
          # excess_percentage = ifelse(excess_percentage>100, 100, excess_percentage),
          # excess_perc_groups =  cut(excess_percentage, 
          #                           breaks = c(-45, 0, 5, 10, 20, 40, 60,80,100,125), 
          #                           labels = c("<0 %", "[0 %,5%)", "[5%, 10%)",
          #                                      "[10 %, 20%)", "[20%, 40%)",  "[40%, 60%)",
          #                                      "[60%, 80%)","[80%, 100%)", ">100"),
          #                           include.lowest = TRUE, right = FALSE))
  # filter(Year==Year_Pan) 

jenks_breaks <- c(0,natural_breaks(k=6, bezirk_geo['excess_percentage_o']),max(bezirk_geo$excess_percentage_o))
bezirk_geo <- bezirk_geo %>%
  mutate(excess_jenk = cut(excess_percentage_o,
                           breaks=jenks_breaks,
                           include.lowest = TRUE, right = FALSE)) %>%
  
  filter(Year==Year_Pan) 

plot_excess <- tm_shape( bezirk_geo  ) + 
  tm_fill("excess_jenk",
           palette = "YlOrBr", 
          # legend.hist = TRUE,
          style = "fixed",
          # style = "kmeans",
          title = "Relative Excess Mortality") +
  tm_borders(alpha = 0.5)+
  # tm_facets(by="Year", ncol=1)+
  tm_layout(
    asp = 1, 
    main.title =Year_Pan, 
    main.title.position = "left",
    legend.text.size = legend_size_map,
    # legend.width = 5,
    # legend.height = 8,
    # legend.outside.position = "bottom",
    legend.position = c(0.75,0.75),
    legend.title.size=legend_size_title,
    main.title.size = main_size_map)



return(plot_excess)

}

