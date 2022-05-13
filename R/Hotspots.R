function_hotspot <- function(Year_Pan) {
# load(paste0("../data/expected_death_inla1890.RData"))
# Expected_death_Russian <-expected_deaths
# load(paste0("../data/expected_death_inla1918.RData"))
# Expected_death_Spanish <- expected_deaths
# load(paste0("../data/expected_death_inla2020.RData"))
# Expected_death_Covid <- expected_deaths

  load(paste0("../data/expected_death_inla1890.RData"))
  Expected_death_Russian <-expected_deaths
  load(paste0("../data/expected_death_inla1918.RData"))
  Expected_death_Spanish <- expected_deaths
  load(paste0("../data/expected_death_inla2020.RData"))
  Expected_death_Covid <- expected_deaths
  

data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid)


data_excess <- data_excess %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890") %>%
  mutate(significant_dummy = ifelse(death > LL & death < UL,0,1),
         significant_dummy = as.factor( significant_dummy ),
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage = round(((death-fit)/fit)*100,2))

# sf::sf_use_s2(TRUE)

bezirk_geo <- read_sf("../data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
             | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
             | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
             | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  dplyr::rename(Bezirk = BEZIRKSNUM) %>%
  dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
  dplyr::select(Bezirk, geometry) %>%
  full_join(data_excess) %>%
  filter(Year==Year_Pan) %>%
  select(geometry,excess_percentage)
  
# start here

neighbours <- poly2nb(bezirk_geo$geometry)
listw <- nb2listw(neighbours)
gi.fixed <- localG(bezirk_geo$excess_percentage, listw)


# dnb_lw <- nb2listw(neighbours, style = 'B')
# gi.adapted <- localG(bezirk_geo$excess_percentage, dnb_lw)



bezirk_geo.gi <- cbind(bezirk_geo, as.matrix(gi.fixed)) %>%
  rename(gstat=as.matrix.gi.fixed.)


tm_shape(bezirk_geo.gi) +
  tm_fill(col = "gstat", 
          style = "pretty",
          palette="-RdBu",
          title = "local Gi",
          midpoint = 0) +
  tm_borders(alpha = 0.5) +
  tm_layout(
    main.title = paste0("Hot spot areas -", Year_Pan), 
    main.title.position = "left")

}
