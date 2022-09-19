function_maps_tbc <- function(Year_Pan){

source("../R/bivariate_tmap.R")

load(paste0("../data/expected_death_inla1890.RData"))
Expected_death_Russian <-expected_deaths
load(paste0("../data/expected_death_inla1918.RData"))
Expected_death_Spanish <- expected_deaths

load("../data/Tbc.RData")

Tbc_data <- Tbc %>%
  mutate(Year = as.factor(Year), 
         Year = recode(Year,"1915" = "1918"))


data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish) %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_percentage_o = ((death-fit)/fit)*100,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk),
         Year = as.factor(Year)) %>%
  full_join(Tbc_data)   %>%
  filter(Year==Year_Pan)



bezirk_geo <- read_sf("../data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
             | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
             | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
             | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  dplyr::rename(Bezirk = BEZIRKSNUM) %>%
  dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
  dplyr::select(Bezirk, geometry) %>%
  full_join(data_excess)


bezirk_geo <-as(bezirk_geo, "Spatial")

bivariate_choropleth(bezirk_geo, c("Tbc_Bezirk", "excess_percentage_o"), c("Tbc", Year_Pan), bivmap_title="Relation Tbc")

}
