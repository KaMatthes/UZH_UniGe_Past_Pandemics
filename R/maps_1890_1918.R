function_maps_1890_1818 <- function(){

source("../R/bivariate_tmap.R")

load(paste0("../data/expected_death_inla1890.RData"))
Expected_death_Russian <-expected_deaths
load(paste0("../data/expected_death_inla1918.RData"))
Expected_death_Spanish <- expected_deaths

data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish)
data_excess <- data_excess %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year=="1918" |Year=="1890") 


data_excess_1890 <- data_excess %>%
  filter(Year==1890) %>%
  mutate( excess_percentage_o = ((death-fit)/fit)*100) %>%
  select(excess_percentage_o, Bezirk)%>%
  rename(excess_percentage_1890 = excess_percentage_o) 


data_excess_1918 <- data_excess %>%
  filter(Year==1918) %>%
  mutate( excess_percentage_o = ((death-fit)/fit)*100) %>%
  select(excess_percentage_o, Bezirk)%>%
  rename(excess_percentage_1918 = excess_percentage_o) 


data_excess_all <- data_excess_1890  %>%
  full_join(data_excess_1918)


bezirk_geo <- read_sf("../data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
             | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
             | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
             | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  dplyr::rename(Bezirk = BEZIRKSNUM) %>%
  dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
  dplyr::select(Bezirk, geometry) %>%
  full_join(data_excess_all)


bezirk_geo <-as(bezirk_geo, "Spatial")

bivariate_choropleth(bezirk_geo, c("excess_percentage_1890", "excess_percentage_1918"), c("1890", "1918"), bivmap_title="Relation 1890 and 1918")

}
