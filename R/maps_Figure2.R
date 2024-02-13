function_maps_sex <- function(Year_Pan){


  load("data/expected_death_inlaf_1890.RData")
  death_1890_f <- expected_deaths

  load("data/expected_death_inlaf_1918.RData")
  death_1918_f <- expected_deaths

  load("data/expected_death_inlaf_2020.RData")
  death_2020_f <- expected_deaths

  load("data/expected_death_inlam_1890.RData")
  death_1890_m <- expected_deaths

  load("data/expected_death_inlam_1918.RData")
  death_1918_m <- expected_deaths

  load("data/expected_death_inlam_2020.RData")
  death_2020_m <- expected_deaths

data_excess <- rbind(death_1890_m,death_1918_m, death_2020_m,
                       death_1890_f,death_1918_f, death_2020_f ) %>%
    select(-column_label) %>%
    rename(sex=sex.x) %>%
  ungroup() %>%
  mutate(excess_death = death - fit,
         excess_rate = (excess_death/population)*10000,
         Bezirk = as.factor(Bezirk)) %>%
  filter(Year==Year_Pan) %>%
  mutate(significant_dummy = ifelse(death > LL & death < UL,"non-significant","significant"),
         significant_dummy = as.factor( significant_dummy ))



bezirk_geo <- read_sf("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
             | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
             | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
             | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  dplyr::rename(Bezirk = BEZIRKSNUM) %>%
  dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
  dplyr::select(Bezirk, geometry) %>%
  left_join(data_excess) %>%
  mutate(excess_percentage_o = ((death-fit)/fit)*100,
         excess_percentage_o = ifelse(excess_percentage_o <0,0, excess_percentage_o),
         excess_percentage_o = ifelse(is.na(excess_percentage_o),0, excess_percentage_o),
         excess_percentage = round(((death-fit)/fit)*100,2),
         excess_perc_groups =  as.numeric(excess_percentage),
         sex=recode(sex, 
                    "f" = "female",
                    "m" = "male")) 


jenks_breaks <- c(0,natural_breaks(k=5, bezirk_geo['excess_percentage_o']),max(bezirk_geo$excess_percentage_o)) %>%
 unique()

bezirk_geo <- bezirk_geo %>%
  mutate(excess_jenk = cut(excess_percentage_o,
                           breaks=jenks_breaks,
                           include.lowest = TRUE, right = FALSE))
         # sex=factor(sex, levels=c("male", "female"))) 
# %>%
#   filter(sex==Sex_v) 
  # select(-geometry)  %>%
  # data.frame()


# bezirk_geo_all <- read_sf("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
#   filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
#              | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
#              | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
#              | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
#   rename(Bezirk=BEZIRKSNUM) %>%
#   mutate(Bezirk = as.factor(Bezirk))

# bezirk_geo <- bezirk_geo_all %>%
#   left_join(bezirk_geo) %>%
#   mutate(excess_jenk = ifelse(is.na( excess_jenk), 1, excess_jenk))

data_sig <- bezirk_geo  %>%
  st_centroid() 

sfc <- st_sfc(st_point(c(2702460,1130540)))

data_locarno1 <- data_sig %>%
  filter(Bezirk==2104) %>%
  filter(sex=="male")

data_locarno2 <- data_sig %>%
  filter(Bezirk==2104) %>%
  filter(sex=="female")

st_geometry(data_locarno1)  <- sfc 



data_sig <- data_sig %>%
  filter(!Bezirk==2104) %>%
  as.data.frame() %>%
  rbind(data_locarno1) %>%
  rbind(data_locarno2) %>%
  st_as_sf()


mod <- lm(excess_percentage_o ~ sex, data=bezirk_geo)
summary(mod)
confint(mod)



plot_excess <- 
  tm_shape(data_canton)  +
  tm_fill() +
  tm_shape( bezirk_geo  ) + 
  tm_fill("excess_jenk", title = "Excess Mortality",
          palette = "YlOrBr", 
          # legend.hist = TRUE,
          style = "fixed",
          legend.format=)+
  # legend.is.portrait = FALSE)+
  # style = "kmeans",
  # title = "Relative Excess Mortality") +
  tm_borders(alpha = 1, lwd=0.8, lty="dashed",col="grey20") +
  tm_facets(by="sex", ncol=2)+
  # tm_shape( data_lake  ) + 
  # tm_fill(col = "azure3") +
  # tm_shape(data_centroiod ) + 
  # tm_dots(title = "Cantonal capital",palette = "black", size = 0.6, shape=23, legend.show = TRUE) +
  # 
  tm_shape(data_canton)  +
  tm_borders(col = "grey20", lwd = 1.5) +
  # tm_shape(data_sig) + 
  # tm_dots(title = "",col="significant_dummy", palette=c(non='#00FF00', significant='#6666FF'),size =0.5, shape=21)+
  tm_add_legend(title = "",labels ="District boundaries",type = "line", lty="dashed", col="black") +
  tm_add_legend(title = "",labels ="Cantonal boundaries",type = "line", lty="solid", col="black") +
  # tm_add_legend(title = "",labels ="Cantonal capital",type = "symbol", shape = 23, col="black") +
  tm_layout(
    frame =TRUE,
    panel.label.size = 1.8,
    panel.label.height = 0.6,
    main.title=Year_Pan,
    main.title.size = 1.5,
    main.title.position = c(0.32),
    legend.outside = TRUE,
    legend.outside.position = "left",
    # legend.outside.size = 2,
    legend.position = c(0.5,0.1),
    legend.title.size=1.2,
    legend.text.size =1) 



return(plot_excess)

}

