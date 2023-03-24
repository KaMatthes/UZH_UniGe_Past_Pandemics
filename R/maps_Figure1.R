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
  mutate(significant_dummy = ifelse(death > LL & death < UL,"non-significant","significant"),
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
    

jenks_breaks <- c(0,natural_breaks(k=5, bezirk_geo['excess_percentage_o']),max(bezirk_geo$excess_percentage_o))
bezirk_geo <- bezirk_geo %>%
  mutate(excess_jenk = cut(excess_percentage_o,
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
  tm_fill("excess_jenk", title = "Excess Mortality",
           palette = "YlOrBr", 
          # legend.hist = TRUE,
          style = "fixed",
          legend.format=)+
          # legend.is.portrait = FALSE)+
          # style = "kmeans",
          # title = "Relative Excess Mortality") +
  tm_borders(alpha = 1, lwd=0.8, lty="dashed",col="grey20") +
  
  # tm_shape( data_lake  ) + 
  # tm_fill(col = "azure3") +
  # tm_shape(data_centroiod ) + 
  # tm_dots(title = "Cantonal capital",palette = "black", size = 0.6, shape=23, jitter=0.15, legend.show = TRUE) +
 
  tm_shape(data_canton)  +
  tm_borders(col = "grey20", lwd = 1.5) +
  tm_shape(data_sig) + 
  # tm_dots(title = "",col="significant_dummy", palette=c(non='#00FF00', significant='#6666FF'),size =0.5, shape=21)+
  tm_dots(title = "",col="significant_dummy", palette=c(non=NA, significant='#373737'),border.lwd = 1.5,border.col = "black",size =0.4, shape=21)+
  tm_add_legend(title = "",labels ="District boundaries",type = "line", lty="dashed", col="black") +
  tm_add_legend(title = "",labels ="Cantonal boundaries",type = "line", lty="solid", col="black") +
  # tm_add_legend(title = "",labels ="Cantonal capital",type = "symbol", shape = 23, col="black") +
  tm_layout(
    frame =TRUE,
    main.title=Year_Pan,
    main.title.size = 2,
    main.title.position = c(0.32),
    legend.outside = TRUE,
    legend.outside.position = "left",
    # legend.outside.size = 2,
    legend.position = c(0.4,0.1),
    legend.title.size=2.0,
    legend.text.size =1.6)
    # legend.text.size = legend_size_map)

    # title.position = c(0.1,0.95),
  #  +
  # tm_add_legend( legend.title.size=legend_size_title,
  #                # legend.show=TRUE)  +
  #                legend.outside = TRUE,
  #                legend.text.size = legend_size_map,
  #                legend.outside.position = "left",
  #                legend.position = c(0.5,0.1))
    # title.size = main_size_map) +


 
   
#    
#   # tm_dots("value")
#   # tm_facets(by="Year", ncol=1)+
#   tm_layout(
#     asp = 1, 
#     frame = TRUE,
#     # title =Year_Pan, 
#     # title.position = c(0.1,0.95),
#     # legend.text.size = legend_size_map,
#     # legend.width = 5,
#     # legend.height = 8,
#     # legend.outside.position = "bottom",
#     # legend.position = c(0.75,0.75),
#     # legend.title.size=legend_size_title,
#     legend.show=FALSE)
#   # title.size = main_size_map) +
#   
#   
# plot_legend <- tm_shape( bezirk_geo  ) + 
#   tm_fill("excess_jenk",
#           palette = "YlOrBr", 
#           # legend.hist = TRUE,
#           style = "fixed",
#           legend.is.portrait = FALSE,
#   # style = "kmeans",
#   title = "") +
#   tm_borders(alpha = 0.5)+
#   # tm_facets(by="Year", ncol=1)+
#   tm_layout(
#     asp = 1, 
#     frame = FALSE,
#     # title =Year_Pan, 
#     # title.position = c(0.1,0.95),
#     legend.text.size = legend_size_map,
#     legend.width = 5,
#     legend.height = 2,
#     legend.only= T,
#     # legend.outside.position = "bottom",
#     legend.position = c(0.1,0.2),
#     legend.title.size=legend_size_title)
# 
# # title.size = main_size_map)


return(plot_excess)

}

