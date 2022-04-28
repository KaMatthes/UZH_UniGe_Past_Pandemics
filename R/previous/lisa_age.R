function_lisa_age <- function(VarYear, Age){

  load(paste0("../data/expected_death_inla0_69_1890.RData"))
  death_1890_0_69 <- expected_deaths

  load(paste0("../data/expected_death_inla0_69_1918.RData"))
  death_1918_0_69 <- expected_deaths

  load(paste0("../data/expected_death_inla0_69_2020.RData"))
  death_2020_0_69<- expected_deaths

  load(paste0("../data/expected_death_inla70_1890.RData"))
  death_1890_70 <- expected_deaths

  load(paste0("../data/expected_death_inla70_1918.RData"))
  death_1918_70 <- expected_deaths

  load(paste0("../data/expected_death_inla70_2020.RData"))
  death_2020_70<- expected_deaths

  
  
  
  # load(paste0("data/expected_death_inla0_69_1890.RData"))
  # death_1890_0_69 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla0_69_1918.RData"))
  # death_1918_0_69 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla0_69_2020.RData"))
  # death_2020_0_69<- expected_deaths
  # 
  # 
  # load(paste0("data/expected_death_inla70_1890.RData"))
  # death_1890_70 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla70_1918.RData"))
  # death_1918_70 <- expected_deaths
  # 
  # load(paste0("data/expected_death_inla70_2020.RData"))
  # death_2020_70<- expected_deaths
  
  data_excess <- rbind(death_1890_0_69,death_1918_0_69, death_2020_0_69,
                       death_1890_70,death_1918_70, death_2020_70)%>%
    select(-column_label)
  
  
  data_excess <- data_excess %>%
    ungroup() %>%
    mutate(excess_death = death - fit,
           excess_rate = (excess_death/population)*10000,
           Bezirk = as.factor(Bezirk)) %>%
    filter(Year=="2020"  |Year=="1918" |Year=="1890") %>%
    rename(age_group= age_group.x)
  
  # sf::sf_use_s2(TRUE)
  
  bezirk_geo <- read_sf("../data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
    filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
               | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
               | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
               | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
    dplyr::rename(Bezirk = BEZIRKSNUM) %>%
    dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
    dplyr::select(Bezirk, geometry) %>%
    full_join(data_excess) 
  # plot_list = list()
  # for (YEAR in c(1889, 1890, 1918,1919,2020)) {
    dataLISA <- bezirk_geo %>%
      filter(Year==VarYear) %>%
      filter(age_group==Age)
    
    neighbours <- poly2nb(dataLISA$geometry)
    listw <- nb2listw(neighbours)
    
    globalMoran <- moran.test(dataLISA$excess_rate, listw)
    local <- localmoran(x = dataLISA$excess_rate, listw = nb2listw(neighbours, style = "W"))
    quadrant <- vector(mode="numeric",length=nrow(local))
    
    # centers the variable of interest around its mean
    m.qualification <-  dataLISA$excess_rate - mean(dataLISA$excess_rate)     
    
    # centers the local Moran's around the mean
    m.local <- local[,1] - mean(local[,1])    
    
    # significance threshold
    signif <- 0.1 
    
    # builds a data quadrant
    quadrant[m.qualification >0 & m.local>0] <- 4  
    quadrant[m.qualification <0 & m.local<0] <- 1      
    quadrant[m.qualification <0 & m.local>0] <- 2
    quadrant[m.qualification >0 & m.local<0] <- 3
    quadrant[local[,5]>signif] <- 0   
    
    # plot in r
    brks <- c(0,1,2,3,4)
    colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
    
    
    plot_excess <- ggplot(data=dataLISA)+
      geom_sf(mapping = aes(fill =colors[findInterval(quadrant,brks,all.inside=FALSE)])) +
      ggtitle(paste0(VarYear,"-",Age))+
      scale_fill_manual("LISA Cluster map",
                        breaks=c("white", "blue","red","#0000FF66","#FF000066"),
                        labels=c("no significant","low-low","high-high","low-high","high-low"),
                        values =c("white", "blue","red","#0000FF66","#FF000066"))+
      theme(
        panel.grid.major=element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom")
    
    return( plot_excess)
}
# 
# cowplot::plot_grid(function_lisa(1889),function_lisa(1890),function_lisa(1918),function_lisa(1919),function_lisa(2020),
#                    nrow =3 ,ncol = 2)
