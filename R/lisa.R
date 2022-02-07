function_lisa <- function(VarYear){
  
  R1 <- read_excel(paste0("../data/Expected_death_Russian_ZH_101.xlsx"))
  R2 <- read_excel(paste0("../data/Expected_death_Russian_ZH_102.xlsx"))
  R3 <- read_excel(paste0("../data/Expected_death_Russian_ZH_103.xlsx"))
  R4 <- read_excel(paste0("../data/Expected_death_Russian_ZH_104.xlsx"))
  R5 <- read_excel(paste0("../data/Expected_death_Russian_ZH_105.xlsx"))
  R6 <- read_excel(paste0("../data/Expected_death_Russian_ZH_106.xlsx"))
  R7 <- read_excel(paste0("../data/Expected_death_Russian_ZH_107.xlsx"))
  R8 <- read_excel(paste0("../data/Expected_death_Russian_ZH_108.xlsx"))
  R9 <- read_excel(paste0("../data/Expected_death_Russian_ZH_109.xlsx"))
  R10 <- read_excel(paste0("../data/Expected_death_Russian_ZH_110.xlsx"))
  R13 <- read_excel(paste0("../data/Expected_death_Russian_ZH_113.xlsx"))

  load(paste0("../data/Expected_death_Russian.RData"))
  Expected_death_Russian <- rbind(alldata,R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R13)
  load(paste0("../data/Expected_death_Spanish.RData"))
  Expected_death_Spanish <- alldata
  load(paste0("../data/Expected_death_Covid.RData"))
  Expected_death_Covid <- alldata
  # 
  # 
  # R1 <- read_excel(paste0("data/Expected_death_Russian_ZH_101.xlsx"))
  # R2 <- read_excel(paste0("data/Expected_death_Russian_ZH_102.xlsx"))
  # R3 <- read_excel(paste0("data/Expected_death_Russian_ZH_103.xlsx"))
  # R4 <- read_excel(paste0("data/Expected_death_Russian_ZH_104.xlsx"))
  # R5 <- read_excel(paste0("data/Expected_death_Russian_ZH_105.xlsx"))
  # R6 <- read_excel(paste0("data/Expected_death_Russian_ZH_106.xlsx"))
  # R7 <- read_excel(paste0("data/Expected_death_Russian_ZH_107.xlsx"))
  # R8 <- read_excel(paste0("data/Expected_death_Russian_ZH_108.xlsx"))
  # R9 <- read_excel(paste0("data/Expected_death_Russian_ZH_109.xlsx"))
  # R10 <- read_excel(paste0("data/Expected_death_Russian_ZH_110.xlsx"))
  # R13 <- read_excel(paste0("data/Expected_death_Russian_ZH_113.xlsx"))
  # 
  # load(paste0("data/Expected_death_Russian.RData"))
  # Expected_death_Russian <- rbind(alldata,R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R13)
  # load(paste0("data/Expected_death_Spanish.RData"))
  # Expected_death_Spanish <- alldata
  # load(paste0("data/Expected_death_Covid.RData"))
  # Expected_death_Covid <- alldata
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) 
  
  
  # save(data_excess ,file=paste0("data/data_excess.RData"))
  # write.xlsx(data_excess,file=paste0("data/data_excess.xlsx"),row.names=FALSE, overwrite = TRUE)
  
  data_excess <- data_excess %>%
    mutate(excess_death = death - fit,
           excess_rate = (excess_death/population)*10000,
           Bezirk = as.factor(Bezirk)) %>%
    filter(Year=="2020"  |Year=="1918" |Year=="1919" |Year=="1890" |Year=="1889")
  
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
      filter(Year==VarYear)
    
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
      facet_wrap(~Year, ncol = 1) +
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
