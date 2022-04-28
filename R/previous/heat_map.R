function_heat_map <- function(Year) {
  

  load(paste0("data/expected_death_1890.RData"))
  Expected_death_Russian <-expected_deaths
  load(paste0("data/expected_death_1918.RData"))
  Expected_death_Spanish <- expected_deaths
  load(paste0("data/expected_death_2020.RData"))
  Expected_death_Covid <- expected_deaths
  load(paste0("data/data_total.RData"))
  
  Bezirke_nr <- data_total %>%
    dplyr::select(MapName, Bezirk, Canton) %>%
    distinct(Bezirk, .keep_all=TRUE)
  
  
  Bezirke_nr2 <- data_total %>%
    dplyr::select(label=MapName, Canton) %>%
    distinct(label, .keep_all=TRUE) %>%
    mutate(Grossregion = Canton,
           Grossregion=recode(Grossregion,
                              "ZH" = "Zurich",
                              "BE" = "Mitteland",
                              "SO" = "Mitteland",
                              "FR" = "Mitteland",
                              "NE" = "Mitteland",
                              "JU" = "Mitteland",
                              "GE" = "Genferseeregion",
                              "VD" = "Genferseeregion",
                              "VS" = "Genferseeregion",
                              "BS" = "Nordwestschweiz",
                              "BL" = "Nordwestschweiz",
                              "AG" = "Nordwestschweiz",
                              "SG" = "Ostschweiz",
                              "TG" = "Ostschweiz",
                              "AI" = "Ostschweiz",
                              "AR" = "Ostschweiz",
                              "GL" = "Ostschweiz",
                              "SH" = "Ostschweiz",
                              "GR" = "Ostschweiz",
                              "TI" = "Tessin",
                              "UR" = "Zentralschweiz",
                              "SZ" = "Zentralschweiz",
                              "OW" = "Zentralschweiz",
                              "NW" = "Zentralschweiz",
                              "LU" = "Zentralschweiz",
                              "ZG" = "Zentralschweiz"),
           Language = Canton,
           Language = recode(Language,
                             "ZH" = "German",
                             "BE" = "German",
                             "SO" = "German",
                             "FR" = "French",
                             "NE" = "French",
                             "JU" = "French",
                             "GE" = "French",
                             "VD" = "French",
                             "VS" = "French",
                             "BS" = "German",
                             "BL" = "German",
                             "AG" = "German",
                             "SG" = "German",
                             "TG" = "German",
                             "AI" = "German",
                             "AR" = "German",
                             "GL" = "German",
                             "SH" = "German",
                             "GR" = "German",
                             "TI" = "Italian",
                             "UR" = "German",
                             "SZ" = "German",
                             "OW" = "German",
                             "NW" = "German",
                             "LU" = "German",
                             "ZG" = "German"),
           Language =ifelse(label=="Bernina", "Italian", Language),
           Language= ifelse(label=="Leuk", "German", Language),
           Language= ifelse(label=="Jura bernois", "French", Language),
           Language= ifelse(label=="Brig", "German", Language),
           Language= ifelse(label=="Visp", "German", Language),
           Language= ifelse(label=="Goms", "German", Language),
           Language= ifelse(label=="Raron", "German", Language)
           )
  
  data_excess <- rbind(Expected_death_Russian, Expected_death_Spanish, Expected_death_Covid) %>%
    mutate(Bezirk=as.character(Bezirk))%>%
    left_join(Bezirke_nr) %>%
    # filter(Year >2014)%>%
    arrange(MapName, Year) %>%
    mutate(excess_percentage_o = (death-fit)/fit) %>%
    select(Year, MapName,Canton,excess_percentage_o ) %>%
    spread( ., Year, excess_percentage_o) %>%
    data.frame(.)
  row.names( data_excess) <-  data_excess$MapName
  data_excess <- data_excess %>%
    select(-MapName, -Canton)
  df <- data_excess
  
  dd <- dist(df, method = "euclidean")
  hc <- hclust(dd, method = "ward.D2")
  
  # hc       <- hclust(dist(df), "ave")           # heirarchal clustering
  dendr    <- dendro_data(hc, type="rectangle") # convert for ggplot
  clust    <- cutree(hc,k=2)                    # find 2 clusters
  clust.df <- data.frame(label=names(clust), cluster=factor(clust)) %>%
    left_join(  Bezirke_nr2)
  # dendr[["labels"]] has the labels, merge with clust.df based on label column
  dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
  # plot the dendrogram; note use of color=cluster in geom_text(...)
  ggplot() + 
    geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, col=Grossregion), 
              size=3) +
    coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank())
  
  cormat <- cor(data_excess)
  
  corrplot::corrplot(cor(data_excess))
  
  ggcorrplot::ggcorrplot(cor(data_excess))
  
  heatmap(cormat,col=col)
  col = terrain.colors(256)
  
  formatted_cors(data_excess) %>%
    ggplot(aes(x = measure1, y = measure2, fill = r))
  
}