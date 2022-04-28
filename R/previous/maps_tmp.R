
# normalisation 0-1
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}
# sf::sf_use_s2(TRUE)
load(paste0("data/data_mortality_rate.RData"))

data_mortality_plot <- data_mortality_rate %>%
  filter(Year=="2020"  |Year=="1918" |Year=="1890" )


bezirk_geo <- read_sf("data_raw/Map_2020/Maps_dissolved/Maps_dissolved_2020.shp") %>%
  filter(!(  BEZIRKSNUM=="1110" |BEZIRKSNUM=="1101" | BEZIRKSNUM=="1102"  | BEZIRKSNUM=="1103" | BEZIRKSNUM=="1104" | BEZIRKSNUM=="1105"
           | BEZIRKSNUM=="1107"  | BEZIRKSNUM=="1106"| BEZIRKSNUM=="1108"| BEZIRKSNUM=="1109"| BEZIRKSNUM=="2225" | BEZIRKSNUM=="2229"
           | BEZIRKSNUM=="1401"  | BEZIRKSNUM=="1402"| BEZIRKSNUM=="1403"| BEZIRKSNUM=="1404"| BEZIRKSNUM=="1405" | BEZIRKSNUM=="1406"
           | BEZIRKSNUM=="311"  | BEZIRKSNUM=="312" | BEZIRKSNUM=="112" | BEZIRKSNUM=="111")) %>%
  dplyr::rename(Bezirk = BEZIRKSNUM) %>%
  dplyr::mutate(Bezirk = as.factor(Bezirk)) %>%
  dplyr::select(Bezirk, geometry) %>%
  full_join(data_mortality_plot) %>%
  mutate(
    mortality_rate = ifelse(is.na(mortality_rate),0,mortality_rate),
    mortality_rate = ifelse(mortality_rate > 300,300,mortality_rate)) %>%
  group_by(Year) %>%
    mutate(Mortality_norm=round(normalit( mortality_rate),2),
           Mortality_scale=scale(mortality_rate))%>%
  ungroup() %>%
  mutate(mort_quant = as.character(cut( Mortality_norm,
                                        breaks=quantile(Mortality_norm,
                                                        probs = seq(0, 1, length.out = no_classes_map + 1)),
                                        include.lowest = TRUE)),
         mort_quant = recode(mort_quant,
                             "[0,0.49]" = "Q1",
                             "(0.49,0.55]" = "Q2",
                             "(0.55,0.61]" = "Q3",
                             "(0.61,0.7]" = "Q4",
                             "(0.7,1]" = "Q5"),
         mort_quant_scale = as.character(cut( Mortality_scale,
                                              breaks=quantile(Mortality_scale,
                                                              probs = seq(0, 1, length.out = no_classes_map + 1)),
                                              include.lowest = TRUE)),
         mort_quant_scale =  recode(mort_quant_scale,
                                    "[-4.71,-0.67]" = "Q1",
                                    "(-0.67,-0.286]" = "Q2",
                                    "(-0.286,0.0893]" = "Q3",
                                    "(0.0893,0.673]" = "Q4",
                                    "(0.673,3.28]" = "Q5")) %>%
  group_by(Year) %>%
         mutate(mortrate_quant = as.character(cut(mortality_rate,
                                              breaks=quantile(mortality_rate,
                                                              probs = seq(0, 1, length.out = no_classes_map + 1)),
                                              include.lowest = TRUE)),
         mortrate_quant =  recode(mortrate_quant,
                                    "[0,91.9]" = "Q1",
                                    "(91.9,170]" = "Q2",
                                    "(170,196]" = "Q3",
                                    "(196,225]" = "Q4",
                                    "(225,300]" = "Q5")) %>%
  ungroup() %>%
  filter(!is.na(Bezirk))


# plot_scale <- ggplot(data=bezirk_geo)+
#   geom_sf(mapping = aes(fill =  mort_quant_scale)) +
#   facet_wrap(~Year, ncol = 3)+
#   scale_fill_manual(values = col_5_groups2_trans)+
#   theme(
#     # panel.background=element_blank(),
#     # panel.grid.minor=element_blank(),
#     panel.grid.major=element_blank(),
#     axis.title=element_blank(),
#     axis.line=element_blank(),
#     axis.text=element_blank(),
#     axis.ticks=element_blank(),
#     panel.border = element_blank(),
#     legend.position = "none")
# 
# cowplot::save_plot("plot_scale.pdf",plot_scale,base_height=10,base_width=15)


plot_norm <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =  mort_quant)) +
  facet_wrap(~Year, ncol = 3)+
  scale_fill_manual(values = col_5_groups2_trans)+
  ggtitle("Normiert")+
  theme(
    # panel.background=element_blank(),
    # panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "none")

cowplot::save_plot("plot_norm.pdf",plot_norm,base_height=10,base_width=15)

plot_mortrate <- ggplot(data=bezirk_geo)+
  geom_sf(mapping = aes(fill =  mortrate_quant)) +
  facet_wrap(~Year, ncol = 3)+
  # scale_fill_manual(values = col_5_groups2_trans)+
  ggtitle("Mortality")+
  theme(
    # panel.background=element_blank(),
    # panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.position = "none")

cowplot::save_plot("plot_mortrate.pdf",plot_mortrate,base_height=10,base_width=15)

plot_combined <- cowplot::plot_grid(plot_norm,plot_mortrate,
                   ncol=1,nrow=2)

cowplot::save_plot("plot_combined.pdf",plot_combined,base_height=10,base_width=15)

