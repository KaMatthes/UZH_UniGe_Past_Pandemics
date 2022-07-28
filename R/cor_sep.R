function_cor_sep <- function(){

load("../data/expected_death_inla2020.RData")
Expected_death_Covid <- expected_deaths
  
  load("../data/Swiss_SEP.RData")
  
    
    data_excess <- Expected_death_Covid %>%
      ungroup() %>%
      full_join(Swiss_SEP) %>%
      mutate(
             Language = as.factor(Language),
             excess_percentage_o = ((death-fit)/fit)*100,
             excess_percentage = round(((death-fit)/fit)*100,2),
             excess_perc_groups =  as.numeric(excess_percentage),
             significant_dummy = ifelse(death > LL & death <UL,0,1),
             significant_dummy = as.factor( significant_dummy ),
             death_inc = death/population *100000,
             Language= factor(Language, levels=c("German","French","Italien")))   %>%
      filter(Year ==2020)

    # plot_SEP <- ggplot(data=data_excess) +
    #   geom_point(aes(x=SEP_Bezirk, y=excess_percentage, shape=Language,col=Language), lwd=3) +
    #   geom_smooth(aes(x=SEP_Bezirk, y=excess_percentage,col=Language), method='lm',lwd=1.5, se=FALSE) +
    #   facet_wrap(~Year, nrow = 2,scales = "free") +
    #   scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
    #   scale_shape_manual("Language region: ",values = c(15,16,17))+
    #   ggtitle("Swiss SEP")+
    #   ylab("Relative Excess Mortality")+
    #   xlab("Swiss SEP") +
    #   theme_bw() +
    #   theme(
    #     strip.text.x=element_text(size=15),
    #     axis.text.x=element_text(color="black",size=10),
    #     axis.title=element_text(size=15),
    #     legend.text=element_text(size=15),
    #     legend.title =element_text(size=15),
    #     plot.title = element_text(size=15),
    #     legend.position = "bottom")
    
    plot_SEP <- ggplot(data=data_excess) +
      geom_point(aes(x=SEP_Bezirk, y=excess_percentage, shape=Language,col=Language), lwd=lwd_size_points ) +
      geom_smooth(aes(x=SEP_Bezirk, y=excess_percentage),  method='loess',se=TRUE,lwd=lwd_size, col=col_line) +
      facet_wrap(~Year, nrow = 2, scales="free_x") +
      scale_color_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_fill_manual("Language region: ",values =  c(cbp1[2],cbp1[1],cbp1[3])) +
      scale_shape_manual("Language region: ",values = c(15,16,17))+
      ggtitle("Swiss SEP")+
      ylab("Relative Excess Mortality")+
      xlab("Swiss SEP") +
      theme_bw() +
      theme(
        strip.text.x=element_text(size=15),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_text(size=15),
        plot.title = element_text(size=15),
        legend.position = "bottom")
    
# cowplot::save_plot("output/plot_SEP.pdf",plot_SEP,base_height=7,base_width=11)
# cowplot::save_plot("output/plot_SEP_all.pdf",plot_SEP2,base_height=7,base_width=11)
# 
# summary(lm(excess_percentage~SEP_Bezirk*Language, data=data_excess))

# plot_SEP_both <- list( plot_SEP , plot_SEP2)
return( plot_SEP)

}



function_test_sep <- function(){
  
  load("../data/expected_death_inla2020.RData")
  Expected_death_Covid <- expected_deaths
  
  # 
  # load(paste0("data/expected_death_inla1890.RData"))
  # Expected_death_Russian <-expected_deaths
  # load(paste0("data/expected_death_inla1918.RData"))
  # Expected_death_Spanish <- expected_deaths
  # load(paste0("data/expected_death_inla2020.RData"))
  # Expected_death_Covid <- expected_deaths
  # 
  
  load("../data/Swiss_SEP.RData")
  
  
  data_excess <- Expected_death_Covid %>%
    ungroup() %>%
    full_join(Swiss_SEP) %>%
    mutate(
      Language = as.factor(Language),
      excess_percentage_o = ((death-fit)/fit)*100,
      excess_percentage = round(((death-fit)/fit)*100,2),
      excess_perc_groups =  as.numeric(excess_percentage),
      significant_dummy = ifelse(death > LL & death <UL,0,1),
      significant_dummy = as.factor( significant_dummy ),
      death_inc = death/population *100000,
      Language= factor(Language, levels=c("German","French","Italien")))   %>%
    filter(Year ==2020)
  
  # summary(gam(excess_percentage ~ s(SEP_Bezirk),data=data_excess))
  summary(lm(excess_percentage ~ SEP_Bezirk,data=data_excess))
  
}

