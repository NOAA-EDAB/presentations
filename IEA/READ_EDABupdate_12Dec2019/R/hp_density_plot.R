hp_density <- function(season, leg = T, lat = T){
  hp <- 
    ecodata::hp_density %>% 
    filter(Season == season) %>% 
    ggplot() +
    geom_tile(aes(x = Longitude, y = Latitude, fill = Bin)) +
    {if (!leg) guides(fill = F)} +
      scale_fill_manual(values = matlab.like(22),
                        guide=guide_legend(reverse=TRUE),
                        breaks = levels(ecodata::hp_density$Bin),
                        labels = c("",0,rep("",2),
                                   "0.0001-0.0003",rep("",2),
                                   "0.001-0.002",rep("",2),
                                   "0.008-0.016",rep("",2),
                                   "0.06-0.12",rep("",2),
                                   "0.87-1.7","","",">3",""),
                        name = expression("Density\n (animals km"^-2*")")) +
    geom_sf(data = coast, size = map.lwd) +
    geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
    geom_sf(data  = ne_states, size = map.lwd) +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    ggtitle(season) +
    xlab("Longitude") +
    {if (lat) ylab("Latitude") else ylab("")} +
    ecodata::theme_map()+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.75),
          legend.key = element_blank(),
          axis.title = element_text(size = 11),
          strip.background = element_blank(),
          strip.text=element_text(hjust=0),
          axis.text = element_text(size = 8),
          legend.key.size = unit(0.1,"cm"),
          legend.key.width = unit(0.5,"cm"))
  return(hp)
}
