engagement_plots <- function(epu, type, leg = T, make_sparse = F){
  
  if (epu == "MAB"){
    xmin = -78
    xmax = -70
    ymin = 35
    ymax = 42
  } else if (epu %in% c("GOM","GB")){
    xmin = -76
    xmax = -66
    ymin = 40
    ymax = 46
  } else if (epu == "shelf"){
    ymin <- 35
    ymax <- 46
    xmin <- -78
    xmax <- -66
  }
  
  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)
  
  biv_col <- c("4" = "#ca0020", "3" = "#f4a582", "2" = "grey", "1" = "#92c5de", "0" = "#0571b0")
  
  
  if (epu == "MAB"){
    eng_rel <- ecodata::eng_rel %>%
      filter(PRIMARY_LATITUDE < 42)
  }  else if (epu %in% c("GOM","GB")){
    eng_rel <- ecodata::eng_rel %>%
      filter(PRIMARY_LATITUDE > 40)
  } else if (epu == "shelf"){
    eng_rel <- ecodata::eng_rel
  }
  eng_rel <- eng_rel %>% 
    dplyr::rename(comeng = ComEng_NE16_ct,
                  comrel = ComRel_NE16_ct,
                  receng = RecEng_NE16_ct,
                  recrel = RecRel_NE16_ct,
                  Latitude = PRIMARY_LATITUDE,
                  Longitude = PRIMARY_LONGITUDE,
                  State = STATEABBR) %>% 
    mutate(comeng = factor(comeng, ordered = TRUE, levels = 1:4),
           comrel = factor(comrel, ordered = TRUE, levels = 1:4),
           receng = factor(receng, ordered = TRUE, levels = 1:4),
           recrel = factor(recrel, ordered = TRUE, levels = 1:4))
  
  if (type == "comm"){
    eng_rel <- eng_rel %>% 
      dplyr::select(comeng, comrel, Latitude, Longitude) %>% 
      filter(comeng != 0 &
               comrel != 0)
    fill <- "comeng"
    size <- "comrel"
  } else {
    eng_rel <- eng_rel %>% 
      dplyr::select(receng, recrel, Latitude, Longitude) %>% 
      filter(receng != 0,
             recrel != 0) 
    fill <- "receng"
    size <- "recrel"
  }
  
  if (make_sparse){
    if (type == "comm"){
      eng_rel <- eng_rel %>% filter(comrel != 1,
                                    comeng != 1)
    } else if (type == "rec"){
      eng_rel <- eng_rel %>% filter(recrel != 1,
                                    receng != 1)
    }
  }
  
out <- eng_rel %>% 
      ggplot() +
      geom_sf(data = coast, size = map.lwd) +
      geom_sf(data  = ne_states, size = map.lwd) +
      coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
      geom_point(aes(x = Longitude, y = Latitude,
                     fill = get(fill), size = get(size)),
                 color = "black",pch = 21) +
      {if (!leg) guides(fill = F, size = F) else
        guides(fill = guide_legend(override.aes = list(size = 5),
                                      reverse = T,
                                    title = "Engagement"),
                size = guide_legend(reverse = T,
                                    title = "Reliance"))} +
      scale_fill_manual(values = biv_col) +
      theme_map() +
      xlab("Longitude") +
      ylab("Latitude") +
      theme(legend.position = c(0.8, 0.25), 
            legend.box = "horizontal",
            legend.direction = "vertical",
            legend.key=element_blank(),
            legend.key.width = unit(0, "cm"))

if (type == "comm"){
  out <- out + ggtitle("Commercial Reliance & Engagement") +
    theme(axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"))
} else {
  out <- out + ggtitle("Recreational Reliance & Engagement")+
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
}
      

return(out)
}


