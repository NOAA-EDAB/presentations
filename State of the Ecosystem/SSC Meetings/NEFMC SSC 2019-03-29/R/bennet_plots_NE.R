bennet_plots_NE <- function(scale_x_min = scale_x_min){
  #Filter data into two dataframes for plotting
  indicators <- ecodata::bennet %>% 
    filter(EPU %in% c("GOM","GB"),
           Var %in% c("Benthivore VI",
                      "Benthivore PI", 
                      "Benthos VI",
                      "Benthos PI")) %>% 
    mutate(Var, Var = plyr::mapvalues(Var, from = c("Benthivore VI","Benthivore PI",
                                                    "Benthos VI", "Benthos PI"),
                                      to = c("Benthivore Volume","Benthivore Price",
                                             "Volume","Price")))
  
  revchange <- ecodata::bennet %>% 
    filter(EPU %in% c("GOM","GB"),
           Var %in% c("REVCHANGE EPU aggregate"),
           Time >= scale_x_min)
  
  #custom bar fill color (color-blind friendly)
  ind_fill <- c("#a6cee3", "#b2df8a")
  
  #limits
  y.lim <- c(-270,270)
  
  #plot
  
  gom_bennet <- indicators %>% filter(EPU == "GOM" & Var %in% c("Benthivore Volume","Benthivore Price"),
                                      Time >= scale_x_min) %>% 
    ggplot()+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf)+
    #guides(color = F, fill = F)+
    geom_bar(aes(x=Time, y= Value, fill = Var), stat="identity")+
    scale_fill_manual(name = "Indicators", values = ind_fill, guide = FALSE) +
    geom_line(data = revchange[revchange$EPU == "GOM",], aes(x = Time, y = Value, colour="$"))+
    guides(color = FALSE) +
    scale_colour_grey(name ="Total Revenue Change") +
    ggtitle("Gulf of Maine Benthivores Component")+
    labs(y="Value $1,000,000 ($2015)") +
    scale_x_continuous(breaks = seq(scale_x_min, 2015, by = 10), expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), limits = y.lim, expand = c(0.01, 0.01)) +
    theme_ts() +
    theme(title = element_text(size = 10, face = "bold")) +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.title = element_blank(), legend.text = element_blank())
  
  gb_bennet <- indicators %>% filter(EPU == "GB" & Var %in% c("Volume","Price"),
                                     Time >= scale_x_min) %>% 
    ggplot()+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf)+
    
    geom_bar(aes(x=Time, y= Value, fill = Var), stat="identity")+
    scale_fill_manual(name = "Indicators", values = ind_fill) +
    geom_line(data = revchange[revchange$EPU == "GB",], aes(x = Time, y = Value, colour="$"))+
    scale_colour_grey(name ="Total Revenue Change") +
    ggtitle("Georges Bank Benthos Component")+
    labs(y="Value $1,000,000 ($2015)") +
    scale_x_continuous(breaks = seq(scale_x_min, 2015, by = 10), expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), limits = y.lim, expand = c(0.01, 0.01)) +
    theme_ts() +
    theme(title = element_text(size = 10, face = "bold")) +
    theme(legend.position="bottom", 
          legend.direction = "horizontal", 
          legend.background = element_rect(fill = "transparent"), 
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 8)) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 0))
  
    return(list(gom_bennet, gb_bennet))
}
