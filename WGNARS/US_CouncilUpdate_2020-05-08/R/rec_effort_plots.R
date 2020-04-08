rec_effort_plots <- function(region){
  recdat <- ecodata::recdat %>% 
    filter(EPU == region) %>% 
    group_by(Var) %>% 
    mutate(hline = mean(Value))
  
  if (region == "NE"){
    ylim_re <- c(5e6, 30e6)
    ylim_rd <- c(1.75,2.75)
    ylim_ra  <- c(0, 2e6)
  } else {
    ylim_re <- c(2e7, 7e7)
    ylim_rd <- c(1.75,2.75)
    ylim_ra  <- c(1e6, 3.5e6)
  }
  
  
  
  
  #Create dataframe for label locations
  # label_loc <- data.frame(xloc = min(recdat$Time)+0.3,
  #                         yloc = c(ylim_re[2]*0.975,
  #                                  ylim_rd[2]*0.975,
  #                                  ylim_ra[2]*0.975),
  #                         labels = LETTERS[1:3],
  #                         Var = c("Recreational Effort",
  #                                 "Recreational fleet effort diversity across modes",
  #                                 "Recreational anglers"))
  
  series.col <- "black"
  #x.shade.min <- max(recdat$Time, na.rm = T) - 9
  #x.shade.max <- max(recdat$Time, na.rm = T)
  
  series.col <- "black"
  
  rec_effort <- recdat %>% 
    filter(Var == "Recreational Effort") %>% 
    ggplot() + 
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    #label
    # annotate("text", 
    #          x = label_loc[label_loc$Var == "Recreational Effort",]$xloc,
    #          y = label_loc[label_loc$Var == "Recreational Effort",]$yloc,
    #          label = label_loc[label_loc$Var == "Recreational Effort",]$labels,
    #          size = letter_size)+
    geom_gls(aes(x = Time, y = Value,
                 group = Var),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
    geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
    
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(labels = function(l){trans = l / 1000000}, limits = ylim_re)+
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    ggtitle(paste(region, "recreational effort")) +
    ylab(expression("Days fished (10"^6*" days)")) +
    xlab("")+
    geom_hline(aes(yintercept = hline,
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts() +
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
  
  
  rec_anglers <- recdat %>% 
    filter(Var == "Recreational anglers") %>% 
    ggplot() + 
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    # annotate("text", 
    #        x = label_loc[label_loc$Var == "Recreational anglers",]$xloc,
    #        y = label_loc[label_loc$Var == "Recreational anglers",]$yloc,
    #        label = label_loc[label_loc$Var == "Recreational anglers",]$labels,
    #        size = letter_size)+
    geom_gls(aes(x = Time, y = Value,
                 group = Var),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
    geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
    
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(labels = function(l){trans = l / 1000000}, limits = ylim_ra)+
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    ggtitle(paste(region, "recreational anglers"))+
    ylab(expression("Anglers (10"^6*" n)")) +
    xlab("Time")+
    geom_hline(aes(yintercept = hline,
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts()+
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
  
  return(list(rec_effort, rec_anglers))
}

