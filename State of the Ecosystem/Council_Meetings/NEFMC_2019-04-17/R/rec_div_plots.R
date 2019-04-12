

rec_div_plots <- function(region){
  recdat <- ecodata::recdat %>% 
    filter(EPU == region) %>% 
    group_by(Var) %>% 
    mutate(hline = mean(Value))
  
  ylim_re <- c(5e6, 30e6)
  ylim_rd <- c(1.75,2.75)
  ylim_ra  <- c(0, 2e6)
  
  series.col <- "black"
  
  rec_div <- recdat %>% 
    filter(Var == "Recreational fleet effort diversity across modes") %>% 
    ggplot() + 
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    #label
    # annotate("text", 
    #          x = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$xloc,
    #          y = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$yloc,
    #          label = label_loc[label_loc$Var == "Recreational fleet effort diversity across modes",]$labels,
    #          size = letter_size)+
    geom_gls(aes(x = Time, y = Value,
                 group = Var),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
    geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
    ylim(ylim_rd)+
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    ggtitle(paste(region,"rec. fleet effort diversity"))+
    ylab(expression("Effective Shannon")) +
    xlab("")+
    geom_hline(aes(yintercept = hline,
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts() +
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
  
  
  rec_div_catch <- recdat %>% 
    filter(Var == "Recreational Diversity of Catch") %>% 
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
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    ggtitle(paste(region,"rec. diversity of catch"))+
    ylab(expression("Effective Shannon")) +
    xlab("Time")+
    geom_hline(aes(yintercept = hline,
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts()+
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
  
  return(list(rec_div, rec_div_catch))
}
