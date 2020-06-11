rec_harvest_plots <- function(region){
  landings_rec <- ecodata::recdat %>% 
    filter(EPU == region,
           Var == "Recreational Seafood") %>% 
    mutate(hline = mean(Value))
  
  series.col <- "black"
  
  out <- ggplot(data = landings_rec)+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    geom_gls(aes(x = Time, y = Value,
                 group = Var),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
    geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
    ggtitle(paste(region,"recreational harvest"))+
    scale_y_continuous(labels = function(l){trans = l / 1000000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    ylab(expression("Fish caught (10"^6*"n)")) +
    
    geom_hline(aes(yintercept = hline,
                   
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts()+
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
  return(out)
}
