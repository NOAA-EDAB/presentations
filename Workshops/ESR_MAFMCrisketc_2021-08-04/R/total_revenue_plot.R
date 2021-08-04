plot_total_revenue <- function(epu_abbr, council){
  
  #Filtering and aggregation step
  rev_agg <- ecodata::comdat %>% 
    filter(str_detect(Var, "Revenue"),
           !str_detect(Var, "prop|Other|NEFMC"), #Remove proportions, "Other" category species, NEFMC managed species in MAB
           EPU == epu_abbr,
           Time >= 1986) %>% 
    mutate(Status = ifelse(str_detect(Var, "Revenue weight"), 
                           "Managed","Total")) %>% #Create groups for aggregation
    group_by(Status, Time) %>% 
    dplyr::summarise(Total = sum(Value)) %>% 
    group_by(Status) %>% 
    mutate(hline = mean(Total))
  
  series.col <- c("indianred","black")
  
  #Plotting
  out_totalrev <- ggplot(data = rev_agg) +
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf)+  
    
    #lines
    geom_gls(aes(x = Time, y = Total,
                 group = Status),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Total, color = Status), size = lwd) +
    geom_point(aes(x = Time, y = Total, color = Status), size = pcex) +
    
    #axes
    scale_y_continuous(labels = function(l){trans = l / 1000000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    ggtitle("Total revenue") +
    ylab(expression("Revenue (10"^6*"USD)")) +
    geom_hline(aes(yintercept = hline,
                   color = Status),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts()
    theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))
  
  return(out_totalrev)
  
}