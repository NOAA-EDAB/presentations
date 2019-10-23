plot_total_landings <- function(epu, council){
  managed_landings <- ecodata::comdat  %>%
    filter(str_detect(Var, paste0(council," managed species - Landings weight|JOINT managed species - Landings weight")),
           !str_detect(Var, "Other"),
           Time >= 1986,
           EPU %in% epu)
  
  #Total landings
  total_landings <- ecodata::comdat  %>%
    filter(!str_detect(Var, "managed species"),
           !str_detect(Var, "Other"),
           str_detect(Var, "Landings"),
           Time >= 1986,
           EPU %in% epu)
  
  total_landings_agg <- total_landings %>%
    group_by(Time) %>%
    dplyr::summarise(Value = sum(Value)) %>% 
    mutate(Var = "Total",hline = mean(Value))
  managed_landings_agg <- managed_landings %>%
    group_by(Time) %>%
    dplyr::summarise(Value = sum(Value)) %>% 
    mutate(Var = "Managed",hline = mean(Value))
  
  landings_agg <- rbind(total_landings_agg, managed_landings_agg)
  
  #Define constants for figure plot
  x.shade.max <- 2018
  x.shade.min <- 2009
  series.col <- c("indianred","black")
  
  
  out_total <- ggplot(data = landings_agg)+
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    geom_gls(aes(x = Time, y = Value,
                 group = Var),
             alpha = trend.alpha, size = trend.size) +
    geom_line(aes(x = Time, y = Value, color = Var), size = lwd) +
    geom_point(aes(x = Time, y = Value, color = Var), size = pcex) +
    
    scale_y_continuous(labels = function(l){trans = l / 1000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    ylab(expression("Landings, 10"^3*"metric tons")) +
    ggtitle(paste(epu,"Total Landings"))+
    geom_hline(aes(yintercept = hline,
                   
                   color = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty) +
    theme_ts()+
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
  
  return(out_total)
}