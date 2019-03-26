get_species_prop_plots <- function(guild, epu, managed_by, region){
  df <- 
    nefsc_survey_disaggregated %>% 
    distinct() %>% 
    filter(EPU %in% epu,
           Management == managed_by,
           !str_detect(`Feeding guild`,"Other")) %>% 
    group_by(EPU, `Feeding guild`, Season, Time) %>% 
    dplyr::summarise(Value = sum(Proportion, na.rm=T)) %>% 
    unite(.,Var,c("Feeding guild","Season"), sep = " ") %>% 
    group_by(EPU,Var) %>% 
    mutate(hline = mean(Value)) %>% 
    filter(str_detect(Var,guild)) %>% 
    ungroup() %>% 
    mutate(Var = paste(str_to_title(str_extract(Var, "fall|spring")),
                       "survey")) %>%
    dplyr::na_if(0)
  
 out <- 
   df %>% 
    ggplot(aes(x = Time, y = Value, group = Var)) +
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max ,
             ymin = -Inf, ymax = Inf) +
    
    #Add time series
    geom_line(size = lwd-0.5) +
    geom_point(size = pcex-0.5) +
    # scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    geom_hline(aes(yintercept = hline,
                   group = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty)+
    
    #Facet 
    facet_wrap(Var~.,scales = "free_y", ncol = 2) +
    ggtitle(paste0(managed_by," ",guild,"s", " in ", region))+
    #Axis and theme
    scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
    ylab(paste("Proportion of",epu,"survey")) +
    theme_facet()+
    theme(strip.text=element_text(hjust=0, size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"))
  return(out)
}