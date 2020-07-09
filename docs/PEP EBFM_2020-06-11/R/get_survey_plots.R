get_survey_plots <- function(region){
  
  if (region == "NE"){
    epu <- c("GOM","GB")
  } else {
    epu <- "MAB"
    #Get NEAMAP
    neamap <- ecodata::mab_inshore_survey %>% 
      mutate(season = str_to_title(word(Var),1),
             feeding.guild = str_to_title(word(Var,2))) %>% 
      filter(str_detect(Var, "index")) %>% 
      unite(.,Var,c("feeding.guild","season"), sep = " ") %>% 
      group_by(Var) %>% 
      mutate(hline = mean(Value))
    
    neamap$Var <- factor(neamap$Var,levels = c("Piscivore Fall",
                                               "Piscivore Spring",
                                               "Benthivore Fall",
                                               "Benthivore Spring",
                                               "Planktivore Fall",
                                               "Planktivore Spring",
                                               "Benthos Fall",
                                               "Benthos Spring"))
    
  }
  
  
  total_surv <- nefsc_survey %>% 
    filter(EPU %in% epu,
           !str_detect(Var, "Other|Apex|managed"),
           Time >= 1968) %>% 
    group_by(EPU,Var) %>% 
    mutate(hline = mean(Value)) %>% 
    ungroup() %>% 
    mutate(Var = word(Var, 1,2))
  series.col <- rep("black",length(unique(total_surv$Var)))
  total_surv$Var <- factor(total_surv$Var,levels = c("Piscivore Fall",
                                                     "Piscivore Spring",
                                                     "Benthivore Fall",
                                                     "Benthivore Spring",
                                                     "Planktivore Fall",
                                                     "Planktivore Spring",
                                                     "Benthos Fall",
                                                     "Benthos Spring"))
  
  if (region == "NE"){
    gom_surv <- total_surv %>% 
      filter(EPU == "GOM") %>% 
      ggplot(aes(x = Time, y = Value, color = Var)) +
      #Add time series
      geom_line(size = lwd-0.5) +
      geom_point(size = pcex-0.5) +
      scale_color_manual(values = series.col, aesthetics = "color")+
      guides(color = FALSE) +
      geom_hline(aes(yintercept = hline,
                     group = Var),
                 size = hline.size,
                 alpha = hline.alpha,
                 linetype = hline.lty)+
      annotate("rect", fill = shade.fill, alpha = shade.alpha,
               xmin = x.shade.min , xmax = x.shade.max,
               ymin = -Inf, ymax = Inf) +
      geom_gls(aes(x = Time, y = Value,
                   color = Var),
               alpha = trend.alpha, size = trend.size) +
      facet_wrap(Var~.,scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
      ylab(expression("Biomass (kg tow"^-1*")")) +
      ggtitle("GOM NEFSC BTS") +
      theme_facet()+
      theme(strip.text=element_text(hjust=0, size = 12),
            plot.title = element_text(size = 16),
            axis.title = element_text(size = 14))
    gb_surv <- total_surv %>% 
      filter(EPU == "GB") %>% 
      ggplot(aes(x = Time, y = Value, color = Var)) +
      #Add time series
      geom_line(size = lwd-0.5) +
      geom_point(size = pcex-0.5) +
      scale_color_manual(values = series.col, aesthetics = "color")+
      guides(color = FALSE) +
      geom_hline(aes(yintercept = hline,
                     group = Var),
                 size = hline.size,
                 alpha = hline.alpha,
                 linetype = hline.lty)+
      annotate("rect", fill = shade.fill, alpha = shade.alpha,
               xmin = x.shade.min , xmax = x.shade.max,
               ymin = -Inf, ymax = Inf) +
      geom_gls(aes(x = Time, y = Value,
                   color = Var),
               alpha = trend.alpha, size = trend.size) +
      facet_wrap(Var~.,scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
      ylab(expression("Biomass (kg tow"^-1*")")) +
      ggtitle("GB NEFSC BTS") +
      theme_facet()+
      theme(strip.text=element_text(hjust=0, size = 12),
            plot.title = element_text(size = 16),
            axis.title = element_text(size = 14))
    return(list(gom_surv, gb_surv))
  } else {
    mab_surv <- ggplot(data = total_surv, aes(x = Time, y = Value, color = Var)) +
      annotate("rect", fill = shade.fill, alpha = shade.alpha,
               xmin = x.shade.min , xmax = x.shade.max ,
               ymin = -Inf, ymax = Inf) +
      geom_gls(aes(x = Time, y = Value,
                   color = Var),
               alpha = trend.alpha, size = trend.size) +
      geom_line(size = lwd-0.5) +
      geom_point(size = pcex-0.5) +
      scale_color_manual(values = series.col, aesthetics = "color")+
      guides(color = FALSE) +
      geom_hline(aes(yintercept = hline,
                     group = Var),
                 size = hline.size,
                 alpha = hline.alpha,
                 linetype = hline.lty)+
      geom_line(data = neamap, aes(x = Time, y = Value, color = Var),
                size = lwd-0.5,
                color = "#ca0020")+
      geom_point(data = neamap, aes(x = Time, y = Value, color = Var),
                 size = pcex-0.5,
                 color = "#ca0020")+
      facet_wrap(Var~.,scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
      ylab(expression("Biomass (kg tow"^-1*")")) +
      theme_facet()+
      theme(strip.text=element_text(hjust=0, size = 12),
            plot.title = element_text(size = 16),
            axis.title = element_text(size = 14))
    return(mab_surv)
  }
  
}