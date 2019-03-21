feeding_guild_landings <- function(epu, council,filt = F){
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
  #Assign feeding guild column for plotting with ggplot
  landings <- rbind(managed_landings, total_landings) %>%
    mutate(feeding.guild = str_extract(Var,paste(feeding.guilds, collapse = "|")),
           grouping = factor(ifelse(str_detect(Var,council), "managed",
                                    ifelse(str_detect(Var, "JOINT"), "joint","total"))),
           Var = paste(word(feeding.guild), grouping)) %>% 
    mutate(feeding.guild = factor(feeding.guild, levels = feeding.guilds))
  
  #Add JOINT landings to MANAGED landings and remove
  landings[landings$Var ==  "Piscivore managed",]$Value <- landings[landings$Var ==  "Piscivore managed",]$Value + landings[landings$Var ==  "Piscivore joint",]$Value
  landings <- landings %>%
    filter(Var != "Piscivore joint")  %>% 
    group_by(Var) %>% 
    mutate(hline = mean(Value))
  
  #Define constants for figure plot
  x.shade.max <- 2018
  x.shade.min <- 2009
  series.col <- c("indianred","black")
  
  if (filt){
    landings <- landings %>% filter(feeding.guild == "Planktivore")
  }
  
  out_landings <- ggplot(data = landings,aes(x = Time, y = Value, color = grouping)) +
    
    #Highlight last ten years
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
             ymin = -Inf, ymax = Inf) +
    
    #Test for trend and add lines
    geom_gls(aes(x = Time, y = Value,
                 group = Var)) +
    
    #Add time series
    geom_line(size = lwd) +
    geom_point(size = pcex) +
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    geom_hline(aes(yintercept = hline,
                   color = grouping,
                   size = grouping),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty)+
    
    #Facet 
    facet_wrap(feeding.guild~.,scales = "free_y", labeller = label, ncol = 1) +
    
    #Axis and theme
    scale_y_continuous(labels = function(l){trans = l / 1000})+
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
    ylab(expression("Landings, 10"^3*"metric tons")) +
    ggtitle(epu) +
    theme_facet() +
    theme(strip.text=element_text(hjust=0, size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"))
  return(out_landings)
}