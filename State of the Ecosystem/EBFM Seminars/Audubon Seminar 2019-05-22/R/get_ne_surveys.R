get_ne_surveys <- function(){
  facet_names <- list("Piscivores" = expression("Piscivores"),
                      "Planktivores" = expression("Planktivores"),
                      "Benthivores" = expression("Benthivores"),
                      "Benthos" = expression("Benthos"))
  
  
  total_surv <- nefsc_survey %>% 
    filter(EPU %in% c("GOM","GB"),
           !str_detect(Var, "Other|Apex|managed"),
           Time >= 1968) %>% 
    group_by(EPU,Var) %>% 
    mutate(hline = mean(Value, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(Var = word(Var, 1,2)) %>% 
    mutate(Survey = "NEFSC") %>% 
    as.data.frame()
  series.col <- rep("black",length(unique(total_surv$Var)))
  total_surv$Var <- factor(total_surv$Var,levels = c("Piscivore Fall",
                                                     "Piscivore Spring",
                                                     "Benthivore Fall",
                                                     "Benthivore Spring",
                                                     "Planktivore Fall",
                                                     "Planktivore Spring",
                                                     "Benthos Fall",
                                                     "Benthos Spring"))
  
  mass <- mass_inshore_survey %>% 
    filter(str_detect(Var, "Index")) %>% 
    mutate(feeding.guild = str_extract(Var, paste(feeding.guilds, collapse = "|")),
           Season = str_extract(Var, "Spring|Fall")) %>%
    filter(!is.na(feeding.guild)) %>% 
    unite(.,Var, c("feeding.guild","Season"),sep = " ") %>% 
    group_by(Var) %>% 
    mutate(hline = mean(Value)) %>% 
    mutate(Survey = "MA") %>% 
    as.data.frame()
  mass$Var <- factor(mass$Var,levels = c("Piscivore Fall",
                                         "Piscivore Spring",
                                         "Benthivore Fall",
                                         "Benthivore Spring",
                                         "Planktivore Fall",
                                         "Planktivore Spring",
                                         "Benthos Fall",
                                         "Benthos Spring"))
  
  menh <- ecodata::ne_inshore_survey %>% 
    filter(Units != "CV",
           !str_detect(Var, " se| ci")) %>% 
    mutate(feeding.guild = str_to_title(str_extract(Var, paste(tolower(feeding.guilds), collapse = "|"))),
           Season = str_to_title(str_extract(Var, "fall|spring"))) %>% 
    unite(.,Var, c("feeding.guild","Season"),sep = " ") %>% 
    group_by(Var) %>% 
    mutate(hline = mean(Value)) %>% 
    mutate(Survey = "MENH") %>% 
    as.data.frame()
  menh$Var <- factor(menh$Var,levels = c("Piscivore Fall",
                                         "Piscivore Spring",
                                         "Benthivore Fall",
                                         "Benthivore Spring",
                                         "Planktivore Fall",
                                         "Planktivore Spring",
                                         "Benthos Fall",
                                         "Benthos Spring"))
  
  gom_surv <- total_surv %>% 
    filter(EPU == "GOM") %>% 
    ggplot(aes(x = Time, y = Value, color = Var)) +
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
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
    facet_wrap(Var~.,scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
    ylab(expression("Biomass (kg tow"^-1*")")) +
    ggtitle("GOM NEFSC BTS") +
    theme_facet()+
    theme(strip.text=element_text(hjust=0,
                                  size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          title = element_text(size = 14))
  
  gb_surv <- total_surv %>% 
    filter(EPU == "GB") %>% 
    ggplot(aes(x = Time, y = Value, color = Var)) +
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max,
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
    facet_wrap(Var~.,scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
    ylab(expression("Biomass (kg tow"^-1*")")) +
    ggtitle("GB NEFSC BTS") +
    theme_facet()+
    theme(strip.text=element_text(hjust=0,
                                  size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          title = element_text(size = 14))
  
  
  mass_plot <- mass %>% 
    ggplot(aes(x = Time, y = Value, color = Var)) +
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
    facet_wrap(Var~.,scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
    ylab(expression("Biomass (kg tow"^-1*")")) +
    ggtitle("MA Inshore BTS") +
    theme_facet()+
    theme(strip.text=element_text(hjust=0,
                                  size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          title = element_text(size = 14))
  
  
  menh_plot <- menh %>% 
    ggplot(aes(x = Time, y = Value, color = Var)) +
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
             xmin = x.shade.min , xmax = x.shade.max ,
             ymin = -Inf, ymax = Inf) +
    
    geom_line(size = lwd-0.5) +
    geom_point(size = pcex-0.5) +
    scale_color_manual(values = series.col, aesthetics = "color")+
    guides(color = FALSE) +
    geom_hline(aes(yintercept = hline,
                   group = Var),
               size = hline.size,
               alpha = hline.alpha,
               linetype = hline.lty)+
    facet_wrap(Var~.,scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
    ylab(expression("Biomass (kg tow"^-1*")")) +
    ggtitle("ME/NH Inshore BTS") +
    theme_facet()+
    theme(strip.text=element_text(hjust=0,
                                  size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          title = element_text(size = 14))
  # 
  # benthivore_NE <- rbind(menh, mass, total_surv) %>% 
  #   dplyr::filter(str_detect(Var,"Benthivore")) %>% 
  #   mutate(group = paste(EPU, Var, Survey)) %>% 
  #   mutate(group = plyr::mapvalues(group, from = c("NE Benthivore Fall MENH","NE Benthivore Spring MENH",
  #                                                  "GOM Benthivore Fall MA","GOM Benthivore Spring MA",
  #                                                  "GOM Benthivore Fall NEFSC","GOM Benthivore Spring NEFSC",
  #                                                  "GB Benthivore Fall NEFSC","GB Benthivore Spring NEFSC"),
  #                                  to = c("ME/NH Fall","ME/NH Spring",
  #                                         "MA Fall","MA Spring",
  #                                         "GOM NEFSC Fall","GOM NEFSC BTS Spring",
  #                                         "GB NEFSC Fall","GB NEFSC Spring")))
  # benthivore_NE$group <- factor(benthivore_NE$group,
  #                               levels = c("ME/NH Fall","ME/NH Spring",
  #                                          "MA Fall","MA Spring",
  #                                          "GOM NEFSC Fall","GOM NEFSC BTS Spring",
  #                                          "GB NEFSC Fall","GB NEFSC Spring"))
  
  return(list(menh_plot, mass_plot, gom_surv, gb_surv))
}
