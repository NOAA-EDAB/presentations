# plot biomass time series facet wrapped by species
plotB <- function(dat, truedat=NULL){
  
  svbio <- dat %>% filter(variable=="biomass")
  svcv <- dat %>% filter(variable=="cv")
  
  ggplot() +
    geom_line(data=svbio, aes(x=year,y=value, color="Survey Biomass"), 
              alpha = 10/10) +
    {if(!is.null(truedat)) geom_line(data=truedat, aes(x=time/365,y=atoutput, color="True B"), alpha = 3/10)} + 
    theme_tufte() +
    theme(legend.position = "top") +
    xlab("model year") +
    ylab("tons") +
    labs(colour=dat$ModSim) +
    facet_wrap(~Name, scales="free") 
  
}

# make a catch series function that can be split by fleet? this doesnt
# also note different time (days) from model timestep in all other output
plotC <- function(dat, truedat=NULL){
  
  ctbio <- dat %>% filter(variable=="catch")
  ctcv <- dat %>% filter(variable=="cv")
  
  ggplot() +
    geom_line(data=ctbio, aes(x=year,y=value, color="Catch biomass"), 
              alpha = 10/10) +
    {if(!is.null(truedat)) geom_line(data=truedat, aes(x=time/365,y=atoutput, color="True Catch"), alpha = 3/10)} + 
    theme_tufte() +
    theme(legend.position = "top") +
    xlab("model year") +
    ylab("tons") +
    labs(colour=dat$ModSim) +
    facet_wrap(~Name, scales="free") 
  
}

# note on ggplot default colors, can get the first and second using this
# library(scales)
# show_col(hue_pal()(2))

# plot length frequencies by timestep (one species)
plotlen <- function(dat, effN=1, truedat=NULL){
  
  cols <- c("Census Lcomp"="#00BFC4","Sample Lcomp"="#F8766D")  
  ggplot(mapping=aes(x=lenbin)) +
    {if(is.null(truedat)) geom_bar(data=dat, aes(weight = value/effN))} +
    {if(!is.null(truedat)) geom_bar(data=dat, aes(weight = censuslen/totlen, fill="Census Lcomp"), alpha = 5/10)} +
    {if(!is.null(truedat)) geom_bar(data=dat, aes(weight = atoutput/effN, fill="Sample Lcomp"), alpha = 5/10)} +
    theme_tufte() +
    theme(legend.position = "bottom") +
    xlab("length (cm)") +
    {if(is.null(truedat)) ylab("number")} +
    {if(!is.null(truedat)) ylab("proportion")} +
    scale_colour_manual(name="", values=cols) +
    labs(subtitle = paste(dat$ModSim,
                          dat$Name)) +
    facet_wrap(~year, ncol=6, scales="free_y")
  
}

# plot numbers at age by timestep (one species)
Natageplot <- function(dat, effN=1, truedat=NULL){
  ggplot() +
    geom_point(data=dat, aes(x=age, y=value/effN, color="Est Comp")) +
    {if(!is.null(truedat)) geom_line(data=dat, aes(x=agecl, y=numAtAge/totN, color="True Comp"))} + 
    theme_tufte() +
    theme(legend.position = "bottom") +    
    xlab("age") +
    {if(is.null(truedat)) ylab("number")} +
    {if(!is.null(truedat)) ylab("proportion")} +
    labs(subtitle = paste(dat$ModSim,
                          dat$Name)) + 
    facet_wrap(~year, ncol=6, scales="free_y")
}

# plot weight at age time series facet wrapped by species
wageplot <- function(dat, truedat=NULL){
  ggplot(dat, aes(year, value)) +
    geom_line(aes(colour = factor(age))) +
    theme_tufte() +
    theme(legend.position = "bottom") +
    xlab("model year") +
    ylab("average individual weight (g)") +
    labs(subtitle = paste0(dat$ModSim)) +
    facet_wrap(c("Name"), scales="free_y")
}
