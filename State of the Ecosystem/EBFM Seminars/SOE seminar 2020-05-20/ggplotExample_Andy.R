# facet plot to show time series data and fit for multiple time series
#
# Data are labelled X1, X2, X3, x4
# There are two fits for each data variable
# X1fitL, X1fitS, etc.

library(ggplot2)
library(magrittr)

timeSeriesFit <- readRDS("fits_Andy.RDS")
timeSeriesData <- readRDS("data_Andy.RDS")

#filter fits
fitL <- timeSeriesFit %>% dplyr::filter(Vars %in% c("X1fitL", "X2fitL", "X3fitL", "X4fitL"))
fitS <- timeSeriesFit %>% dplyr::filter(Vars %in% c("X1fitS", "X2fitS", "X3fitS", "X4fitS"))
 

g <- ggplot(data = timeSeriesData) + 
  geom_line(aes(x=time,y=Vals)) +
  facet_wrap(~Vars)  +
  geom_line(aes(x=fitL$time, y=fitL$Vals), color = "blue") +
  facet_wrap(~fits1$Vars) +
  geom_line(aes(x=fitS$time, y=fitS$Vals), color = "red") +
  facet_wrap(~fitS$Vars, scales="free_y",labeller = ggplot2::as_labeller(c("X1fitS" = "X1",
                                                                            "X2fitS" = "X2",
                                                                            "X3fitS" = "X3",
                                                                            "X4fitS" = "X4")))


print(g)
