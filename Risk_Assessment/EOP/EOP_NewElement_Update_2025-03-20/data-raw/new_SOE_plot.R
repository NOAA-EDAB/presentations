#Plot figure new
soe.plot <- function(data, x.var, y.var, x.label = '', y.label = '', tol = 0.1, 
                     x.start = NA, end.start = 2006, bg.col = background, 
                     end.col = recent, trend.neg = main.neg, trend.pos = main.pos, 
                     end.trend.neg = end.neg, end.trend.pos = end.pos, 
                     stacked = NA, x.line = 2.5, y.line = 3.5, scale.axis = 1){
  
  #Select Data
  x <- data[Var == y.var, ]
  x <- x[order(x[, get(x.var)]), ]
  setnames(x, x.var, 'X')
  
  #Set common time step if necessary
  if(is.na(x.start)) x.start <- min(x[, X])
  x <- x[X >= x.start, ]
  
  #Set up plot parameters
  y.max <- max(x[, Value]) + tol * max(x[, Value])
  y.min <- min(x[, Value]) - tol * abs(min(x[, Value]))
  y.mean <- mean(x[, Value])
  y.sd <- sd(x[, Value])
  
  #Plot blank plot
  plot(x[X >= x.start, list(X, Var)], ylim = c(y.min, y.max), xlab = '', ylab = '', 
       axes = F, ty = 'n')
  
  #Add background
  u <- par('usr')
  rect(u[1], u[3], u[2], u[4], border = NA, col = bg.col)
  
  #Add end period shading
  rect(start - 0.5, u[3], u[2], u[4], border = NA, col = end.col)
  
  #Add grides
  abline(h = y.mean + y.sd, col = 'white', lwd = 3, lty = 2)
  abline(h = y.mean - y.sd, col = 'white', lwd = 3, lty = 2)
  
  #Add data points/lines
  points(x[, list(X, Value)], pch = 16, cex = 1.5)
  lines( x[, list(X, Value)], lwd = 2)
  
  #Add axis
  if(is.na(stacked)) axis(1, cex.axis = 1.5)
  if(!is.na(stacked)){
    if(stacked!= 'A') axis(3, cex.axis = 1.5, tck = 0.1, labels = F)
  } 
  axis(2, at = axTicks(2), labels = axTicks(2) / scale.axis, cex.axis = 1.5, las = T)

  #Identify significant trends
  #Whole time series
  mksts <- MannKendall(x[, Value])
  mkstsp <- round(unlist(mksts[2]), 3)
  
  if(mkstsp < 0.05){
    lmod <- lm(x[, Value] ~ x[, X])
    lmod_c <- unlist(lmod[1])
    lmod_i <- lmod_c[1]
    lmod_s <- lmod_c[2]
    if(lmod_s > 0){
      lines(x[, X], lmod_s * x[, X] + lmod_i, 
          col = trend.pos, lty = 1, lwd = 7)
    }
    if(lmod_s < 0){
      lines(x[, X], lmod_s * x[, X] + lmod_i, 
            col = trend.neg, lty = 1, lwd = 7)
    }
  }
  
  #Final portion of time series
  mksld <- MannKendall(x[X > (start - 1), Value])
  mksldp <- round(unlist(mksld[2]), 3)
  if(mksldp < 0.05){
    l10_x <- x[X > (start - 1), X]
    l10_y <- x[X > (start - 1), Value]
    lmod <- lm(l10_y ~ l10_x)
    lmod_c <- unlist(lmod[1])
    lmod_i <- lmod_c[1]
    lmod_s <- lmod_c[2]
    if(lmod_s > 0){
      lines(l10_x, lmod_s * l10_x + lmod_i, 
          col = end.trend.pos, lwd=7)
    }
    if(lmod_s < 0){
      lines(l10_x, lmod_s * l10_x + lmod_i, 
            col = end.trend.neg, lwd=7)
    }
  }
  
  #Add axis labels
  if(!is.na(stacked)) text(u[1], u[4], labels = stacked, cex = 2, adj = c(-0.5, 1.5))
  if(is.na(stacked)){
    mtext(1, text = x.label, line = x.line, cex = 1.5)
    mtext(2, text = y.label, line = y.line, cex = 1.5)
  }
}

#Add axis labels for stacked plots
soe.stacked.axis <- function(x.label, y.label){
  axis(1, cex.axis = 1.5)
  mtext(1, text = x.label, line = 2.5, cex = 1.5, outer = T)
  mtext(2, text = y.label, line = 3.5, cex = 1.5, outer = T)
}



background   <- '#F4F7F2'
recent       <- '#E6E6E6'
#trend lines
main.pos <- rgb(253/255, 184/255, 99/255,  alpha = 0.8)
main.neg <- rgb(178/255, 171/255, 210/255, alpha = 0.8)
end.pos  <- rgb(230/255, 97/255,  1/255,   alpha = 0.8)
end.neg  <- rgb(94/255,  60/255,  153/255, alpha = 0.8)


#Example with new features
soe.plot(SOE.data, 'Time', 'MAB Benthos Landings', x.start = 1982, scale.axis = 10^3,
         x.label = 'Year', y.label = expression('Revenue, 10'^3*' $US'))
