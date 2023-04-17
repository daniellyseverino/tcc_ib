# This function creates a data frame from the run-off triangle matrix to be used in INLA
make.df.trian <- function(M){
  Time <- nrow(M)
  Delay <- ncol(M)
  aux.df <- data.frame(Y = as.vector(as.matrix(M)), 
                       Time = rep(x = 1:Time, times = Delay),
                       Delay = rep(x = 0:(Delay-1), each=Time)
  )
  aux.df
}

# Function for plotting INLA random effects
plot.inla.re = function(outputRE, x = outputRE$ID){
  plot( x, y = outputRE$mean, type = "n", ylim = range(outputRE[,c(4,6)]), ylab="", xlab="" )
  polygon(x = c(x, rev(x)),
          y = c(outputRE$'0.025quant', rev(outputRE$'0.975quant')),
          border = "black", col = "gray")
  lines(x, outputRE$mean, lty=1, lwd=2)
  lines(x = range(x), y = rep(0,2), lty=2)
}

# Priori para INLA
half_normal_sd <- function(sigma){
  return(
    paste("expression:
              sigma = ",sigma,";
              precision = exp(log_precision);
              logdens = -0.5*log(2*pi*sigma^2)-1.5*log_precision-1/(2*precision*sigma^2);
              log_jacobian = log_precision;
              return(logdens+log_jacobian);",sep='')
  )
} 
