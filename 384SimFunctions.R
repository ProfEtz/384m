#Function to generate random regression data

sim <- function(n, nsim, beta=c(1,2), x.mu = 1, x.sd = 2, sps.mu=0, eps.sd = 1){
  rslt <- matrix(NA,nrow=nsim,ncol=4)
  colnames(rslt) <- c("b0", "b1", "b0.se","b1.se")
  y <- x <- matrix(NA, nrow=n, ncol = nsim)
  for(i in 1:nsim){
    x[,i]   <- rnorm(n, mean = x.mu, sd = x.sd)
    mu  <- cbind(1,x[,i]) %*% beta
    eps <- rnorm(n, mean = eps.mu, sd = eps.sd)
    y[,i]   <- mu + eps
    
    lmfit <- lm( y[,i]~ x[,i])
    lmfit.sum <- summary(lmfit)
    rslt[i,] <- c(lmfit.sum$coef[,c(1,2)])
  }
  
  return(list("y" = y, "x" = x, "result" = rslt))
}
