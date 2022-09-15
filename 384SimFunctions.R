#Function to generate random regression data

sim <- function(n=20, nsim=1, beta=c(1,2), x.mu = 1, x.sd = 2, eps.mu=0, eps.sd = 2 ){
  rslt <- matrix(NA,nrow=nsim,ncol=4)
  colnames(rslt) <- c("b0", "b1", "b0.se","b1.se")
  
  x <- rnorm(n*nsim, mean = x.mu, sd = x.sd)
  f <- cbind(1,x) %*% beta
  if(is.numeric(eps.sd)==T){
    eps <- rnorm(n*nsim, mean = eps.mu, sd = eps.sd)
  }else{
    s <- function(f) eval(call(eps.sd,f))
    eps <- rnorm(n*nsim, mean = eps.mu, sd = s(f))
  }
  
  y <- f + eps
  
  x <- matrix(x, nrow=n)
  eps <- matrix(eps, nrow=n)
  y <- matrix(y, nrow=n)
  
  
  for(i in 1:nsim){
    lmfit <- lm( y[,i]~ x[,i])
    lmfit.sum <- summary(lmfit)
    rslt[i,] <- c(lmfit.sum$coef[,c(1,2)])
  }
  
  return(list("y" = y, "x" = x, "eps" = eps, "result" = rslt))
}
