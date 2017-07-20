MonteCarlo <- function(n, f){
  X <- runif(n)
  Y <- f(X)
  Intgr <- sum(Y)/n
  Error <- Intgr-(pnorm(1)-pnorm(0))
  list("Result=" = Intgr,"Error=" = Error)
}

t2 <- function(x){ (1/sqrt(2*pi))^(((x-2)^2)/2)}

t3 <- function(x) { exp(x / sqrt(2*pi)*3)^(((x-2)^2)/-2) }