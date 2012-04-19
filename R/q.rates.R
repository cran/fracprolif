# See Powell, 1956
q.rates <- function(dist, est)
{
  est <- as.list(coef(est))
  Q   <- est$Q
  est$Q <- NULL
  
  g <- function(t, v)  { params <- est;
                         params$x <- t;
                         exp(-v * t) *
                           do.call(paste("d",dist, sep=''), params) }
  f <- function(v) {integrate(g, 0, Inf, v)$value - 0.5}
       # May need to adjust max value of integration range from Inf to 100
  v <- uniroot(f, lower=0.0001, upper=1)$root
  
  # Next compute quiescence rate
  v <- append(v, Q*v/(1-Q))
  names(v) <- c("d", "q")
  v
}
