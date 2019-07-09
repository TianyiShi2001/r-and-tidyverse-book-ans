
Rmklike <- function(data) { 
  n = length(data)  
  sumx = sum(data) 
  lfun = function(mu) n * log(mu) - mu * + sumx 
  score = function(mu) n/mu - sumx 
  d2 = function(mu) -n/mu^2 
  list(lfun = lfun, score = score, d2 = d2) 
}



newton <- function(lfun, est, tol = 1e-07, niter = 500) {
  cscore = lfun$score(est)
  if (abs(cscore) < tol){
    return(est)
  }
  for (i in 1:niter) {
    new = est - cscore/lfun$d2(est)
    cscore = 
  }
}

lfun <- Rmklike(rexp(10000))
curve(lfun$score(x), 0,5)
abline(h = 0, v = 1)
