ADAM_trial <- function(R,S,theta) {
  
  # initialise variables
  total_i <- length(S)
  
  # likelihood values
  lld <- rep(0,total_i)
  
  # set timekeeper
  t <- S[1]
  
  for (i in 3:(total_i-1)) {
    
    Rs <- R[seq(i,i+1)]
    Ss <- S[seq(i-2,i)]
    
    out <- ADAM_tap(Rs,Ss,t,theta)
    
    lld[i] <- as.numeric(out[1])
    
    t <- as.numeric(out[2])
    
  }
  
  return(sum(lld[4:(total_i-2)]))
  
}

