ADAM_tap <- function(Rs,Ss,t,theta) {
  
  
  # alpha theta[1]  phase correction
  # beta  theta[2]  period correction
  # delta theta[3]  extrapolation/tracking
  # gamma theta[4]  anticipatory error correction
  # TKn   theta[5]  timekeeper noise
  
  
  # get stimulus intervals
  s <- Ss[2:3] - Ss[1:2]
  
  # Exectue action, calculate asynchrony
  e <- Rs[1] - Ss[3] #note Rs[1] and Ss[3] correspond to the same interval
  
  ## ADAPTATION MODULE (reactive error correction, asynchrony based)
  
  # Update timekeeper, apply period correction
  t <- t - theta[2] * e 
  
  # Plan response onset time, apply phase correction, add timekeeper noise
  R_prime <- Rs[1] + t - theta[3] * e
  
  ## ANTICIPATION MODULE (prediction of stimulus timing, based on stimulus intervals)
  
  # Extrapolate from recent inter-stimulus intervals
  p = 2 * s[2] - s[1]
  
  # Predict future stimulus onset from weighted mixture of extrapolation/tracking strategy
  # Strategy controlled by delta. Delta=1 pure extrapolation, Delta=0 pure tracking
  # timekeeper is assumed to be involved in anticipation, thus add timekeeper noise
  S_prime <- Ss[3] + theta[3]*(p) + (1-theta[3])*s[1]
  
  ## JOINT MODULE
  
  # Calculate predicted error
  e_prime <- R_prime - S_prime
  
  # Perform predicted error correction, update response onset, add motor noise
  R_pred <- R_prime - theta[4] * e_prime
  
  lld <- dnorm(Rs[2], mean=R_pred, sd=theta[5], log=TRUE)
  
  return(list(lld,t)) # return the likelihood and the current timekeeper setting
  # as this may have been altered by period correction
}

