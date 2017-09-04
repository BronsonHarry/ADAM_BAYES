ADAM_condition <- function(participant,condition,path,theta) {
  
  ## get the files
  allFiles <- list.files(path = path, pattern = paste("^sub-",participant,"_data_",condition,"(.*)","csv$",sep = ""))
  
  lld_trial <- array(dim = c(length(allFiles)))
  for (cF in 1:length(allFiles)) {
    
    #read the data, set the stimulus and response onsets
    data <- read.csv(paste(path,allFiles[cF],sep=""),header=FALSE)
    S <- data$V1
    S <- S * 1000
    R <- S + data$V2
    
    # Call the _trial function
    lld_trial[cF] <- ADAM_trial(R=R,S=S,theta=theta)
    
  }
  
  return(lld_trial)
  
}