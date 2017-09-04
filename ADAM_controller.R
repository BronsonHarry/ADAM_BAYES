rm(list=ls()) # Clear workspace
source("ADAM_condition.r")
source("ADAM_tap.r")
source("ADAM_trial.r")

# Set the path
path<-"~/Work/Experiments/bayesADAM/data/"

# set the condition names
condition_names <- c("virtualPartner","tempoChange")

# number of participants
participant_num <- 20

# set test parameter values
theta <- c(0.1,0.2,0.3,0.4,10)

# initialise loglikelihood 
lld_all <- array(dim = c(participant_num,length(condition_names)))

#cC<-1
#for each condition
for (cC in 1:length(condition_names)) {
  
  sC <- condition_names[cC]
  
#cP<-12
  # and for each participant
  for (cP in 1:participant_num) {
    if (cP < 10) {
      sP <- paste("0",toString(cP),sep = "")
      } else {
        sP <- toString(cP)
      }
      
    lld_condition <- ADAM_condition(participant=sP,condition=sC,path=path,theta=theta)
    
    lld_all[cP,cC] <- mean(lld_condition)
      
      }
   }


