library(tidyverse)

#Specify parameters 
#Vary some parameters (# of indivuduals or connectedness)
#Need a break, if this isn't going to work, skip it (if some to infect = 0, then skip time step)
#see where it gets stuck, if ToInfect, (sum=0, __)


NID <- 100

TransmissionProbability <- 0.25


#for(i in 1:100){

t <- 0

#Run the initial loop

Population<- data.frame(ID= 1:NID, TimeStep = NA)

Population$I <- 0

#Random number selection/generation of n group 

(PatientZero <- sample(1:NID, 1, replace=T))

Population[PatientZero,]

#This individual is now infected
Population[PatientZero, "I"] <- 1
Population[PatientZero, "TimeStep"] <- t

(Matrix <- matrix(0, ncol = NID, nrow = NID))

#~5% of them would be full
Connections <- sample(1: (NID*NID), NID*NID/20)

Matrix[Connections] <- 1

Matrix[PatientZero,]

t <-1 

ToInfect <- rbinom(NID, 1, TransmissionProbability)

ToInfect <- ToInfect*Matrix[PatientZero,]

WhichtoInfect <- which(ToInfect == 1)

Population[WhichtoInfect, "I"] <- 1
Population[WhichtoInfect, "TimeStep"] <- t

# Population %>% filter(TimeStep== 0)

#Defining infected

for(t in 2:100){
  print(t)
  Infected <- which(Population$I ==1)
  
  Matrix[Infected,] 
  
  ToInfect <- Matrix[Infected,] %>% colSums
  
  
  ToInfect[ToInfect>1] <- 1
  ToInfect <- ToInfect*rbinom(NID, 1, TransmissionProbability)
  
  WhichtoInfect <- which(ToInfect == 1)
  WhichtoInfect <- WhichtoInfect %>% setdiff(which(Population$I == 1))
  
  Population[WhichtoInfect, "I"] <- 1
  Population[WhichtoInfect, "TimeStep"] <- t
  
}

#i<- 1

#Population  %>%  saveRDS("Population1.rds")
Population  %>%  saveRDS(paste("Population1", i, ".rds", sep=""))

}

Matrix