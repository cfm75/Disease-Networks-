library(tidyverse); library(fs)

dir_create("Outputs")

# Specify parameters

NID <- 100

TransmissionProbability <- 0.05

for(i in 1:100){
  
  print(i)
  
  # Run the initial loop
  
  t <- 0
  
  Population <- data.frame(ID = 1:NID,
                           TimeStep = NA)
  
  Population$I <- 0
  
  (PatientZero <- 
      sample(1:NID, 1, replace = T))
  
  Population[PatientZero,]
  
  Population[PatientZero,"I"] <- 1
  Population[PatientZero,"TimeStep"] <- t
  
  (Matrix <- matrix(0, ncol = NID, nrow = NID))
  
  Connections <- sample(1:(NID*NID), NID*NID/20)
  
  Matrix[Connections] <- 1
  
  Matrix[PatientZero,]
  
  t <- 1
  
  ToInfect <- rbinom(NID, 1, TransmissionProbability)
  
  ToInfect <- ToInfect*Matrix[PatientZero,]
  
  WhichToInfect <- which(ToInfect == 1)
  
  Population[WhichToInfect, "I"] <- 1
  Population[WhichToInfect, "TimeStep"] <- t
  
  # Population %>% filter(TimeStep == 0)
  
  for(t in 2:100){
    
    print(t)
    
    Infected <- which(Population$I == 1)
    
    Matrix[Infected,]
    
    if(length(Infected) < 2){
      
      ToInfect <- Matrix[Infected,]
      
    }else{
      
      ToInfect <- Matrix[Infected,] %>% colSums
      
    }
    
    ToInfect[ToInfect>1] <- 1
    
    ToInfect <- ToInfect*rbinom(NID, 1, TransmissionProbability)
    
    WhichToInfect <- which(ToInfect == 1)
    
    WhichToInfect <- WhichToInfect %>% setdiff(which(Population$I == 1))
    
    Population[WhichToInfect, "I"] <- 1
    Population[WhichToInfect, "TimeStep"] <- t
    
  }
  
  # i <- 1
  
  # Population %>% saveRDS("FritzePopulation1.rds")
  # Population %>% saveRDS(paste0("FritzePopulation", i, ".rds"))
  Population %>% saveRDS(paste0("Outputs/FritzePopulation", "_S_I_", TransmissionProbability, "_", i, ".rds"))
  
}

Population %>%
  arrange(TimeStep) %>% 
  mutate_at("TimeStep", ~replace_na(.x, 100)) %>% 
  ggplot(aes(TimeStep, I)) + 
  geom_point()

Population %<>%
  arrange(TimeStep) %>% 
  mutate_at("TimeStep", ~replace_na(.x, 100)) %>% 
  mutate(N = 1:n())

Population %>% 
  ggplot(aes(TimeStep, N)) + 
  geom_line()

library(magrittr)

Population %<>% 
  mutate_at("N", ~ifelse(I == 0, 97, .x))

Population %>% 
  ggplot(aes(TimeStep, N)) + 
  geom_point(aes(colour = as.factor(I))) + 
  scale_x_continuous(limits = c(0, 100))