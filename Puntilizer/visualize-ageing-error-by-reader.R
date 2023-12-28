# Visualize double reads for each reader independently using double read data from 2010-2019

## Still unfinished, need to split out by reader

### To visualize double read data by reader. One thing to keep in mind is at the moment, these are all calculated based on data from 2010-2019, but only using spectra from 2013. Might be a mismatch.

# Load in ageing error matrices for each reader in 2013
# Reader 3 - B1_S2
agemat_3 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 3 tester 6 2010-2019/B1_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 8 - B2_S2
agemat_8 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 8 tester 6 2010-2019/B2_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 21 - B2_S1_S3
agemat_21 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 21 tester 6 2010-2019/B2_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 59 - B0_S1_S3
agemat_59 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 59 tester 6 2010-2019/B0_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 71 - B2_S1 
agemat_71 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 71 tester 6 2010-2019/B2_S1/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

Iter <- 10

known_age <- rep(0:14, each = Iter) #make vector of known ages

sim_3 <- matrix( , nrow = Iter, ncol = 15) #initiate matrix for simulation

set.seed(13)
for (i in 1:15) { #each loop iterates through agemat rows for ages 0:20 and samples from numbers 0-20 based on probabilities
  sim_3[,i] <- sample(x = c(0:14), #drawing from numbers 0-20
                      size = Iter, #can set sample size per iteration/age
                      prob = agemat_3[i,], #iterate through rows of probabilities for age 0 - 20
                      replace = TRUE)
}

error_age <- c(sim_3) #concatonate matrix into vector

sim_3_data <- data.frame(cbind(known_age, error_age)) #join "known_age" and simulated "error_age" 

# Visualize

ggplot(sim_3_data)+
  geom_point(aes(known_age, error_age), alpha = .2)+
  geom_abline(slope = 1, intercept = 0)

# Figure
ggplot(sim_71_data)+
  geom_point(aes(x = known_age, y = error_age), color = "grey40", alpha = .5)+
  scale_x_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  #geom_smooth(aes(x = known_age, y = error_age),method = "lm")+
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "True age",
       y = "Simulated age estimate",
       subtitle = "Pacific cod simulated age data - reader index 71")+
  theme_classic()