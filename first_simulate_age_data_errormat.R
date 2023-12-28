# Load in matrix

# How can I simulate data frame from this...
# sample over possible ages using a string of probabilities from puntilizer

# test
library(tidyverse)

agemat <- matrix(data = c(0.999988,	1.15E-05,	0,	0,	0,
                          1.15E-05,	0.999977,	1.15E-05,	0,	0,
                          2.30E-10,	0.0161425,	0.967715,	0.0161425,	2.30E-10,
                          3.18E-12,	1.15E-05,	0.0763062,	0.847364,	0.0763062,
                          3.36E-13,	7.74E-08,	0.00069669,	0.140823,	0.71696
                          
                          
), nrow = 5, byrow = T)

Iter <- 5


sim <- matrix(, nrow = Iter, ncol = max(df$age))

for (i in 1:max(df$age)) {
  sim[,i] <- sample(x = c(1:5),
                    size = Iter,
                    prob = agemat[i,],
                    replace = TRUE)
}


# FULL

# All readers combined for 2013-2018 (to match spectra) was B2_S3_S1. This is Reader 1 bias 2, and curvilinear CV (3 parameter hotellings)

#Columns are True age, rows are age estimate

## Load data - all readers combined into "reader 1"
agemat <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Puntilizer/Model comparisons all data 2013-2018/B2_S3_S1/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

model_error <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Puntilizer/Model comparisons all data 2013-2018/B2_S3_S1/cv and bias Reader 1.csv")

## Plot CV and Bias
ggplot(model_error)+
  geom_line(aes(Age, Expected),color = "blue")+
  geom_ribbon(aes(x = Age, ymin = Expected - SD*2, ymax = Expected + SD*2), fill = "blue", alpha = .2)+
  geom_abline(slope = 1, intercept = 0, linetype = 2)+
  scale_x_continuous(limits = c(0,16), breaks=seq(0,16,2), expand = c(0,0))+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,16,2))+
  theme_classic()

# Simulate data
Iter <- 100

known_age <- rep(0:14, each = Iter) #make vector of known ages

sim <- matrix( , nrow = Iter, ncol = 15) #initiate matrix for simulation

set.seed(13)
for (i in 1:15) { #each loop iterates through agemat rows for ages 0:20 and samples from numbers 0-14 based on probabilities
  sim[,i] <- sample(x = c(0:14), #drawing from numbers 0-20
                      size = Iter, #can set sample size per iteration/age
                      prob = agemat[i,], #iterate through rows of probabilities for age 0 - 14
                      replace = TRUE)
}

error_age <- c(sim) #concatonate matrix into vector

sim_data <- data.frame(cbind(known_age, error_age)) #join "known_age" and simulated "error_age" 

# Visualize
ggplot(sim_data)+
  geom_point(aes(known_age, error_age), alpha = .2)+
  geom_abline(slope = 1, intercept = 0)

# Figure
ggplot(sim_data)+
  geom_point(aes(x = known_age, y = error_age), color = "grey40", alpha = .2, position = position_jitter(w = 0.1, h = 0))+
  scale_x_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  #geom_smooth(aes(x = known_age, y = error_age),method = "lm")+
  geom_abline(slope = 1, intercept = 0)+
  geom_line(data = model_error, aes(Age, Expected),color = "blue")+
  geom_ribbon(data = model_error, aes(x = Age, ymin = Expected - SD*2, ymax = Expected + SD*2), fill = "blue", alpha = .3)+
  labs(x = "True age",
       y = "Simulated age estimate",
       subtitle = "Pacific cod simulated age data")+
  theme_classic()

# Save data 
write.csv(x = sim_data, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Data/simulated age data mat.csv")
