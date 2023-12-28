# Okay, starting out new analysis
# What I want to end up with is samp that has a list of different datasets, each dataset has known age, error age, and spectra with random error drawn. 

# Packages
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)
library(ggpubr)


# First, let's use data just from 2013
# No errors, and no instrument off-set issues

#######################
# Load in 2017 spectral data
# raw_dat <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Data/Spectra_2010-2018_n9427.csv")
# 
# meta_dat <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Data/MetaData_2010-2019_n14579.csv")
# 
# all_dat <- left_join(raw_dat, meta_dat, by = "Code")

# Try THIS INSTEAD

# Load in data - outputs from script 'explore-pollock-spectra-offset'
load("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/offset-dependency_files/raw_dat.rda")

load("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/offset-dependency_files/raw_dat_L.rda")

load("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/offset-dependency_files/dat_sg.rda")

#load("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Walleye Pollock/offset-dependency_files/sg_dat_L.rda")

# Filter all by spectra collected between 6/2021 and 5/2022
## Filter out all of 2022, and 2021 if month >= 6
dat_sg_rm <- dat_sg %>%
  filter(scan_year == "2021" & scan_month %in% c(6,7,8,9,10,11,12))

dat_sg_filt_og <- anti_join(dat_sg, dat_sg_rm)

dat_sg_rm <- dat_sg_filt_og%>%
  filter(instrument_name == "MPAII_AFSC" & scan_year == "2022" & scan_month %in% c(1,2,3,4,5,6))

dat_sg_filt_og <- anti_join(dat_sg_filt_og, dat_sg_rm)

summary(dat_sg_filt_og$scan_year)
summary(dat_sg_filt_og$collection_year)

dat_sg_filt_og <- filter(dat_sg_filt_og, final_age %in% c(1:10))

# PCA for outliers
# PCA
m1 = mdatools::pca(dat_sg_filt_og[,c(39:942)], 5, scale = F, center = F, info = "Pollock age PCA")

# Scree Plot
plotVariance(m1$res$cal, show.labels = TRUE)

# Plot Residuals
plotResiduals(m1, show.labels = TRUE, labels = "indices")

# Remove outlier
c <- categorize(m1)
index_outliers <- as.numeric(which(c == "outlier"))

dat_sg_filt <- dat_sg_filt_og[-c(index_outliers),]

# PCA
m1 = mdatools::pca(dat_sg_filt[,c(39:942)], 5, scale = F, center = F, info = "Pollock age PCA")

# Scree Plot
plotVariance(m1$res$cal, show.labels = TRUE)

# Plot Residuals
plotResiduals(m1, show.labels = TRUE, labels = "indices")

# Remove outlier
c <- categorize(m1)
index_outliers <- as.numeric(which(c == "outlier"))

dat_sg_filt <- dat_sg_filt[-c(index_outliers),]

# PCA
m1 = mdatools::pca(dat_sg_filt[,c(39:942)], 5, scale = F, center = F, info = "Pollock age PCA")

# Scree Plot
plotVariance(m1$res$cal, show.labels = TRUE)

# Plot Residuals
plotResiduals(m1, show.labels = TRUE, labels = "indices")

# Remove outlier
c <- categorize(m1)
#index_outliers <- as.numeric(which(c == "outlier"))

#dat_sg_filt <- dat_sg_filt[-c(index_outliers),]

# Identify outliers
outliers <- anti_join(dat_sg_filt_og, dat_sg_filt, by = NULL)

#Clean up
names(dat_sg_filt) <- sub('X','', names(dat_sg_filt)) #remove X in front of wavenumbers

dat_sg_rm <- dat_sg_filt[is.na(dat_sg_filt$final_age),] #any rows that don't have a final age (n = 7)

dat_sg_filt <- anti_join(dat_sg_filt, dat_sg_rm, by = NULL) #filter out rows with no final_age

# all_dat <- filter(all_dat, Year == "2013", Age %in% c(1:10)) #filter out just 2013

counts <- dat_sg_filt%>% #see how many specimens per age
  group_by(final_age)%>%
  summarize(n())

summary(dat_sg_filt$collection_year)

rm(dat_sg_filt_og)
rm(raw_dat)

# # Run PCA
# m <- pca(dat_sg_filt[,7:506], ncomp = 10, scale= F, center = F)
# 
# # Might need to do some outlier removal before getting better average
# plotResiduals(m, ncomp = 5, show.labels = T) #808, 1233
# # filter out outliers
# all_dat_or <- filter(all_dat[-c(808,1233),])
# 
# # refit PCA
# m2 <- pca(all_dat_or[,7:506], ncomp = 10, scale= F, center = F)
# 
# # Check outlier plot
# plotResiduals(m2, ncomp = 5, show.labels = T) #364
# 
# # Remove
# all_dat_or <- filter(all_dat_or[-c(947,963),])
# 
# #refit PCA
# m2 <- pca(all_dat_or[,7:505], ncomp = 10, scale= F, center = F)
# 
# # Check outlier plot
# plotResiduals(m2, ncomp = 5, show.labels = T) #808
# 
# # Remove
# all_dat_or <- filter(all_dat_or[-c(921,877,795,116),])
# 
# #refit PCA
# m2 <- pca(all_dat_or[,7:506], ncomp = 10, scale= F, center = F)
# 
# # Check outlier plot
# plotResiduals(m2, ncomp = 5, show.labels = T)

#######################
# Plot raw spectra - 4.76 x 3.50
## First need to pivot_longer to get column of absorbance
all_dat_L<-dat_sg_filt%>%tidyr::pivot_longer(.,cols=c(38:942),names_to="wavenumber",values_to="absorbance") 

# Clean up data
raw_dat_L$Age <- as.factor(raw_dat_L$final_age)
raw_dat_L <- filter(raw_dat_L, Age %in% c(1:10))

all_dat_L$Age <- as.factor(all_dat_L$final_age)
all_dat_L$wavenumber <- as.numeric(all_dat_L$wavenumber)

dat_sg_filt$Age <- dat_sg_filt$final_age

# Look at raw spectra
ggplotly(ggplot(raw_dat_L[seq(1,length(raw_dat_L$wavenumber), by=5),])+
           geom_line(aes(x = wavenumber, y = absorbance, group_by = filename, color = Age))+
           scale_x_reverse()+
           theme_classic())

# Look at raw spectra
ggplotly(ggplot(all_dat_L[seq(1,length(all_dat_L$wavenumber), by=5),])+
           geom_line(aes(x = wavenumber, y = absorbance, group_by = filename, color = Age))+
           scale_x_reverse()+
           theme_classic())

# Take average by age
summ <- all_dat_L %>% 
  dplyr::group_by(wavenumber, Age) %>%
  dplyr::summarize(mean_spec = mean(absorbance), se_spec = sd(absorbance)/sqrt(n()))

# Plot
ggplotly(ggplot(summ)+
  geom_line(aes(x = wavenumber, y = mean_spec, color = Age))+
  geom_ribbon(aes(x = wavenumber, ymin = mean_spec - se_spec, ymax = mean_spec + se_spec, fill = Age), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic())

#######################
# Plot scores - based on this I think we should be using through PC5 for reconstruction
plotScores(m1$res$cal, show.labels = FALSE, cgroup = dat_sg_filt$Age) # raw scores PC1 and PC2

# PC1 scores by age
ggplotly(ggplot(dat_sg_filt)+
           geom_point(aes(m1$res$cal$scores[,1], Age, color = as.factor(Age)))+
           scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic()) #plot scores from PC1 vs age

# Read age y axis, test age color
ggplotly(ggplot(dat_sg_filt)+
           geom_point(aes(m1$res$cal$scores[,1], read_age, color = as.factor(test_age)))+
           scale_y_continuous(lim=c(0,10), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic()) #plot scores from PC1 vs age

ggplotly(ggplot(dat_sg_filt)+ #boxplot
           geom_violin(aes(m1$res$cal$scores[,1], Age, middle = mean(m1$res$cal$scores[,1]), color = as.factor(Age), fatten = NULL))+
           scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,1], Age,  color = as.factor(Age)), trim = F)+
scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC1 Scores")+
  theme_classic()

  
# PC2 scores by age

# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,2], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC2 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,2], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC2 Scores")+
  theme_classic()

# PC3 scores by age

# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,3], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC3 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,3], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC3 Scores")+
  theme_classic()

# PC4 scores by age
# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,4], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC4 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,4], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC4 Scores")+
  theme_classic()

# PC5 scores by age
# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,5], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC5 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,5], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC5 Scores")+
  theme_classic()

# PC6 scores by age
# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,6], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC6 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,6], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC6 Scores")+
  theme_classic()

# PC7 scores by age

# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,7], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC7 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,7], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC7 Scores")+
  theme_classic()

# PC8 scores by age
# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,8], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC8 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[8], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC8 Scores")+
  theme_classic()

# PC9 scores by age

# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,9], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC9 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,9], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC9 Scores")+
  theme_classic()

# PC10 scores by age

# ggplotly(ggplot(dat_sg_filt)+
#            geom_boxplot(aes(m1$res$cal$scores[,10], middle = mean(m1$res$cal$scores[,2]), color = as.factor(Age), fatten = NULL))+
#            scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
#            labs(x = "PC10 Scores")+
#            theme_classic())

ggplot(dat_sg_filt)+
  geom_violin(aes(m1$res$cal$scores[,10], Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC10 Scores")+
  theme_classic()

#######################
# Do Shapiro-wilks test for normality in continuous data
## The null hypothesis of these tests is that “sample distribution is normal”. If the test is significant, the distribution is non-normal.
## Some are normally distributed, some not. Have to remember we are dealing with the potential of ageing error and therefore some spectra (scores, here) are likely being binned in the wrong age class. 
## are the ones that are super non-normal an indication of this?

scores <- as.data.frame(cbind(m1$res$cal$scores[,1], m1$res$cal$scores[,2], m1$res$cal$scores[,3], m1$res$cal$scores[,4], m1$res$cal$scores[,5], dat_sg_filt$Age))
  
scores <- scores %>%
  dplyr::rename(PC1 = V1, PC2 = V2, PC3 = V3, PC4 = V4, PC5 = V5, Age = V6)

# Age 1, PC1
# ggdensity(scores[which(scores$Age == "1"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "1"),]$PC1)

plot(density(scores[which(scores$Age == "1"),]$PC1)) 

shapiro.test(scores[which(scores$Age == "1"),]$PC1)

# Age 2, PC1
# ggdensity(scores[which(scores$Age == "2"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "2"),]$PC1)

plot(density(scores[which(scores$Age == "2"),]$PC1))

shapiro.test(scores[which(scores$Age == "2"),]$PC1)

# Age 3, PC1
# ggdensity(scores[which(scores$Age == "3"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "3"),]$PC1)

plot(density(scores[which(scores$Age == "3"),]$PC1))

shapiro.test(scores[which(scores$Age == "3"),]$PC1)

# Age 4, PC1
# ggdensity(scores[which(scores$Age == "4"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "4"),]$PC1)

plot(density(scores[which(scores$Age == "4"),]$PC1))

shapiro.test(scores[which(scores$Age == "4"),]$PC1)

# Age 5, PC1
# ggdensity(scores[which(scores$Age == "5"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "5"),]$PC1)

plot(density(scores[which(scores$Age == "5"),]$PC1))

shapiro.test(scores[which(scores$Age == "5"),]$PC1)

# Age 6, PC1
# ggdensity(scores[which(scores$Age == "6"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "6"),]$PC1)

plot(density(scores[which(scores$Age == "6"),]$PC1))

shapiro.test(scores[which(scores$Age == "6"),]$PC1)

# Age 7, PC1
# ggdensity(scores[which(scores$Age == "7"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "7"),]$PC1)

plot(density(scores[which(scores$Age == "7"),]$PC1))

shapiro.test(scores[which(scores$Age == "7"),]$PC1)

# Age 8, PC1
# ggdensity(scores[which(scores$Age == "8"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "8"),]$PC1)

plot(density(scores[which(scores$Age == "8"),]$PC1))

shapiro.test(scores[which(scores$Age == "8"),]$PC1)

# Age 9, PC1
# ggdensity(scores[which(scores$Age == "9"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "9"),]$PC1)

plot(density(scores[which(scores$Age == "9"),]$PC1))

shapiro.test(scores[which(scores$Age == "9"),]$PC1)

# Age 10, PC1
## too few data points
# ggdensity(scores[which(scores$Age == "10"),]$PC1, 
#           main = "Density plot of scores",
#           xlab = "Scores")
# 
# ggqqplot(scores[which(scores$Age == "10"),]$PC1)

plot(density(scores[which(scores$Age == "10"),]$PC1))

shapiro.test(scores[which(scores$Age == "10"),]$PC1)

#######################
# Sample from random distribution of scores around each age average, 100 per age.Do this n = Iter times. (Start with 10 to test). Make a list that contains score vectors by age.

# get an idea of variation in scores by age
summ <- scores %>% 
  group_by(Age) %>%
  summarize(mean_spec = across(c(PC1:PC5), ~mean(.)), se_spec = across(c(PC1:PC5), ~sd(.)/sqrt(n())))

summ$Age <- as.factor(summ$Age)

# plot raw scores to compare to simulated scores
#PC1
ggplot(summ)+
  geom_point(aes(y = as.factor(Age), x = mean_spec$PC1, color = as.factor(Age)))+
  geom_errorbar(aes(y = Age, xmin = mean_spec$PC1 - se_spec$PC1, xmax = mean_spec$PC1 + se_spec$PC1, color = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  theme_classic()

#PC2
ggplot(summ)+
  geom_point(aes(y = as.factor(Age), x = mean_spec$PC2, color = as.factor(Age)))+
  geom_errorbar(aes(y = Age, xmin = mean_spec$PC2 - se_spec$PC2, xmax = mean_spec$PC2 + se_spec$PC2, color = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  theme_classic()

#PC3
ggplot(summ)+
  geom_point(aes(y = as.factor(Age), x = mean_spec$PC3, color = as.factor(Age)))+
  geom_errorbar(aes(y = Age, xmin = mean_spec$PC3 - se_spec$PC3, xmax = mean_spec$PC3 + se_spec$PC3, color = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  theme_classic()

#PC4
ggplot(summ)+
  geom_point(aes(y = as.factor(Age), x = mean_spec$PC4, color = as.factor(Age)))+
  geom_errorbar(aes(y = Age, xmin = mean_spec$PC4 - se_spec$PC4, xmax = mean_spec$PC4 + se_spec$PC4, color = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  theme_classic()

#PC5
ggplot(summ)+
  geom_point(aes(y = as.factor(Age), x = mean_spec$PC5, color = as.factor(Age)))+
  geom_errorbar(aes(y = Age, xmin = mean_spec$PC5 - se_spec$PC5, xmax = mean_spec$PC5 + se_spec$PC5, color = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  theme_classic()


# different optimal SE for each PC (scale of variation is diff)
# DEPRECATED can standardize the score so that they all have unit variance (mean = 0, SD = 1) by squaring and dividing by corresponding eigenvalue. Because PC2 and PC3 cross 0, need to add 2 to each raw score before standardization and then remember to subtract it later. 

# Maybe can standardize instead by subtracting the mean and dividing by SD of all scores in that PC 
scores <- scores%>%
  mutate(PC1_stand = (PC1-mean(PC1))/sd(PC1), PC2_stand = (PC2-mean(PC2))/sd(PC2), PC3_stand = (PC3-mean(PC3))/sd(PC3), PC4_stand = (PC4-mean(PC4))/sd(PC4), PC5_stand = (PC5-mean(PC5))/sd(PC5))

# # Scores has columns as PCs, do dplyr across() to standardize the scores
# scores <- scores%>%
#   mutate(PC1_stand = ((PC1-5)^2)/m2$eigenvals[1], PC2_stand = ((PC2+5)^2)/m2$eigenvals[2], PC3_stand = ((PC3+5)^2)/m2$eigenvals[3], PC4_stand = ((PC4+5)^2)/m2$eigenvals[4], PC5_stand = ((PC5+5)^2)/m2$eigenvals[5])

# visualize
# PC1 scores by age
pdf(file = "PC1 stand scores.pdf",
  width = 4.5,
  height = 2.75
)

ggplot(scores)+
  geom_violin(aes(PC1_stand, Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC1 Scores (99.3%)")+
  theme_classic()

dev.off()

# PC2 scores by age
pdf(file = "PC2 stand scores.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(scores)+
  geom_violin(aes(PC2_stand, Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC2 Scores (0.46%)")+
  theme_classic()

dev.off()

# PC3 scores by age
pdf(file = "PC3 stand scores.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(scores)+
  geom_violin(aes(PC3_stand, Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC3 Scores (0.09%)")+
  theme_classic()

dev.off()

# PC4 scores by age
pdf(file = "PC4 stand scores.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(scores)+
  geom_violin(aes(PC4_stand, Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC4 Scores (0.08%)")+
  theme_classic()

dev.off()

# PC5 scores by age
pdf(file = "PC5 stand scores.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(scores)+
  geom_violin(aes(PC5_stand, Age,  color = as.factor(Age)), trim = F)+
  scale_y_continuous(lim=c(0.5,10.5), breaks = seq(1,10,1))+
  labs(x = "PC5 Scores (0.02%)")+
  theme_classic()

dev.off()


# Then draw error around each average standardized score by age, do this n = Iter times.

summ <- scores %>% # calculate mean and get feel for error
  group_by(Age) %>%
  dplyr::summarize(mean_stand = across(c(PC1_stand:PC5_stand), ~mean(.)), se_stand = across(c(PC1_stand:PC5_stand), ~sd(.)/sqrt(n())), sd_stand = across(c(PC1_stand:PC5_stand), ~sd(.)))

summ$Age <- as.factor(summ$Age)

# Plot average standardize score for each PC with SE bars
#PC1
ggplot(summ)+
  geom_point(aes(y = Age, x = mean_stand$PC1_stand, color = Age))+
  geom_errorbar(aes(y = Age, xmin = mean_stand$PC1_stand - se_stand$PC1_stand, xmax = mean_stand$PC1_stand + se_stand$PC1_stand, color = as.factor(Age)), alpha = 0.3)+
  labs(x = "PC1 Standardized",
       y = "Age")+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

#PC2
ggplot(summ)+
  geom_point(aes(y = Age, x = mean_stand$PC2_stand, color = Age))+
  geom_errorbar(aes(y = Age, xmin = mean_stand$PC2_stand - se_stand$PC2_stand, xmax = mean_stand$PC2_stand + se_stand$PC2_stand, color = as.factor(Age)), alpha = 0.3)+
  labs(x = "PC2 Standardized",
       y = "Age")+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

#PC3
ggplot(summ)+
  geom_point(aes(y = Age, x = mean_stand$PC3_stand, color = Age))+
  geom_errorbar(aes(y = Age, xmin = mean_stand$PC3_stand - se_stand$PC3_stand, xmax = mean_stand$PC3_stand + se_stand$PC3_stand, color = as.factor(Age)), alpha = 0.3)+
  labs(x = "PC3 Standardized",
       y = "Age")+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

#PC4
ggplot(summ)+
  geom_point(aes(y = as.factor(Age), x = mean_stand$PC4_stand, color = as.factor(Age)))+
  geom_errorbar(aes(y = Age, xmin = mean_stand$PC4_stand - se_stand$PC4_stand, xmax = mean_stand$PC4_stand + se_stand$PC4_stand, color = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

#PC5
ggplot(summ)+
  geom_point(aes(y = as.factor(Age), x = mean_stand$PC5_stand, color = as.factor(Age)))+
  geom_errorbar(aes(y = Age, xmin = mean_stand$PC5_stand - se_stand$PC5_stand, xmax = mean_stand$PC5_stand + se_stand$PC5_stand, color = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

# I think PC1-PC3 will be plenty to capture variability by age. Just added PC4+5 to see what happens and if results change.

# Now that I have standardized scores, need to draw random error around average PC by age for each PC1-5. I already have averages calculated in summ tibble. So, lets make a list "samp", make a loop which will draw error around mean = 0, sd = 0.01

#set up dataframe with 100 records per age (1-10 years)
#Can I sample from standardized scores grouped by age?

samp_list <- list()
empir_list <- list()
Iter <- 100 # reps

# Draw from empirical distribution so as to make no assumptions (baseline)
# Resample by scores summarized by age for each PC.

for (i in 1:Iter){
    for (k in seq_along(1:10)) {
      samp_emp <- data.frame(PC1_new_score = sample(scores[which(scores$Age == k),]$PC1_stand, size = 100, replace = TRUE), PC2_new_score = sample(scores[which(scores$Age == k),]$PC2_stand, size = 100, replace = TRUE), PC3_new_score = sample(scores[which(scores$Age == k),]$PC3_stand, size = 100, replace = TRUE), PC4_new_score = sample(scores[which(scores$Age == k),]$PC4_stand, size = 100, replace = TRUE), PC5_new_score = sample(scores[which(scores$Age == k),]$PC5_stand, size = 100, replace = TRUE), Age = k)
  samp_list[[k]] <- samp_emp
    }
    empir_list[[i]]<- samp_list
}


samp_emp <- empir_list %>%
  map(bind_rows)

test <- samp_emp[[1]]

# Assume normal distribution (which is a simplifying assumption)
summ_rep <- summ%>% 
  slice(rep(1:n(), each = 100)) #however many data points we want per age

samp.1 <- list() # creates an empty list
Iter <- 100 # reps

for (k in 1:Iter){ # make Iter objects in a list 
  samp.1[[k]] <- summ_rep %>% 
    rowwise()%>%
    dplyr::transmute(PC1_new_score = rnorm(1,mean = mean_stand$PC1_stand, sd = 0.1), PC2_new_score = rnorm(1,mean = mean_stand$PC2_stand, sd = 0.1), PC3_new_score = rnorm(1,mean = mean_stand$PC3_stand, sd = 0.1), PC4_new_score = rnorm(1,mean = mean_stand$PC4_stand, sd = 0.1), PC5_new_score = rnorm(1,mean = mean_stand$PC5_stand, sd = 0.1), Age)
}

## Sample with error of .5
samp.5 <- list() # creates an empty list
Iter <- 100 # reps

for (k in 1:Iter){ # make Iter objects in a list 
  samp.5[[k]] <- summ_rep %>% 
    rowwise()%>%
    dplyr::transmute(PC1_new_score = rnorm(1,mean = mean_stand$PC1_stand, sd = 0.5), PC2_new_score = rnorm(1,mean = mean_stand$PC2_stand, sd = 0.5), PC3_new_score = rnorm(1,mean = mean_stand$PC3_stand, sd = 0.5), PC4_new_score = rnorm(1,mean = mean_stand$PC4_stand, sd = 0.5), PC5_new_score = rnorm(1,mean = mean_stand$PC5_stand, sd = 0.5), Age)
}

## Sample with error of 1

samp1 <- list() # creates an empty list
Iter <- 100 # reps

for (k in 1:Iter){ # make Iter objects in a list 
  samp1[[k]] <- summ_rep %>% 
    rowwise()%>%
    dplyr::transmute(PC1_new_score = rnorm(1,mean = mean_stand$PC1_stand, sd = 1), PC2_new_score = rnorm(1,mean = mean_stand$PC2_stand, sd = 1), PC3_new_score = rnorm(1,mean = mean_stand$PC3_stand, sd = 1), PC4_new_score = rnorm(1,mean = mean_stand$PC4_stand, sd = 1), PC5_new_score = rnorm(1,mean = mean_stand$PC5_stand, sd = 1), Age)
}

# Then convert back by * by sd for each PC and then adding the mean

standscores_to_raw <- function(df) {
  df%>%
    transmute(PC1_sim = PC1_new_score*sd(scores$PC1)+mean(scores$PC1), PC2_sim = PC2_new_score*sd(scores$PC2)+mean(scores$PC2), PC3_sim = PC3_new_score*sd(scores$PC3)+mean(scores$PC3), PC4_sim = PC4_new_score*sd(scores$PC4)+mean(scores$PC4),PC5_sim = PC5_new_score*sd(scores$PC5)+mean(scores$PC5), Age)
}

## OLD
# standscores_to_raw <- function(df) {
#  df%>%
#     transmute(PC1_sim = sqrt(PC1_new_score*m2$eigenvals[1])*-1+5, PC2_sim = sqrt(PC2_new_score*m2$eigenvals[2])-5, PC3_sim = sqrt(PC3_new_score*m2$eigenvals[3])-5, PC4_sim = sqrt(PC4_new_score*m2$eigenvals[4])-5,PC5_sim = sqrt(PC5_new_score*m2$eigenvals[5])-5, Age)
# }

samp_.1 <- map(samp.1, standscores_to_raw)
samp_.5 <- map(samp.5, standscores_to_raw)
samp_1 <- map(samp1, standscores_to_raw)
samp_base <- map(samp_emp, standscores_to_raw)

explor.1 <- samp_.1[[1]]
explor.5 <- samp_.5[[1]]
explor1 <- samp_1[[1]]
exploremp<- samp_base[[1]]

pdf(file = "PC1 low error.pdf",
    width = 4.5,
    height = 2.75
)
ggplot(explor.1)+ #boxplot
           geom_violin(aes(PC1_sim, Age, color = as.factor(Age)), trim = F)+
           #scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic()

dev.off()

pdf(file = "PC1 med error.pdf",
    width = 4.5,
    height = 2.75
)
ggplot(explor.5)+ #boxplot
           geom_violin(aes(PC1_sim, Age, color = as.factor(Age)), trim = F)+
           #scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic()

dev.off()

pdf(file = "PC1 high error.pdf",
    width = 4.5,
    height = 2.75
)
ggplot(explor1)+ #boxplot
           geom_violin(aes(PC1_sim, Age, color = as.factor(Age)), trim = F)+
           #scale_y_continuous(lim=c(.5,10.5), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic()

dev.off()

pdf(file = "PC1 emp error.pdf",
    width = 4.5,
    height = 2.75
)
ggplot(exploremp)+ #boxplot
           geom_violin(aes(PC1_sim, Age, color = as.factor(Age)), trim = F)+
           scale_y_continuous(lim=c(.5,10.5), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic()

dev.off()

# For PC1, multiplied by -1 to make all raw scores, add 5 negative
# For PC2, subtracted 5
# For PC3, subtracted 5
# For PC4, subtracted 5
# For PC5, subtracted 5


#######################
# map() apply function to each element of the list which is to multiply scores * T(loadings) which will generate a matrix of spectra. Add spectra for each PC together to get final spectra. These spectra will be matched to "known age" now. 

Z1 <- as.matrix(m1$loadings[,1], ncols = 1, byrow = F) 
Z2 <- as.matrix(m1$loadings[,2], ncols = 1, byrow = F) 
Z3 <- as.matrix(m1$loadings[,3], ncols = 1, byrow = F) 
Z4 <- as.matrix(m1$loadings[,4], ncols = 1, byrow = F) 
Z5 <- as.matrix(m1$loadings[,5], ncols = 1, byrow = F) 
#transpose loading matrices
Z1_T <- t(Z1)
Z2_T <- t(Z2)
Z3_T <- t(Z3)
Z4_T <- t(Z4)
Z5_T <- t(Z5)
#scores from each PC
scores1 <- as.matrix(exploremp$PC1_sim, ncols=1, byrow = F)
scores2 <- as.matrix(exploremp$PC2_sim, ncols=1, byrow = F)
scores3 <- as.matrix(exploremp$PC3_sim, ncols=1, byrow = F)
scores4 <- as.matrix(exploremp$PC4_sim, ncols=1, byrow = F)
scores5 <- as.matrix(exploremp$PC5_sim, ncols=1, byrow = F)
#multiple each by scores from same PC
Spec1 <- scores1%*%Z1_T #compare these to OG scores
Spec2 <- scores2%*%Z2_T
Spec3 <- scores3%*%Z3_T
Spec4 <- scores3%*%Z4_T
Spec5 <- scores3%*%Z5_T
Spec <- Spec1+Spec2+Spec3+Spec4+Spec5


matplot(t(Spec1), type = "l")
matplot(t(Spec2), type = "l")
matplot(t(Spec3), type = "l")
matplot(t(Spec4), type = "l")
matplot(t(Spec5), type = "l")
matplot(t(Spec), type = "l")

#write function to convert scores back to spectra
scores_to_spectra <- function(scores_df){ #for 3 PCs
  #matrices of loadings for 3 pcs
  Z1 <- as.matrix(m1$loadings[,1], ncols = 1, byrow = F) 
  Z2 <- as.matrix(m1$loadings[,2], ncols = 1, byrow = F) 
  Z3 <- as.matrix(m1$loadings[,3], ncols = 1, byrow = F) 
  Z4 <- as.matrix(m1$loadings[,4], ncols = 1, byrow = F) 
  Z5 <- as.matrix(m1$loadings[,5], ncols = 1, byrow = F) 
  #transpose loading matrices
  Z1_T <- t(Z1)
  Z2_T <- t(Z2)
  Z3_T <- t(Z3)
  Z4_T <- t(Z4)
  Z5_T <- t(Z5)
  #scores from each PC
  scores1 <- as.matrix(scores_df$PC1_sim, ncols=1, byrow = F)
  scores2 <- as.matrix(scores_df$PC2_sim, ncols=1, byrow = F)
  scores3 <- as.matrix(scores_df$PC3_sim, ncols=1, byrow = F)
  scores4 <- as.matrix(scores_df$PC4_sim, ncols=1, byrow = F)
  scores5 <- as.matrix(scores_df$PC5_sim, ncols=1, byrow = F)
  #multiple each by scores from same PC
  Spec1 <- scores1%*%Z1_T #compare these to OG scores
  Spec2 <- scores2%*%Z2_T
  Spec3 <- scores3%*%Z3_T
  Spec4 <- scores4%*%Z4_T
  Spec5 <- scores5%*%Z5_T
  Spec <- Spec1+Spec2+Spec3+Spec4+Spec5
  Spec <- data.frame(scores_df$Age,Spec)
  Spec <- data.frame(seq(1:100), Spec)
  Spec <- Spec%>%
    dplyr::rename(spec_num = `seq.1.100.`, known_age = `scores_df.Age`)
}

#apply function to sample list
## normal, sd = .1
sim_samp.1 <- map(samp_.1, scores_to_spectra)
save(sim_samp.1, file = "sim_samp_100_.1.rda")

load(file = "sim_samp_100_.1.rda")

## normal, sd = .5
sim_samp.5 <- map(samp_.5, scores_to_spectra)
save(sim_samp.5, file = "sim_samp_100_.5.rda")

load(file = "sim_samp_100_.5.rda")

## normal, sd = 1
sim_samp1 <- map(samp_1, scores_to_spectra)
save(sim_samp1, file = "sim_samp_100_1.rda")

load(file = "sim_samp_100_1.rda")

## sample from empirical distribution
sim_sampemp <- map(samp_base, scores_to_spectra)
save(sim_sampemp, file = "sim_samp_100_emp.rda")

load(file = "sim_samp_100_emp.rda")

#plot to make sure this looks right!
test_.1 <- sim_samp.1[[1]]
test_.5 <- sim_samp.5[[1]]
test_1 <- sim_samp1[[1]]
test_emp <- sim_sampemp[[1]]

# Pivot for plotting
test_.1_L<-test_.1%>%tidyr::pivot_longer(.,cols=c(3:906),names_to="wavenumber",values_to="absorbance", names_prefix = "X") 

test_.5_L<-test_.5%>%tidyr::pivot_longer(.,cols=c(3:906),names_to="wavenumber",values_to="absorbance", names_prefix = "X") 

test_1_L<-test_1%>%tidyr::pivot_longer(.,cols=c(3:906),names_to="wavenumber",values_to="absorbance", names_prefix = "X") 

test_emp_L<-test_emp%>%tidyr::pivot_longer(.,cols=c(3:906),names_to="wavenumber",values_to="absorbance", names_prefix = "X") 


# Plot to see what recomposed spectra from PC1-PC5 look like
# SD = .1
pdf(file = "low sim spectra.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(test_.1_L)+
  geom_line(aes(as.numeric(wavenumber), absorbance, color = as.factor(known_age)))+
  #scale_y_continuous(lim = c(-.5,2), breaks = c(-0.5, 0, 0.5, 1, 1.5, 2))+
  labs(x = expression(paste("Wavenumber ",cm^-1)), y = "Absorbance")+
  scale_x_reverse()+
  theme_classic()

dev.off()

## SD = .5
pdf(file = "med sim spectra.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(test_.5_L)+
  geom_line(aes(as.numeric(wavenumber), absorbance, color = as.factor(known_age)))+
  #scale_y_continuous(lim = c(-.5,2), breaks = c(-0.5, 0, 0.5, 1, 1.5, 2))+
  labs(x = expression(paste("Wavenumber ",cm^-1)), y = "Absorbance")+
  scale_x_reverse()+
  theme_classic()

dev.off()
## SD = 1
pdf(file = "high sim spectra.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(test_1_L)+
  geom_line(aes(as.numeric(wavenumber), absorbance, color = as.factor(known_age)))+
  #scale_y_continuous(lim = c(-.5,2), breaks = c(-0.5, 0, 0.5, 1, 1.5, 2))+
  labs(x = expression(paste("Wavenumber ",cm^-1)), y = "Absorbance")+
  scale_x_reverse()+
  theme_classic()

dev.off()
## Empirical
pdf(file = "emp sim spectra.pdf",
    width = 4.5,
    height = 2.75
)

ggplot(test_emp_L)+
  geom_line(aes(as.numeric(wavenumber), absorbance, color = as.factor(known_age)))+
  #scale_y_continuous(lim = c(-.5,2), breaks = c(-0.5, 0, 0.5, 1, 1.5, 2))+
  labs(x = expression(paste("Wavenumber ",cm^-1)), y = "Absorbance")+
  scale_x_reverse()+
  theme_classic()

dev.off()

# test out pca to see scores!
m_sim <- pca(test_1[,3:906], ncomp = 10, scale= F, center = F)

plotScores(m_sim$res$cal, show.labels = FALSE, cgroup = test_df$known_age) # raw scores PC1 and PC2

plotScores(m_sim$res$cal, c(1,3), show.labels = FALSE, cgroup = test_df$known_age) # raw scores PC1 and PC2

plotScores(m_sim$res$cal, c(2,3), show.labels = FALSE, cgroup = test_df$known_age) # raw scores PC1 and PC2

#######################
# join each list element with simulated age dataset by "known age". 

sim_age_data <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Data/simulated age data mat.csv") #simulated age data

sim_age_data <- sim_age_data%>%
  filter(known_age %in% c(1,2,3,4,5,6,7,8,9,10))

model_error <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Puntilizer/Model comparisons all data 2013-2018/B2_S3_S1/cv and bias Reader 1.csv")

#plot simulated age data
count_dum <- sim_age_data %>% group_by(known_age, error_age) %>% dplyr::summarise(count = n())

ggplot(count_dum)+
  geom_point(aes(x = known_age, y = error_age, size = count), alpha = .2)+
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10,1))+
  scale_y_continuous(limits = c(0,13), breaks=seq(0,13,1))+
  geom_abline(slope=1,intercept = 0)+
  geom_line(data = model_error, aes(Age, Expected),color = "red")+
  geom_line(data = model_error, aes(Age, Expected + SD*2), color = "red", linetype = 2)+
  geom_line(data = model_error, aes(Age, Expected - SD*2), color = "red", linetype = 2)+
  #geom_ribbon(data = model_error, aes(x = Age, ymin = Expected - SD*2, ymax = Expected + SD*2), fill = "red", alpha = .3)+
  #geom_smooth(aes(x = known_age, y = error_age),method = "lm")+
  labs(x = "Known age",
       y = "Age estimate",
       subtitle = "Pacific cod simulated age data")+
  theme_classic()

# Plot raw data
## Plot raw test and read data
#Load data
MetaData <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Data/MetaData_2010-2019_n14579.csv")

AgeReads <- MetaData %>%
  filter(test_age > 0)%>%
  transmute(read_age, test_age)

count_dum <- AgeReads %>% group_by(read_age, test_age) %>% dplyr::summarise(count = n())

ggplot(count_dum)+
  geom_point(aes(x = read_age, y = test_age, size = count), alpha = .5)+
  scale_size_continuous((breaks = unique(count_dum$count)))+
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10,1))+
  scale_y_continuous(limits = c(0,13), breaks=seq(0,13,1))+
  geom_abline(slope=1,intercept = 0)+
  #geom_smooth(aes(x = known_age, y = error_age),method = "lm")+
  labs(x = "Read age",
       y = "Test age",
       subtitle = "Pacific cod 2010-2019 data")+
  theme_classic()

# write function to merge sim_age_data with simulated spectra
merge_dat <- function(df){
  sim_df <- data.frame(sim_age_data$error_age, df)
  sim_df <- sim_df%>%
    dplyr::rename(error_age = `sim_age_data.error_age`)
}

sim_samp_fin_.1 <- map(sim_samp.1, merge_dat)
sim_samp_fin_.5 <- map(sim_samp.5, merge_dat)
sim_samp_fin_1 <- map(sim_samp1, merge_dat)
sim_samp_fin_emp <- map(sim_sampemp, merge_dat)

#save to rda file
save(sim_samp_fin_.1, file = "simulated_dataset_.1_100.rda")
save(sim_samp_fin_.5, file = "simulated_dataset_.5_100.rda")
save(sim_samp_fin_1, file = "simulated_dataset_1_100.rda")
save(sim_samp_fin_emp, file = "simulated_dataset_emp_100.rda")


test_df <- sim_samp_fin[[1]]

#######################
# Now ready to pass these through code for model fitting!

#remove a lot of uneeded objects from environment
rm(all_dat)
rm(all_dat_L)
rm(all_dat_or)
rm(counts)
rm(f)
rm(m)
rm(m_sim)
rm(m2)
rm(meta_dat)
rm(raw_dat)
rm(raw_dat_filt)
rm(raw_dat_L)
rm(samp)
rm(samp2)
rm(scores)
rm(sim_age_data)
rm(sim_samp)
rm(Spec)
rm(Spec_L)
rm(summ)
rm(summ_rep)
rm(test)
rm(test_df)
rm(test_L)

