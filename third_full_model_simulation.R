#################
# Modeling

# packages
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)
library(ggpubr)

# write function for passing PLS model to each sim dataset
known_age_mod <- function(df) {
  df$known_age <- as.numeric(df$known_age)
  mdatools::pls(df[, c(4:907)], df$known_age, scale = FALSE, center = TRUE, ncomp = 5, cv = 20) #when I have more time to run this, change to ncomp = 5 and cv = 1
}

error_age_mod <- function(df) {
  mdatools::pls(df[, c(4:907)], df$error_age, scale = FALSE, center = TRUE, ncomp = 5, cv = 20) #when I have more time to run this, change to ncomp = 5 and cv = 1
}

load(file = "simulated_dataset_.1_100.rda")
load(file = "simulated_dataset_.5_100.rda")
load(file = "simulated_dataset_1_100.rda")
load(file = "simulated_dataset_emp_100.rda")


known_age_list.1 <- map(sim_samp_fin_.1, known_age_mod)
error_age_list.1 <- map(sim_samp_fin_.1, error_age_mod)

known_age_list.5 <- map(sim_samp_fin_.5, known_age_mod)
error_age_list.5 <- map(sim_samp_fin_.5, error_age_mod)

known_age_list1 <- map(sim_samp_fin_1, known_age_mod)
error_age_list1 <- map(sim_samp_fin_1, error_age_mod)

known_age_listemp <- map(sim_samp_fin_emp, known_age_mod)
error_age_listemp<- map(sim_samp_fin_emp, error_age_mod)

#save to rda file
save(known_age_list.1, file = "known_age_list.1.rda")
save(error_age_list.1, file = "error_age_list.1.rda")
save(known_age_list.5, file = "known_age_list.5.rda")
save(error_age_list.5, file = "error_age_list.5.rda")
save(known_age_list1, file = "known_age_list1.rda")
save(error_age_list1, file = "error_age_list1.rda")
save(known_age_listemp, file = "known_age_listemp.rda")
save(error_age_listemp, file = "error_age_listemp.rda")

load(file = "known_age_list.1.rda")
load(file = "error_age_list.1.rda")
load(file = "known_age_list.5.rda")
load(file = "error_age_list.5.rda")
load(file = "known_age_list1.rda")
load(file = "error_age_list1.rda")
load(file = "known_age_listemp.rda")
load(file = "error_age_listemp.rda")


# Pull out predictions and make a matrix
extract_preds <- function(mod) {
  #x <- mod$cvres$ncomp.selected #allow each to use optimal ncomp
  print(mod$cvres$y.pred[,5,1]) #use x to select preds from matrix
}

known_age_preds.1 <- map(known_age_list.1, extract_preds) #apply function to mod list
error_age_preds.1 <- map(error_age_list.1, extract_preds) # apply functiont to mod list

known_age_preds.5 <- map(known_age_list.5, extract_preds) #apply function to mod list
error_age_preds.5 <- map(error_age_list.5, extract_preds) # apply functiont to mod list

known_age_preds1 <- map(known_age_list1, extract_preds) #apply function to mod list
error_age_preds1 <- map(error_age_list1, extract_preds) # apply functiont to mod list

known_age_predsemp <- map(known_age_listemp, extract_preds) #apply function to mod list
error_age_predsemp <- map(error_age_listemp, extract_preds) # apply functiont to mod list

# Make a dataframe that has known_age, error_age, known_age_preds, error_age_preds from each Iter

known_age_preds.1 <- data.frame(matrix(unlist(known_age_preds.1), ncol=100, byrow=F)) 
colnames(known_age_preds.1) <- paste(colnames(known_age_preds.1), "T", sep = "_") #T for true
error_age_preds.1 <- data.frame(matrix(unlist(error_age_preds.1), ncol = 100, byrow=F))
colnames(error_age_preds.1) <- paste(colnames(error_age_preds.1), "E", sep = "_") #E for error or estimate

known_age_preds.5 <- data.frame(matrix(unlist(known_age_preds.5), ncol=100, byrow=F)) 
colnames(known_age_preds.5) <- paste(colnames(known_age_preds.5), "T", sep = "_") #T for true
error_age_preds.5 <- data.frame(matrix(unlist(error_age_preds.5), ncol = 100, byrow=F))
colnames(error_age_preds.5) <- paste(colnames(error_age_preds.5), "E", sep = "_") #E for error or estimate

known_age_preds1 <- data.frame(matrix(unlist(known_age_preds1), ncol=100, byrow=F)) 
colnames(known_age_preds1) <- paste(colnames(known_age_preds1), "T", sep = "_") #T for true
error_age_preds1 <- data.frame(matrix(unlist(error_age_preds1), ncol = 100, byrow=F))
colnames(error_age_preds1) <- paste(colnames(error_age_preds1), "E", sep = "_") #E for error or estimate

known_age_predsemp <- data.frame(matrix(unlist(known_age_predsemp), ncol=100, byrow=F)) 
colnames(known_age_predsemp) <- paste(colnames(known_age_predsemp), "T", sep = "_") #T for true
error_age_predsemp <- data.frame(matrix(unlist(error_age_predsemp), ncol = 100, byrow=F))
colnames(error_age_predsemp) <- paste(colnames(error_age_predsemp), "E", sep = "_") #E for error or estimate

age_df <- sim_samp_fin_.1[[1]][,c(1:3)]

preds_df.1 <- data.frame(age_df$known_age, age_df$error_age,  known_age_preds.1, error_age_preds.1)
colnames(preds_df.1) <- c("known_age", "error_age", paste0("T_", 1:100), paste0("E", 1:100))

preds_df.5 <- data.frame(age_df$known_age, age_df$error_age, known_age_preds.5, error_age_preds.5)
colnames(preds_df.5) <- c("known_age", "error_age", paste0("T_", 1:100), paste0("E", 1:100))

preds_df1 <- data.frame(age_df$known_age, age_df$error_age, known_age_preds1, error_age_preds1)
colnames(preds_df1) <- c("known_age", "error_age", paste0("T_", 1:100), paste0("E", 1:100))

preds_dfemp <- data.frame(age_df$known_age, age_df$error_age, known_age_predsemp, error_age_predsemp)
colnames(preds_dfemp) <- c("known_age", "error_age", paste0("T_", 1:100), paste0("E", 1:100))

# Output data
save(preds_df.1, file = "preds_df.1.rda")
save(preds_df.5, file = "preds_df.5.rda")
save(preds_df1, file = "preds_df1.rda")
save(preds_dfemp, file = "preds_dfemp.rda")

# Could I try a pc plot?
plotScores(known_age_list.1[[1]]$res$cal$xdecomp, show.labels = FALSE, cgroup = age_df$known_age)

plotScores(known_age_list.1[[1]]$res$cal$xdecomp, c(1,3), show.labels = FALSE, cgroup = age_df$known_age)

plotScores(known_age_list.1[[1]]$res$cal$xdecomp, c(2,3), show.labels = FALSE, cgroup = age_df$known_age)

plotScores(error_age_list.1[[1]]$res$cal$xdecomp, show.labels = FALSE, cgroup = age_df$known_age)

plotScores(known_age_list.5[[1]]$res$cal$xdecomp, show.labels = FALSE, cgroup = age_df$known_age)
