# plotting for simulation model results
## I could change this one into a rmarkdown pretty easily
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)
library(Metrics)
library(MLmetrics)
library(FSA)
library(MASS) 
library(reshape2) 
library(reshape)
library(flextable)

# What I need to do is somehow join datasets so that first half is all 1000 predictions from known age, and second half is all 1000 from age estimate.

# OLD for multiple batches to add up to 1000
# preds_df_1 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch1.csv") #simulated age data
# 
# preds_df_2 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch2.csv") #simulated age data
# preds_df_2 <- preds_df_2[,-c(1:3)]
# 
# preds_df_3 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch3.csv") #simulated age data
# preds_df_3 <- preds_df_3[,-c(1:3)]
# 
# preds_df_4 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch4.csv") #simulated age data
# preds_df_4 <- preds_df_4[,-c(1:3)]
# 
# preds_df_5 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch5.csv") #simulated age data
# preds_df_5 <- preds_df_5[,-c(1:3)]
# 
# preds_df_6 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch6.csv") #simulated age data
# preds_df_6 <- preds_df_6[,-c(1:3)]
# 
# preds_df_7 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch7.csv") #simulated age data
# preds_df_7 <- preds_df_7[,-c(1:3)]
# 
# preds_df_8 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch8.csv") #simulated age data
# preds_df_8 <- preds_df_8[,-c(1:3)]
# 
# preds_df_9 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch9.csv") #simulated age data
# preds_df_9 <- preds_df_9[,-c(1:3)]
# 
# preds_df_10 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/all_model_preds_PCA_0.1_batch10.csv") #simulated age data
# preds_df_10 <- preds_df_10[,-c(1:3)]
# 
# preds_df <- cbind(preds_df_1, preds_df_2, preds_df_3, preds_df_4, preds_df_5, preds_df_6, preds_df_7, preds_df_8, preds_df_9, preds_df_10)

# Load
load(file = "preds_df.1.rda")
load(file = "preds_df.5.rda")
load(file = "preds_df1.rda")
load(file = "preds_dfemp.rda")

  
# Now that we have data.frame of preds from each Iter, we can pivot longer to plot preds vs. "True Age"
preds_df.1$known_age <- as.numeric(preds_df.1$known_age)
preds_df.1_L <- melt(round(preds_df.1), id = c("known_age", "error_age"))
preds_df.5$known_age <- as.numeric(preds_df.5$known_age)
preds_df.5_L <- melt(round(preds_df.5), id = c("known_age", "error_age"))
preds_df1$known_age <- as.numeric(preds_df1$known_age)
preds_df1_L <- melt(round(preds_df1), id = c("known_age", "error_age"))
preds_dfemp$known_age <- as.numeric(preds_dfemp$known_age)
preds_dfemp_L <- melt(round(preds_dfemp), id = c("known_age", "error_age"))

# preds_df_L <- pivot_longer(round(preds_df),  cols=c(4:ncol(preds_df)), names_to="ref_type", values_to="prediction", names_repair = "minimal")

# Clean up data
preds_df.1_L <- preds_df.1_L %>%
  transmute(known_age, error_age, ref_type = case_when(str_detect(variable, "T") ~ "T", str_detect(variable, "E") ~ "E"), prediction = `value`)
preds_df.1_L$ref_type <- as.factor(preds_df.1_L$ref_type)

preds_df.5_L <- preds_df.5_L %>%
  transmute(known_age, error_age, ref_type = case_when(str_detect(variable, "T") ~ "T", str_detect(variable, "E") ~ "E"), prediction = `value`)
preds_df.5_L$ref_type <- as.factor(preds_df.5_L$ref_type)

preds_df1_L <- preds_df1_L %>%
  transmute(known_age, error_age, ref_type = case_when(str_detect(variable, "T") ~ "T", str_detect(variable, "E") ~ "E"), prediction = `value`)
preds_df1_L$ref_type <- as.factor(preds_df1_L$ref_type)

preds_dfemp_L <- preds_dfemp_L %>%
  transmute(known_age, error_age, ref_type = case_when(str_detect(variable, "T") ~ "T", str_detect(variable, "E") ~ "E"), prediction = `value`)
preds_dfemp_L$ref_type <- as.factor(preds_dfemp_L$ref_type)


# summarize for plotting
pred.1_summ <- preds_df.1_L %>% 
  group_by(known_age, ref_type) %>%
  summarize(mean_pred = mean(prediction), se_pred = sd(prediction)) #for SE - /sqrt(n())

pred.5_summ <- preds_df.5_L %>% 
  group_by(known_age, ref_type) %>%
  summarize(mean_pred = mean(prediction), se_pred = sd(prediction)) #for SE - /sqrt(n())

pred1_summ <- preds_df1_L %>% 
  group_by(known_age, ref_type) %>%
  summarize(mean_pred = mean(prediction), se_pred = sd(prediction)) #for SE - /sqrt(n())

predemp_summ <- preds_dfemp_L %>% 
  group_by(known_age, ref_type) %>%
  summarize(mean_pred = mean(prediction), se_pred = sd(prediction)) #for SE - /sqrt(n())

# Symmetry
## .01
known_age <- preds_df.1$known_age
estimated_age <- preds_df.1$error_age
known_age_ref <- preds_df.1_L[which(preds_df.1_L$ref_type == "T"),]$prediction
estimated_age_ref <- preds_df.1_L[which(preds_df.1_L$ref_type == "E"),]$prediction
bias_df.1 <- data.frame(known_age_ref, estimated_age_ref, known_age, estimated_age)
ab.ale <- ageBias(known_age_ref~estimated_age_ref,data=bias_df.1)
summary(ab.ale,what="bias")

## .05
known_age_ref <- preds_df.5_L[which(preds_df.5_L$ref_type == "T"),]$prediction
estimated_age_ref <- preds_df.5_L[which(preds_df.5_L$ref_type == "E"),]$prediction
bias_df.5 <- data.frame(known_age_ref, estimated_age_ref, known_age, estimated_age)
ab.ale <- ageBias(known_age_ref~estimated_age_ref,data=bias_df.5)
summary(ab.ale,what="bias")

## .1
known_age_ref <- preds_df1_L[which(preds_df1_L$ref_type == "T"),]$prediction
estimated_age_ref <- preds_df1_L[which(preds_df1_L$ref_type == "E"),]$prediction
bias_df1 <- data.frame(known_age_ref, estimated_age_ref, known_age, estimated_age)
ab.ale <- ageBias(known_age_ref~estimated_age_ref,data=bias_df1)
summary(ab.ale,what="bias")

## empirical
known_age_ref <- preds_dfemp_L[which(preds_dfemp_L$ref_type == "T"),]$prediction
estimated_age_ref <- preds_dfemp_L[which(preds_dfemp_L$ref_type == "E"),]$prediction
bias_dfemp <- data.frame(known_age_ref, estimated_age_ref, known_age, estimated_age)
ab.ale <- ageBias(known_age_ref~estimated_age_ref,data=bias_dfemp)
summary(ab.ale,what="bias")

# Bias plots
## 0.01
bias_df.1_summ <- bias_df.1%>%
  mutate(bias_known = known_age_ref-known_age, bias_est = estimated_age_ref-known_age)%>%
  group_by(known_age)%>%
  summarise(mean_bias_known = mean(bias_known), sd_bias_known = sd(bias_known), mean_bias_est = mean(bias_est), sd_bias_est = sd(bias_est))

pdf(file = "age bias plot 0.01.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4.5)

ggplot(bias_df.1_summ)+
  geom_point(aes(as.numeric(known_age), mean_bias_known), color="#26BB98")+
  geom_point(aes(as.numeric(known_age)-.2, mean_bias_est), color="#be188a")+
  geom_errorbar(aes(as.numeric(known_age)-.2,ymin = mean_bias_est - sd_bias_est, ymax = mean_bias_est + sd_bias_est), color = "#be188a", width = .2)+
  geom_errorbar(aes(as.numeric(known_age),ymin = mean_bias_known - sd_bias_known, ymax = mean_bias_known + sd_bias_known), color = "#26BB98", width = .2)+
  geom_abline(slope = 0, intercept = 0, color = "grey50", linetype = 2)+
  scale_x_continuous(lim = c(0,10.5), breaks=seq(0,10,1))+
  scale_y_continuous(lim = c(-4,4), breaks=seq(-4,4,1))+
  labs(x = "Simulated known age (years)",
       y = "Prediction - known age (years)")+
  theme_classic()

dev.off()

## 0.05
bias_df.5_summ <- bias_df.5%>%
  mutate(bias_known = known_age_ref-known_age, bias_est = estimated_age_ref-known_age)%>%
  group_by(known_age)%>%
  summarise(mean_bias_known = mean(bias_known), sd_bias_known = sd(bias_known), mean_bias_est = mean(bias_est), sd_bias_est = sd(bias_est))

pdf(file = "age bias plot 0.05.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4.5)

ggplot(bias_df.5_summ)+
  geom_point(aes(as.numeric(known_age), mean_bias_known), color="#26BB98")+
  geom_point(aes(as.numeric(known_age)-.2, mean_bias_est), color="#be188a")+
  geom_errorbar(aes(as.numeric(known_age)-.2,ymin = mean_bias_est - sd_bias_est, ymax = mean_bias_est + sd_bias_est), color = "#be188a", width = .2)+
  geom_errorbar(aes(as.numeric(known_age),ymin = mean_bias_known - sd_bias_known, ymax = mean_bias_known + sd_bias_known), color = "#26BB98", width = .2)+
  geom_abline(slope = 0, intercept = 0, color = "grey50", linetype = 2)+
  scale_x_continuous(lim = c(0,10.5), breaks=seq(0,10,1))+
  scale_y_continuous(lim = c(-4,4), breaks=seq(-4,4,1))+
  labs(x = "Simulated known age (years)",
       y = "Prediction - known age (years)")+
  theme_classic()

dev.off()

## .1
bias_df1_summ <- bias_df1%>%
  mutate(bias_known = known_age_ref-known_age, bias_est = estimated_age_ref-known_age)%>%
  group_by(known_age)%>%
  summarise(mean_bias_known = mean(bias_known), sd_bias_known = sd(bias_known), mean_bias_est = mean(bias_est), sd_bias_est = sd(bias_est))

pdf(file = "age bias plot 0.1.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4.5)

ggplot(bias_df1_summ)+
  geom_point(aes(as.numeric(known_age), mean_bias_known), color="#26BB98")+
  geom_point(aes(as.numeric(known_age)-.2, mean_bias_est), color="#be188a")+
  geom_errorbar(aes(as.numeric(known_age)-.2,ymin = mean_bias_est - sd_bias_est, ymax = mean_bias_est + sd_bias_est), color = "#be188a", width = .2)+
  geom_errorbar(aes(as.numeric(known_age),ymin = mean_bias_known - sd_bias_known, ymax = mean_bias_known + sd_bias_known), color = "#26BB98", width = .2)+
  geom_abline(slope = 0, intercept = 0, color = "grey50", linetype = 2)+
  scale_x_continuous(lim = c(0,10.5), breaks=seq(0,10,1))+
  scale_y_continuous(lim = c(-4,4), breaks=seq(-4,4,1))+
  labs(x = "Simulated known age (years)",
       y = "Prediction - known age (years)")+
  theme_classic()

dev.off()

## Emp
bias_dfemp_summ <- bias_dfemp%>%
  mutate(bias_known = known_age_ref-known_age, bias_est = estimated_age_ref-known_age)%>%
  group_by(known_age)%>%
  summarise(mean_bias_known = mean(bias_known), sd_bias_known = sd(bias_known), mean_bias_est = mean(bias_est), sd_bias_est = sd(bias_est))

pdf(file = "age bias plot emp.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4.5)

ggplot(bias_dfemp_summ)+
  geom_point(aes(as.numeric(known_age), mean_bias_known), color="#26BB98")+
  geom_point(aes(as.numeric(known_age)-.2, mean_bias_est), color="#be188a")+
  geom_errorbar(aes(as.numeric(known_age)-.2,ymin = mean_bias_est - sd_bias_est, ymax = mean_bias_est + sd_bias_est), color = "#be188a", width = .2)+
  geom_errorbar(aes(as.numeric(known_age),ymin = mean_bias_known - sd_bias_known, ymax = mean_bias_known + sd_bias_known), color = "#26BB98", width = .2)+
  geom_abline(slope = 0, intercept = 0, color = "grey50", linetype = 2)+
  scale_x_continuous(lim = c(0,10.5), breaks=seq(0,10,1))+
  scale_y_continuous(lim = c(-4,4), breaks=seq(-4,4,1))+
  labs(x = "Simulated known age (years)",
       y = "Prediction - known age (years)")+
  theme_classic()

dev.off()

# plot predictions relative to known age

ggplot(filter(preds_df.1_L, ref_type == "T"))+
  geom_point(aes(as.numeric(known_age), prediction, color = ref_type), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  scale_color_manual(values = "darkblue")+
  scale_fill_manual(values = "darkblue")+
  scale_x_continuous(lim = c(0,12), breaks=seq(0,12,1))+
  scale_y_continuous(lim = c(0,12), breaks=seq(0,12,1))+
  labs(x = "Known age (years)",
       y = "Predicted age (years)")+
  theme_classic()

# plot predictions relative to error age

ggplot(filter(preds_df.1_L, ref_type == "E"))+
  geom_point(aes(as.numeric(error_age), prediction, color = ref_type), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  scale_color_manual(values =  "cornflowerblue")+
  scale_fill_manual(values =  "cornflowerblue")+
  scale_x_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  scale_y_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  labs(x = "Simulated age estimate (years)",
       y = "Predicted age (years)")+
  theme_classic()

# plot predictions relative to known age

ggplot(filter(preds_df.1_L, ref_type == "E"))+
  geom_point(aes(as.numeric(known_age), prediction, color = ref_type), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  scale_color_manual(values =  "cornflowerblue")+
  scale_fill_manual(values =  "cornflowerblue")+
  scale_x_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  scale_y_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  labs(x = "Known age (years)",
       y = "Predicted age (years)")+
  theme_classic()


# full prediction plot for error f 0.01
count_dum <- preds_df.1_L %>% group_by(known_age, ref_type, prediction) %>% dplyr::summarise(count = n())

pdf(file = "full prediction plot 0.01.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4)

ggplot()+
  geom_point(data = filter(count_dum, ref_type == "T"), aes(as.numeric(known_age)-.2, prediction, size = count), color = "#26BB98", alpha = 0.1)+
  geom_point(data = filter(count_dum, ref_type == "E"), aes(as.numeric(known_age)+.2, prediction, size = count), color = "#be188a", alpha = 0.1)+
  scale_size_continuous(limits=c(1,10000),breaks=c(500,2500,4500,6500))+
  geom_line(data = pred.1_summ, aes(x = as.numeric(known_age), y = mean_pred, color = ref_type))+
  geom_ribbon(data = pred.1_summ, aes(x = as.numeric(known_age), ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = ref_type), alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, color = "grey50")+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = c("#be188a", "#26BB98"))+
  scale_x_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  labs(x = "Simulated known age (years)",
       y = "Predicted age (years)")+
  theme_classic()

dev.off()

# full prediction plot for error f 0.5
count_dum <- preds_df.5_L %>% group_by(known_age, ref_type, prediction) %>% dplyr::summarise(count = n())

pdf(file = "full prediction plot 0.05.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4)

ggplot()+
  geom_point(data = filter(count_dum, ref_type == "T"), aes(as.numeric(known_age)-.2, prediction, size = count), color = "#26BB98", alpha = 0.1)+
  geom_point(data = filter(count_dum, ref_type == "E"), aes(as.numeric(known_age)+.2, prediction, size = count), color = "#be188a", alpha = 0.1)+
  scale_size_continuous(limits=c(100,7500),breaks=c(500,2500,4500,6500))+
  geom_line(data = pred.5_summ, aes(x = as.numeric(known_age), y = mean_pred, color = ref_type))+
  geom_ribbon(data = pred.5_summ, aes(x = as.numeric(known_age), ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = ref_type), alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, color = "grey50")+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = c("#be188a", "#26BB98"))+
  scale_x_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  labs(x = "Simulated known age (years)",
       y = "Predicted age (years)")+
  theme_classic()

dev.off()

# full prediction plot for error f 1
count_dum <- preds_df1_L %>% group_by(known_age, ref_type, prediction) %>% dplyr::summarise(count = n())

pdf(file = "full prediction plot 1.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4)

ggplot()+
  geom_point(data = filter(count_dum, ref_type == "T"), aes(as.numeric(known_age)-.2, prediction, size = count), color = "#26BB98", alpha = 0.1)+
  geom_point(data = filter(count_dum, ref_type == "E"), aes(as.numeric(known_age)+.2, prediction, size = count), color = "#be188a", alpha = 0.1)+
  scale_size_continuous(limits=c(100,7500),breaks=c(500,2500,4500,6500))+
  geom_line(data = pred1_summ, aes(x = as.numeric(known_age), y = mean_pred, color = ref_type))+
  geom_ribbon(data = pred1_summ, aes(x = as.numeric(known_age), ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = ref_type), alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, color = "grey50")+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = c("#be188a", "#26BB98"))+
  scale_x_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  labs(x = "Simulated known age (years)",
       y = "Predicted age (years)")+
  theme_classic()

dev.off()

# full prediction plot for empirical
count_dum <- preds_dfemp_L %>% group_by(known_age, ref_type, prediction) %>% dplyr::summarise(count = n())

pdf(file = "full prediction plot emp.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4)

ggplot()+
  geom_point(data = filter(count_dum, ref_type == "T"), aes(as.numeric(known_age)-.2, prediction, size = count), color = "#26BB98", alpha = 0.1)+
  geom_point(data = filter(count_dum, ref_type == "E"), aes(as.numeric(known_age)+.2, prediction, size = count), color = "#be188a", alpha = 0.1)+
  scale_size_continuous(limits=c(100,7500),breaks=c(500,2500,4500,6500))+
  geom_line(data = predemp_summ, aes(x = as.numeric(known_age), y = mean_pred, color = ref_type))+
  geom_ribbon(data = predemp_summ, aes(x = as.numeric(known_age), ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = ref_type), alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, color = "grey50")+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = c("#be188a", "#26BB98"))+
  scale_x_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(lim = c(0,14), breaks=seq(0,14,1))+
  labs(x = "Simulated known age (years)",
       y = "Predicted age (years)")+
  theme_classic()

dev.off()

# Calculate RMSE and R2 as side plots to show range
RMSE.1 <- preds_df.1 %>%
  summarise(rmse_t = across(3:102, ~rmse(known_age,round(.))), rmse_e = across(103:202, ~rmse(error_age,round(.))))

RMSE.5 <- preds_df.5 %>%
  summarise(rmse_t = across(3:102, ~rmse(known_age,round(.))), rmse_e = across(103:202, ~rmse(error_age,round(.))))

RMSE1 <- preds_df1 %>%
  summarise(rmse_t = across(3:102, ~rmse(known_age,round(.))), rmse_e = across(103:202, ~rmse(error_age,round(.))))

RMSEemp <- preds_dfemp %>%
  summarise(rmse_t = across(3:102, ~rmse(known_age,round(.))), rmse_e = across(103:202, ~rmse(error_age,round(.))))

RMSE.1 <- t(RMSE.1)
RMSE.1 <- data.frame(RMSE = RMSE.1, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("low",200))
RMSE.1$ref_type <- as.factor(RMSE.1$ref_type)
RMSE.1$RMSE <- as.numeric(RMSE.1$RMSE)

RMSE.5 <- t(RMSE.5)
RMSE.5 <- data.frame(RMSE = RMSE.5, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("medium",200))
RMSE.5$ref_type <- as.factor(RMSE.5$ref_type)
RMSE.5$RMSE <- as.numeric(RMSE.5$RMSE)

RMSE1 <- t(RMSE1)
RMSE1 <- data.frame(RMSE = RMSE1, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("high",200))
RMSE1$ref_type <- as.factor(RMSE1$ref_type)
RMSE1$RMSE <- as.numeric(RMSE1$RMSE)

RMSEemp <- t(RMSEemp)
RMSEemp <- data.frame(RMSE = RMSEemp, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("empirical",200))
RMSEemp$ref_type <- as.factor(RMSEemp$ref_type)
RMSEemp$RMSE <- as.numeric(RMSEemp$RMSE)

# Plot
pdf(file = "RMSE plot 0.01.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(RMSE.1)+
  geom_violin(aes(x = ref_type, y = RMSE, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,2))+
  labs(x = "Reference age type",
       y = "RMSE (years)")+
  theme_classic()

dev.off()

pdf(file = "RMSE plot 0.05.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(RMSE.5)+
  geom_violin(aes(x = ref_type, y = RMSE, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,2))+
  labs(x = "Reference age type",
       y = "RMSE (years)")+
  theme_classic()

dev.off()

pdf(file = "RMSE plot 0.1.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(RMSE1)+
  geom_violin(aes(x = ref_type, y = RMSE, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,2))+
  labs(x = "Reference age type",
       y = "RMSE (years)")+
  theme_classic()

dev.off()

pdf(file = "RMSE plot emp.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(RMSEemp)+
  geom_violin(aes(x = ref_type, y = RMSE, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,2))+
  labs(x = "Reference age type",
       y = "RMSE (years)")+
  theme_classic()

dev.off()

## Full plot
RMSE_full <- rbind(RMSEemp, RMSE.1, RMSE.5, RMSE1)
RMSE_full$error <- factor(RMSE_full$error, levels = c("empirical", "low", "medium", "high"))

pdf(file = "full RMSE.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4)

ggplot(RMSE_full)+
  geom_violin(aes(x = error, y = RMSE, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,2))+
  labs(x = "Spectral error",
       y = "RMSE (years)")+
  theme_classic()

dev.off()

save(RMSE_full, file = "RMSE_values.csv")

# Calculate R2 
R2.1 <- preds_df.1 %>%
  summarise(R2_t = across(3:102, ~R2_Score(known_age,round(.))), R2_e = across(103:202, ~R2_Score(error_age,round(.))))

R2.5 <- preds_df.5 %>%
  summarise(R2_t = across(3:102, ~R2_Score(known_age,round(.))), R2_e = across(103:202, ~R2_Score(error_age,round(.))))

R21 <- preds_df1 %>%
  summarise(R2_t = across(3:102, ~R2_Score(known_age,round(.))), R2_e = across(103:202, ~R2_Score(error_age,round(.))))

R2emp <- preds_dfemp %>%
  summarise(R2_t = across(3:102, ~R2_Score(known_age,round(.))), R2_e = across(103:202, ~R2_Score(error_age,round(.))))

R2.1 <- t(R2.1)
R2.1 <- data.frame(R2_score = R2.1, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("low", 200))
R2.1$ref_type <- as.factor(R2.1$ref_type)
R2.1$R2_score <- as.numeric(R2.1$R2_score)

R2.5 <- t(R2.5)
R2.5 <- data.frame(R2_score = R2.5, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("medium", 200))
R2.5$ref_type <- as.factor(R2.5$ref_type)
R2.5$R2_score <- as.numeric(R2.5$R2_score)

R21 <- t(R21)
R21 <- data.frame(R2_score = R21, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("high", 200))
R21$ref_type <- as.factor(R21$ref_type)
R21$R2_score <- as.numeric(R21$R2_score)

R2emp <- t(R2emp)
R2emp <- data.frame(R2_score = R2emp, ref_type = cbind(rep(c("T", "E"), each = 100)), error = rep("empirical", 200))
R2emp$ref_type <- as.factor(R2emp$ref_type)
R2emp$R2_score <- as.numeric(R2emp$R2_score)


# Plot

pdf(file = "R2 plot 0.01.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(R2.1)+
  geom_violin(aes(x = ref_type, y = R2_score, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Reference age type",
       y = "R2")+
  theme_classic()

dev.off()

pdf(file = "R2 plot 0.05.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(R2.5)+
  geom_violin(aes(x = ref_type, y = R2_score, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Reference age type",
       y = "R2")+
  theme_classic()

dev.off()

pdf(file = "R2 plot 0.1.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(R21)+
  geom_violin(aes(x = ref_type, y = R2_score, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Reference age type",
       y = "R2")+
  theme_classic()

dev.off()

pdf(file = "R2 plot emp.pdf",   # The directory you want to save the file in
    width = 3, # The width of the plot in inches
    height = 4)

ggplot(R2emp)+
  geom_violin(aes(x = ref_type, y = R2_score, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Reference age type",
       y = "R2")+
  theme_classic()

dev.off()

## Full plot
R2_full <- rbind(R2emp, R2.1, R2.5, R21)
R2_full$error <- factor(R2_full$error, levels = c("empirical", "low", "medium", "high"))

pdf(file = "R2 plot all.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 4)

ggplot(R2_full)+
  geom_violin(aes(x = error, y = R2_score, color = ref_type, fill = ref_type), trim = F)+
  scale_color_manual(values = c("#be188a", "#26BB98"))+
  scale_fill_manual(values = alpha(c("#be188a", "#26BB98"),.3))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "Spectral error",
       y = "R2")+
  theme_classic()

dev.off()

save(R2_full, file = "R2_values.csv")

# Table of RMSE and R2 ranges for each 
# Make table df
table_df <- data.frame(spec_error = c("empirical","empirical","low","low","medium","medium","high", "high"), ref_age = c("known age", "age estimate", "known age", "age estimate", "known age", "age estimate", "known age", "age estimate"), RMSE = c(paste0(round(min(RMSEemp[which(RMSEemp$ref_type == "T"),]$RMSE), digits = 2), "-", round(max(RMSEemp[which(RMSEemp$error == "empirical" & RMSEemp$ref_type == "T"),]$RMSE),digits = 2)), 
paste0(round(min(RMSEemp[which(RMSEemp$ref_type == "E"),]$RMSE), digits = 2), "-", round(max(RMSEemp[which(RMSEemp$ref_type == "E"),]$RMSE),digits = 2)), 
paste0(round(min(RMSE.1[which(RMSE.1$ref_type == "T"),]$RMSE), digits = 2), "-", round(max(RMSE.1[which(RMSEemp$ref_type == "T"),]$RMSE),digits = 2)), 
paste0(round(min(RMSE.1[which(RMSE.1$ref_type == "E"),]$RMSE), digits = 2), "-", round(max(RMSE.1[which(RMSE.1$ref_type == "E"),]$RMSE),digits = 2)), 
paste0(round(min(RMSE.5[which(RMSE.5$ref_type == "T"),]$RMSE), digits = 2), "-", round(max(RMSE.5[which(RMSE.5$ref_type == "T"),]$RMSE),digits = 2)), 
paste0(round(min(RMSE.5[which(RMSE.5$ref_type == "E"),]$RMSE), digits = 2), "-", round(max(RMSE.5[which(RMSE.5$ref_type == "E"),]$RMSE),digits = 2)),
paste0(round(min(RMSE1[which(RMSE1$ref_type == "T"),]$RMSE), digits = 2), "-", round(max(RMSE1[which(RMSE1$ref_type == "T"),]$RMSE),digits = 2)), 
paste0(round(min(RMSE1[which(RMSE1$ref_type == "E"),]$RMSE), digits = 2), "-", round(max(RMSE1[which(RMSE1$ref_type == "E"),]$RMSE),digits = 2))), 
r2 = c(paste0(round(min(R2emp[which(R2emp$ref_type == "T"),]$R2), digits = 2), "-", round(max(R2emp[which(R2emp$ref_type == "T"),]$R2),digits = 2)), 
       paste0(round(min(R2emp[which(R2emp$ref_type == "E"),]$R2), digits = 2), "-", round(max(R2emp[which(R2emp$ref_type == "E"),]$R2),digits = 2)),
       paste0(round(min(R2.1[which(R2.1$ref_type == "T"),]$R2), digits = 2), "-", round(max(R2.1[which(R2.1$ref_type == "T"),]$R2),digits = 2)), 
       paste0(round(min(R2.1[which(R2.1$ref_type == "E"),]$R2), digits = 2), "-", round(max(R2.1[which(R2.1$ref_type == "E"),]$R2),digits = 2)),
       paste0(round(min(R2.5[which(R2.5$ref_type == "T"),]$R2), digits = 2), "-", round(max(R2.5[which(R2.5$ref_type == "T"),]$R2),digits = 2)), 
       paste0(round(min(R2.5[which(R2.5$ref_type == "E"),]$R2), digits = 2), "-", round(max(R2.5[which(R2.5$ref_type == "E"),]$R2),digits = 2)),
       paste0(round(min(R21[which(R21$ref_type == "T"),]$R2), digits = 2), "-", round(max(R21[which(R21$ref_type == "T"),]$R2),digits = 2)), 
       paste0(round(min(R21[which(R21$ref_type == "E"),]$R2), digits = 2), "-", round(max(R21[which(R2.5$ref_type == "E"),]$R2),digits = 2))))


# format values properly


# plot table using flextable package
ft <- flextable(table_df) %>%
  align(align = "center", part = "all") %>%
  line_spacing(space = 1, part = "all") %>%
  width (j = 2, width = 1.5) %>%
  width (j = 3, width = 1.5) %>%
  width (j = 4, width = 1.5)
  


ft <- set_header_labels(ft, spec_error = "Spectral error", ref_age = "Reference age type", RMSE = "RMSE (years)", r2 = "r2") #change headers

ft <- compose(ft, i = 1, j = 4, part = "header", # Change headers
              value = as_paragraph(
                "r",
                as_sup("2")
              ) )

# ft <- add_header_row(ft,
#                      colwidths = c(5, 7),
#                      values = c("All Data", "20% Double-Read")
# )
# 
# border <- fp_border_default()
# ft <- vline(ft, j = c('Year','outlier'), border = border, part = "all")
# 
# ft <- bg(ft, i = ~ `outlier`  > .5,
#          j = c(5),
#          bg="red")
# 
# ft <- bg(ft, i = ~ `outlier_20`  > .5,
#          j = c(9),
#          bg="red")


ft

