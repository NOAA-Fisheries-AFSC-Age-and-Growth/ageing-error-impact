# # Load packages & set wd
# rm(list=ls())
library("devtools")
devtools::install_github("nwfsc-assess/nwfscAgeingError",force=TRUE)
library(nwfscAgeingError)
library(tidyverse)

# File where the Puntilizer (pre-compiled in ADMB) resides
SourceFile <-file.path(system.file("executables",package="nwfscAgeingError"),.Platform$file.sep)

SourceFile <- ("C:/Users/marri/OneDrive/Documents/R/win-library/4.1/nwfscAgeingError/executables")

#Load data
MetaData <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Data/MetaData_2010-2019_n14579.csv")

# #Filter data to investigate specific reader indices, all relative to tester index 6; 2010-2019
# AgeReads <- filter(MetaData, tester == "6")
# 
# #Reader index 3
# AgeReads_3 <- AgeReads %>%
#   filter(reader_index == "3")%>%
#   transmute(read_age, test_age)
# 
# #Reader index 8
# AgeReads_8 <- AgeReads %>%
#   filter(reader_index == "8")%>%
#   transmute(read_age, test_age)
# 
# #Reader index 21
# AgeReads_21 <- AgeReads %>%
#   filter(reader_index == "21")%>%
#   transmute(read_age, test_age)
# 
# #Reader code 59
# AgeReads_59 <- AgeReads %>%
#   filter(reader_index == "59")%>%
#   transmute(read_age, test_age)
# 
# #Reader code 71
# AgeReads_71 <- AgeReads %>%
#   filter(reader_index == "71")%>%
#   transmute(read_age, test_age)

AgeReads <- MetaData %>%
  filter(test_age > 0)%>%
  transmute(read_age, test_age)

# AgeReads<-round(AgeReads)
# mydir<-getwd()

################################################################################
# Format data (unsure about this section)
# Is this necessary in order to categorize age estimate scenarios
# For example, ages match, ages differ by 1, ages differ by 2, etc.?
Nreaders <-dim(AgeReads)[2]
# make table formatted as required by ageing error software
Reads2 = data.frame(count=1, AgeReads[1,])
# loop over rows of original data
for(RowI in 2:nrow(AgeReads)){
  DupRow <- NA
  # loop over all previous rows looking for duplicates of the current row
  for(PreviousRowJ in 1:nrow(Reads2)){
    # if all values match, take note of previous row number
    if(all(AgeReads[RowI,1:Nreaders]==
           Reads2[PreviousRowJ,1:Nreaders+1])){
      DupRow = PreviousRowJ
    }
  }
  # if no duplicate found, add new row
  if(is.na(DupRow)){
    # Add new row to ChinaReads2
    Reads2 <- rbind(Reads2, data.frame(count=1, AgeReads[RowI,]))
  }
  # if duplicate found, increment count
  if(!is.na(DupRow)){
    # Increment number of samples for the previous duplicate
    Reads2[DupRow,1] <- Reads2[DupRow,1] + 1
  }
}


# Reads2[,4] <-Reads2[,2]-Reads2[,3]
  
################################################################################

setwd("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Pacific cod/Puntilizer")

mydir<-getwd()

### ADMB settings
# Define min/max ages
MinAge <- 1 #min(Reads2[,-1])
MaxAge <- 14 #ceiling(max(Reads2[,-1])/10)*10
  
# Generate vector with settings for Bias (1 entry per reader)
# -X = Mirror the parameters for reader X
# 0 = Unbiased (at least 1 reader has to be)
# 1 = Linear bias
# 2 = Curvilinear bias (3 param)
  
  
# Generate vector with settings for SD  (1 entry per reader)
# -X = Mirror the parameters for reader X
# 0 = No error
# 1 = Constant coefficient of variation
# 2 = Curvilinear standard deviation (3 param)
# 3 = Curvilinear coefficient of variation (3 param)
  
KnotAges = list(NA,NA,NA) # required for BiasOpt/SigOpt option 5 and 6

# What are the rows for in these matrices? Columns are readers
# I believe in SigOpt, rows specify settings for different models
# For example, testing out constant CV, curvilinear SD, or curvilinear CV models
# BiasOpt.mat kept same across rows, because reader settings won't change
# However, looks like SigOpt row 1 is being overwritten, so not sure why BiasOpt needs 3 rows
BiasOpt.mat = SigOpt.mat = matrix(0,3,Nreaders)
BiasOpt.mat[1,] = c(2,0)
BiasOpt.mat[2,] = c(2,0)
BiasOpt.mat[3,] = c(2,0)
SigOpt.mat[1,] = c(1, 1)
SigOpt.mat[2,] = c(1, 2)
SigOpt.mat[3,] = c(1, 3)

model.aic <-as.data.frame(matrix(NA,3,4))
colnames(model.aic) <-c("Run","AIC","AICc","BIC")
model.name <-c("B2_S1_S1","B2_S1_S2","B2_S1_S3")


### Getting the following error when running:
### Error in ll(object) : argument "object" is missing, with no default

for(i in 1:3){
  setwd(mydir)
  DateFile = paste(getwd(),"/",model.name[i],"/",sep="")
  dir.create(DateFile)
  BiasOpt = BiasOpt.mat[i,]
  SigOpt = SigOpt.mat[i,]
  RunFn(Data=Reads2,
        SigOpt=SigOpt,
        KnotAges=KnotAges,
        BiasOpt=BiasOpt,
        NDataSets=1,
        MinAge=MinAge,
        MaxAge=MaxAge,
        RefAge=10, # what do you base your RefAge off of?
        MinusAge=0,
        PlusAge=10, # need to check that this is consistent w/ assessment
        SaveFile=DateFile,
        AdmbFile=SourceFile,
        EffSampleSize=0,
        Intern=FALSE,
        JustWrite=FALSE,
        CallType = "shell")
  
  info <-PlotOutputFn(Data=Reads2,MaxAge=MaxAge,SaveFile=DateFile,PlotType="PNG")
  Df = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%",what="character",quiet=TRUE)[6])
  Nll = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%",what="character",quiet=TRUE)[11])
  n = sum(ifelse(Reads2[,1]==-999,0,1))
  Aic = 2*Nll+2*Df
  Aicc = Aic+2*Df*(Df+1)/(n-Df-1)
  Bic = 2*Nll+Df*log(n)
  run.name<-strsplit(DateFile,"/")[[1]][9]
  model.aic[i,]<-c(run.name,Aic,Aicc,Bic)
}
  
save(model.aic,file=paste(getwd(),"/","model_selection.dmp",sep=""))
  
load(paste(getwd(),"/","model_selection.dmp",sep=""))
