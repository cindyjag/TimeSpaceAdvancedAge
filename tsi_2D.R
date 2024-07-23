####This Script is for loading and cleaning the data of the 2D task

library(gridExtra)
library(dplyr)
library(ggplot2)
library(car)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(Routliers)
library(dplyr)
library(lme4)
library(Rcpp)
library(cowplot) 
library(sjPlot)
library(sjmisc) 
library(effects)
library(sjstats)
library(simr)
library(ggpubr)
library(rlang)
library(plotrix)
library(Hmisc)
library(gghalves)
library(tidyquant)
library(ggdist)
library(ggthemes)
library(lme4)
library(jsonlite)
library(stringr)
library(ggstatsplot)
library(readxl)
library(ggpubr)
library(plotly)
library(patchwork)
library(EnvStats)
library(scales)
library(ggpattern)
library(BayesFactor)
library(bayestestR)

###Read Data
dsa<-read.table("tsi1.txt", sep = "\t", header = TRUE)

#Mark Age
dsa$age <- ifelse(nchar(dsa$id) > 2, "old", "young")


########Mark Task Type
#### st0 -> space task
#### st1 -> time task


dsa_st0 <- dsa[dsa$st==0,]
dsa_st1 <- dsa[dsa$st==1,]





######CLEAN DATA########### using MAD
##### SPACE TASK #########
########## 3110 -> 3050
dsa_st0_space0 <- subset(dsa_st0, stim_space == 0)
dsa_st0_space1 <- subset(dsa_st0, stim_space == 1)

remove_outliers <- function(data) {
  unique_ids <- unique(data$id)
  for (id in unique_ids) {
    subset_data <- data$repro_size[data$id == id]
    outliers <- unlist(outliers_mad(subset_data, threshold = 3))
    data <- data[!(data$id == id & data$repro_size %in% outliers), ]
  }
  return(data)
}


dsa_st0_space0_cleaned <- remove_outliers(dsa_st0_space0)
dsa_st0_space1_cleaned <- remove_outliers(dsa_st0_space1)
dsa_st0 <- rbind(dsa_st0_space0_cleaned, dsa_st0_space1_cleaned)
rownames(dsa_st0) <- NULL
rm(dsa_st0_space1,dsa_st0_space0, dsa_st0_space0_cleaned,dsa_st0_space1_cleaned )

##### TIME ##########
######## 3110 -> 3007
dsa_st1_time0 <- subset(dsa_st1, stim_time == 0)
dsa_st1_time1 <- subset(dsa_st1, stim_time == 1)


remove_outliers <- function(data) {
  unique_ids <- unique(data$id)
  for (id in unique_ids) {
    subset_data <- data$repro_time[data$id == id]
    outliers <- unlist(outliers_mad(subset_data, threshold = 3))
    data <- data[!(data$id == id & data$repro_time %in% outliers), ]
  }
  return(data)
}


dsa_st1_time0_cleaned <- remove_outliers(dsa_st1_time0)
dsa_st1_time1_cleaned <- remove_outliers(dsa_st1_time1)
dsa_st1 <- rbind(dsa_st1_time0_cleaned, dsa_st1_time1_cleaned)
rownames(dsa_st1) <- NULL
rm(dsa_st1_time0_cleaned,dsa_st1_time1_cleaned,dsa_st1_time0,dsa_st1_time1)



###############################################################################
##### AGGREGATE SPACE DATA 
dsa_agg0 = aggregate(dsa_st0$repro_size,
                     by=list(dsa_st0$stim_time,
                     dsa_st0$id, dsa_st0$age,
                     dsa_st0$stim_space),
                     mean, na.rm=T)
colnames(dsa_agg0) <- c("stim_time","id", "age", "stim_space", "repro_size")

##### AGGREGATE TIME DATA
dsa_agg1 = aggregate(dsa_st1$repro_time,
                     by=list(dsa_st1$stim_time,
                             dsa_st1$id, dsa_st1$age,
                             dsa_st1$stim_space),
                     mean, na.rm=T)
colnames(dsa_agg1) <- c("stim_time","id", "age", "stim_space", "repro_time")




#####CALCULATE RATIOS 
#TIME
dsa_agg1$stim_time_numeric <- ifelse(dsa_agg1$stim_time == 0, 4, 8)
dsa_agg1$ratioTime <-dsa_agg1$repro_time/dsa_agg1$stim_time_numeric 


#SPACE
dsa_agg0$stim_space_numeric <- ifelse(dsa_agg0$stim_space == 0, 4, 7)
dsa_agg0$ratioSpace <-dsa_agg0$repro_size/dsa_agg0$stim_space_numeric 




rm(dsa)


