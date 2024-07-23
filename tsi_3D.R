#read data
combined_data<-read.table("tsi2.txt", sep = "\t", header = TRUE)


#Mark age
combined_data$age <- ifelse(nchar(combined_data$subjectNumber) > 2, "old", "young")


####### Mark task type
#### st0 -> space task
#### st1 -> time task
dsaVR_st0 <- combined_data[combined_data$conditionST==1,]
dsaVR_st1 <- combined_data[combined_data$conditionST==2,]




######CLEAN DATA########### using MAD
#####SPACE #############
#### 3040 --> 2933
dsaVR_st0_roomSizeSL0 <- subset(dsaVR_st0, roomSizeSL == 1)
dsaVR_st0_roomSizeSL1 <- subset(dsaVR_st0, roomSizeSL == 2)

remove_outliers <- function(data) {
  unique_subjects <- unique(data$subjectNumber)
  for (subject in unique_subjects) {
    subset_data <- data$reproChangeAbs[data$subjectNumber == subject]
    outliers <- unlist(outliers_mad(subset_data, threshold = 3))
    data <- data[!(data$subjectNumber == subject & data$reproChangeAbs %in% outliers), ]
  }
  return(data)
}


dsaVR_st0_roomSizeSL0_cleaned <- remove_outliers(dsaVR_st0_roomSizeSL0)
dsaVR_st0_roomSizeSL1_cleaned <- remove_outliers(dsaVR_st0_roomSizeSL1)
dsaVR_st0 <- rbind(dsaVR_st0_roomSizeSL0_cleaned, dsaVR_st0_roomSizeSL1_cleaned)
rownames(dsaVR_st0) <- NULL
rm(dsaVR_st0_roomSizeSL0,dsaVR_st0_roomSizeSL1,dsaVR_st0_roomSizeSL0_cleaned,dsaVR_st0_roomSizeSL1_cleaned)



##### TIME ############
######## 3040 -> 2955
dsaVR_st1_timeSL0 <- subset(dsaVR_st1, timeSL == 1)
dsaVR_st1_timeSL1 <- subset(dsaVR_st1, timeSL == 2)


remove_outliers <- function(data) {
  unique_subjects <- unique(data$subjectNumber)
  for (subject in unique_subjects) {
    subset_data <- data$reproductionTime[data$subjectNumber == subject]
    outliers <- unlist(outliers_mad(subset_data, threshold = 3))
    data <- data[!(data$subjectNumber == subject & data$reproductionTime %in% outliers), ]
  }
  return(data)
}

dsaVR_st1_timeSL0_cleaned <- remove_outliers(dsaVR_st1_timeSL0)
dsaVR_st1_timeSL1_cleaned <- remove_outliers(dsaVR_st1_timeSL1)
dsaVR_st1 <- rbind(dsaVR_st1_timeSL0_cleaned, dsaVR_st1_timeSL1_cleaned)
rownames(dsaVR_st1) <- NULL
rm(dsaVR_st1_timeSL0,dsaVR_st1_timeSL1,dsaVR_st1_timeSL0_cleaned,dsaVR_st1_timeSL1_cleaned)




################################################################
#######AGGREGATE TIME DATA  

#calculate ratio
dsaVR_st1$stim_time_numeric <- ifelse(dsaVR_st1$timeSL == 1, 4, 8)
dsaVR_st1$ratioTime <-dsaVR_st1$reproductionTime/dsaVR_st1$stim_time_numeric 

dsa_agg1_Vr = aggregate(dsaVR_st1$ratioTime,
                        by=list(dsaVR_st1$subjectNumber,
                                dsaVR_st1$age,
                                dsaVR_st1$timeSL,
                                dsaVR_st1$roomSizeSL),
                        mean, na.rm=T)
colnames(dsa_agg1_Vr) <- c("subjectNumber", "age", "timeSL", "roomSizeSL", "ratioTime")



####AGGREGATE SPACE DATA

#calculate ratio
dsaVR_st0$stim_space_numeric <- ifelse(dsaVR_st0$roomSizeSL == 1, 4, 7)
dsaVR_st0$ratioSpace <-dsaVR_st0$reproductionEndWall2/dsaVR_st0$roomShowSpaceWall2

dsa_agg0_Vr = aggregate(dsaVR_st0$ratioSpace,
                        by=list(dsaVR_st0$subjectNumber,
                                dsaVR_st0$age,
                                dsaVR_st0$timeSL,
                                dsaVR_st0$roomSizeSL),
                        mean, na.rm=T)
colnames(dsa_agg0_Vr) <- c("subjectNumber", "age", "timeSL", "roomSizeSL","cv_space")
rm(combined_data)





