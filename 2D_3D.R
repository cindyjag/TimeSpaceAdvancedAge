############## 3D Data
stand_dsa1 <- dsa_agg1_Vr
stand_dsa0 <- dsa_agg0_Vr 

#mark vr
stand_dsa0$presentation<- "VR"
stand_dsa1$presentation<- "VR"

#rename
names(stand_dsa0)<- c("id", "age", "stim_time", "stim_space", "ratio", "presentation")
names(stand_dsa1)<- c("id", "age", "stim_time", "stim_space", "ratio", "presentation")

stand_dsa0$stim_space <- ifelse(stand_dsa0$stim_space == 1, 0, 1)
stand_dsa0$stim_time <- ifelse(stand_dsa0$stim_time == 1, 0, 1)

stand_dsa1$stim_space <- ifelse(stand_dsa1$stim_space == 1, 0, 1)
stand_dsa1$stim_time <- ifelse(stand_dsa1$stim_time == 1, 0, 1)

############## 2D data
standD_dsa1 <- dsa_agg1 
standD_dsa0 <- dsa_agg0 

####correct some ids
standD_dsa0$id <- as.numeric(sprintf("%s", standD_dsa0$id))
standD_dsa1$id <- as.numeric(sprintf("%s", standD_dsa1$id))


########remove cols i dont need
standD_dsa0$groups <- NULL
standD_dsa0$repro_size <- NULL
standD_dsa0$stim_space_numeric <- NULL

standD_dsa1$groups <- NULL
standD_dsa1$repro_time <- NULL
standD_dsa1$stim_time_numeric <- NULL

####mark desktop
standD_dsa0$presentation <- "desktop"
standD_dsa1$presentation <- "desktop"

names(standD_dsa0)<- c("stim_time", "id", "age", "stim_space", "ratio", "presentation")
names(standD_dsa1)<- c("stim_time", "id", "age", "stim_space", "ratio", "presentation")





###merge space tasks
space_data<-rbind(stand_dsa0, standD_dsa0)
space_data1 <- rbind(stand_dsa0, standD_dsa0)
space_data<- space_data%>% mutate_at(c('ratio'), ~(scale(.) %>% as.vector))
##mark relevant and irrelevant
space_data$irrelevant<- space_data$stim_time
space_data$relevant <- space_data$stim_space



###merge time tasks
time_data<- rbind(stand_dsa1, standD_dsa1)
time_data1<- rbind(stand_dsa1, standD_dsa1)
time_data <- time_data%>% mutate_at(c('ratio'), ~(scale(.) %>% as.vector))
##mark relevant and irrelevant
time_data$irrelevant <- time_data$stim_space
time_data$relevant <- time_data$stim_time



################################################
##merge
space_data$task<- "space"
time_data$task<- "time"


all_data<- rbind(space_data,time_data)


rm(dsa_st1,dsa_st0,dsa_agg1_Vr, dsa_agg0_Vr, dsa_agg0, dsa_agg1, dsaVR_st0,dsaVR_st1,
   space_data, space_data1, time_data, time_data1, standD_dsa0, stand_dsa1,
   standD_dsa1, stand_dsa0)

