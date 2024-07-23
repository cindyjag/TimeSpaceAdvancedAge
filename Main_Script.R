#this script prepares the data and computes the lme


#run script to load and clean 2D data
source("C:/Users/cindy/Desktop/tsi_2D.R")
#run script to load and clean 3D data
source("C:/Users/cindy/Desktop/tsi_3D.R")
#run script to compute cv data, look into script for lmer
source("C:/Users/cindy/Desktop/cv_tsi.R")
#run script to combine 2D and 3D data
source("C:/Users/cindy/Desktop/2D_3D.R")

#save for a later plot:
all_data_plot <- all_data%>%
  group_by(id, task, presentation, age) %>%
  summarise(
    mean_ratio = mean(ratio),
  )


##### now run the script for calculating the time-on-space effect
source("C:/Users/cindy/Desktop/Projects_PHD/TSI/tsi_irr_space.R")


##### and the script for calculating the space-on-time effect
source("C:/Users/cindy/Desktop/Projects_PHD/TSI/tsi_irr_time.R")


####combine them
combined_diff_info_s$task <- "space"
combined_diff_info_t$task <- "time"
all_combined<-rbind(combined_diff_info_s, combined_diff_info_t) 

###turn around values (so that higher values equal higher interference)
all_combined$diff_irr <- all_combined$diff_irr * -1
all_combined$age<-ifelse(all_combined$subj < 900, "young","old")




##now prepare data for lme
dsa = all_combined



### recode variables
dsa$pres = ifelse(dsa$presentation=="desktop", -0.5, 0.5)
dsa$presentation = NULL
dsa$group = ifelse(dsa$age=="young", -0.5, 0.5)
dsa$age = NULL
dsa$task = ifelse(dsa$task=="time", -0.5, 0.5)
dsa$rel = ifelse(dsa$rel==0, -0.5, 0.5)

### sort dataset
dsa = dsa[order(dsa$rel),]
dsa = dsa[order(dsa$task),]
dsa = dsa[order(dsa$pres),]
dsa = dsa[order(dsa$subj),]


upper_crit = mean(dsa$diff_irr)+5*sd(dsa$diff_irr)
lower_crit = mean(dsa$diff_irr)-5*sd(dsa$diff_irr)

#see which data points
dsa[dsa$diff_irr<lower_crit | dsa$diff_irr>upper_crit,]
#both from the same person. questionable if this person acutally understood the task
#remove person
dsa<-dsa[dsa$subj != 1023,]

m1 = lmer(diff_irr ~ pres*task*group + (1 | subj), dsa); summary(m1)
tab_model(m1)







#####################################



