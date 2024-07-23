#tests

##tests on time-space interference#######

dsa <- dsa %>%
  group_by(subj, group, task) %>%
  summarise(diff_irr = mean(diff_irr))

#young time vs space
t.test(dsa[dsa$group=="-0.5" & dsa$task == "-0.5",]$diff_irr,
       dsa[dsa$group=="-0.5" & dsa$task == "0.5",]$diff_irr, paired = TRUE)
#old time vs space
t.test(dsa[dsa$group=="0.5" & dsa$task == "-0.5",]$diff_irr,
       dsa[dsa$group=="0.5" & dsa$task == "0.5",]$diff_irr, paired = TRUE)
#space young vs old
t.test(dsa[dsa$group=="-0.5" & dsa$task == "0.5",]$diff_irr,
       dsa[dsa$group=="0.5" & dsa$task == "0.5",]$diff_irr)
#time young vs old
t.test(dsa[dsa$group=="-0.5" & dsa$task == "-0.5",]$diff_irr,
       dsa[dsa$group=="0.5" & dsa$task == "-0.5",]$diff_irr)


###corr between tsi and sti for old and young
#young
cor.test(dsa[dsa$group=="-0.5" & dsa$task == "-0.5",]$diff_irr,
         dsa[dsa$group=="-0.5" & dsa$task == "0.5",]$diff_irr)
#old
cor.test(dsa[dsa$group=="0.5" & dsa$task == "-0.5",]$diff_irr,
         dsa[dsa$group=="0.5" & dsa$task == "0.5",]$diff_irr)


#tests on reproductions###############################

###remove data from the participants who only have data from 2D
all_data <- all_data[all_data$id != 1037, ]
all_data <- all_data[all_data$id != 1028, ]

all_data <- all_data %>%
  group_by(id, age, presentation, task) %>%
  summarise(mean_ratio = mean(ratio))

##old 2d vs 3d for time
t.test(all_data[all_data$age=="old"& all_data$task=="time" &all_data$presentation=="VR",]$mean_ratio,
       all_data[all_data$age=="old"& all_data$task=="time" &all_data$presentation=="desktop",]$mean_ratio ,
       paired = TRUE, na.omit = TRUE)
#old, 2d vs 3d for space
t.test(all_data[all_data$age=="old"& all_data$task=="space" &all_data$presentation=="VR",]$mean_ratio,
       all_data[all_data$age=="old"& all_data$task=="space" &all_data$presentation=="desktop",]$mean_ratio ,
       paired = TRUE, na.omit = TRUE)

#young 2d vs 3d for time
t.test(all_data[all_data$age=="young"& all_data$task=="time" &all_data$presentation=="VR",]$mean_ratio,
       all_data[all_data$age=="young"& all_data$task=="time" &all_data$presentation=="desktop",]$mean_ratio ,
       paired = TRUE, na.omit = TRUE)

#young 2d vs 3d for space
t.test(all_data[all_data$age=="young"& all_data$task=="space" &all_data$presentation=="VR",]$mean_ratio,
       all_data[all_data$age=="young"& all_data$task=="space" &all_data$presentation=="desktop",]$mean_ratio ,
       paired = TRUE, na.omit = TRUE)




##################### now for the cognitive tasks
#demo vlmt data
demo<-read.table( "demo.txt", sep = "\t", header = TRUE)
#nback data
load("RData")
nback<-as.data.frame(cbind(tsi12_nback_res$subj,tsi12_nback_res$dprime))
names(nback)<- c("subj","nback")

df_all<- merge(dsa, nback, by = "subj")
df_all<- merge(df_all, demo, by = "subj") 
df_all$SumVLMT<- as.numeric(df_all$SumVLMT)

#split for old and young
young<-df_all[df_all$group == -0.5,]
old<-df_all[df_all$group == 0.5,]

#summarise data
old <- old %>%
  group_by(subj, task, nback, SumVLMT) %>%
  summarise(
    mean_diff_irr = mean(diff_irr),
  )


young <- young %>%
  group_by(subj, task, nback, SumVLMT) %>%
  summarise(
    mean_diff_irr = mean(diff_irr),
  )

######SPACE
# Perform correlation tests
cor_test_1s <- cor.test(old[old$task == 0.5,]$mean_diff_irr, old[old$task == 0.5,]$SumVLMT)
cor_test_2s <- cor.test(young[young$task == "0.5",]$mean_diff_irr, young[young$task == "0.5",]$SumVLMT)
cor_test_3s <- cor.test(old[old$task == "0.5",]$mean_diff_irr, old[old$task == "0.5",]$nback)
cor_test_4s <- cor.test(young[young$task == "0.5",]$mean_diff_irr, young[young$task == "0.5",]$nback)

cor_test_1s
cor_test_2s
cor_test_3s
cor_test_4s


######TIME
# Perform correlation tests
cor_test_1t <- cor.test(old[old$task == "-0.5",]$mean_diff_irr, old[old$task == "-0.5",]$SumVLMT)
cor_test_2t <- cor.test(young[young$task == "-0.5",]$mean_diff_irr, young[young$task == "-0.5",]$SumVLMT)
cor_test_3t <- cor.test(old[old$task == "-0.5",]$mean_diff_irr, old[old$task == "-0.5",]$nback)
cor_test_4t <- cor.test(young[young$task == "-0.5",]$mean_diff_irr, young[young$task == "-0.5",]$nback)

cor_test_1t
cor_test_2t
cor_test_3t
cor_test_4t






#######
##tests for cv 


#young time vs space
t.test(all_cv[all_cv$group=="-0.5" & all_cv$task == "-0.5",]$cv,
       all_cv[all_cv$group=="-0.5" & all_cv$task == "0.5",]$cv, paired = TRUE)

#old time vs space
t.test(all_cv[all_cv$group=="0.5" & all_cv$task == "-0.5",]$cv,
       all_cv[all_cv$group=="0.5" & all_cv$task == "0.5",]$cv, paired = TRUE)
#space young vs old
t.test(all_cv[all_cv$group=="-0.5" & all_cv$task == "0.5",]$cv,
       all_cv[all_cv$group=="0.5" & all_cv$task == "0.5",]$cv)
#time young vs old
t.test(all_cv[all_cv$group=="-0.5" & all_cv$task == "-0.5",]$cv,
       all_cv[all_cv$group=="0.5" & all_cv$task == "-0.5",]$cv)


all_cv <- all_cv[all_cv$id != 1037, ]
all_cv <- all_cv[all_cv$id != 1028, ]
#young time hmd desk
t.test(all_cv[all_cv$group=="-0.5" & all_cv$task == "-0.5"& all_cv$pres == "-0.5",]$cv,
       all_cv[all_cv$group=="-0.5" & all_cv$task == "-0.5"& all_cv$pres == "0.5",]$cv, paired = TRUE)
#young space hmd desk
t.test(all_cv[all_cv$group=="-0.5" & all_cv$task == "0.5"& all_cv$pres == "-0.5",]$cv,
       all_cv[all_cv$group=="-0.5" & all_cv$task == "0.5"& all_cv$pres == "0.5",]$cv, paired = TRUE)
#old time hmd desk
t.test(all_cv[all_cv$group=="0.5" & all_cv$task == "-0.5"& all_cv$pres == "-0.5" ,]$cv,
       all_cv[all_cv$group=="0.5" & all_cv$task == "-0.5"& all_cv$pres == "0.5",]$cv, paired = TRUE)
#old space hmd desk
t.test(all_cv[all_cv$group=="0.5" & all_cv$task == "0.5"& all_cv$pres == "-0.5" ,]$cv,
       all_cv[all_cv$group=="0.5" & all_cv$task == "0.5"& all_cv$pres == "0.5",]$cv, paired = TRUE)


