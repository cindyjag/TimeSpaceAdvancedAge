##plots
#fig 2

SpaceTime <- c("space task", "time task")
names(SpaceTime) <- c(0.5, -0.5)

ggplot(data = dsa, aes(x = as.factor(task), y = diff_irr, fill = as.factor(group ))) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  # facet_wrap(~task , labeller = as_labeller(SpaceTime))+
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        legend.position = "top",
        legend.key.size = unit(2, 'cm')) +
  stat_summary(
    fun.data = function(x) {
      ymin = mean(x) - sd(x) / sqrt(length(x))
      ymax = mean(x) + sd(x) / sqrt(length(x))
      data.frame(ymin = ymin, ymax = ymax)
    },
    geom = "errorbar",
    width = 0.2,
    size = 1,
    position = position_dodge(width = 0.9),
    color = "skyblue4" ) +
  coord_cartesian(ylim = c(0,0.5))+
  labs(title = "", x = "", y = "interference effect", fill = "age group") +
  scale_x_discrete(labels = c("space-on-time-effect", "time-on-space effect")) +
  scale_fill_manual(values = c( "darkseagreen4","aquamarine3"), labels = c ( "young", "old"))


#fig 3a 3b

##3a
all_data_plot$task <- factor(all_data_plot$task, levels = c("time", "space"))
all_data_plot$age <- factor(all_data_plot$age, levels = c("young", "old"))


SpaceTime <- c("time task", "space task")
names(SpaceTime) <- c("time", "space")
###############################################################
rat<-ggplot(data = all_data_plot, aes(x = presentation, y = mean_ratio, fill = age)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  facet_wrap(~task, labeller = as_labeller(SpaceTime) )+
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        legend.position = "top",
        legend.key.size = unit(2, 'cm')) +
  stat_summary(
    fun.data = function(x) {
      ymin = mean(x) - sd(x) / sqrt(length(x))
      ymax = mean(x) + sd(x) / sqrt(length(x))
      data.frame(ymin = ymin, ymax = ymax)
    },
    geom = "errorbar",
    width = 0.2,
    size = 1,
    position = position_dodge(width = 0.9),
    color = "skyblue4" ) +
  #coord_cartesian(ylim = c(0,0.5))+
  labs(title = "", x = "", y = "standardized ratio", fill = "age group") +
  scale_x_discrete(labels = c("", "")) +
  scale_fill_manual(values = c( "darkseagreen4","aquamarine3"),labels = c ( "young", "old"))


##3b
all_cv_plot = all_cv
all_cv_plot$task <- factor(all_cv_plot$task, levels = c("time", "space"))



SpaceTime <- c("space task", "time task")
names(SpaceTime) <- c(0.5, -0.5)

cov<-ggplot(data = all_cv, aes(x = as.factor(pres), y = cv, fill = as.factor(group ))) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  facet_wrap(~task , labeller = as_labeller(SpaceTime))+
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        legend.position = "none") +
  stat_summary(
    fun.data = function(x) {
      ymin = mean(x) - sd(x) / sqrt(length(x))
      ymax = mean(x) + sd(x) / sqrt(length(x))
      data.frame(ymin = ymin, ymax = ymax)
    },
    geom = "errorbar",
    width = 0.2,
    size = 1,
    position = position_dodge(width = 0.9),
    color = "skyblue4" ) +
  coord_cartesian(ylim = c(0,0.5))+
  labs(title = "", x = "", y = "coefficient of variation", fill = "age group") +
  scale_x_discrete(labels = c("2D task", "3D task")) +
  scale_fill_manual(values = c( "darkseagreen4","aquamarine3"))

rat + cov + plot_layout(ncol = 1)






##fig4
df_all <- df_all %>%
  group_by(subj, task, nback, SumVLMT) %>%
  summarise(
    diff_irr = mean(diff_irr),
  )

df_all$age<- ifelse(df_all$subj< 900, "young","old")
df_all$age <- factor(df_all$age, levels = c("young","old" ))

vl<-ggplot(df_all[df_all$task == "-0.5",], aes(x = SumVLMT, y = diff_irr)) +
  geom_point() +
  facet_wrap(~age)+
  geom_smooth(aes(color=age),method = "lm", se = FALSE, size = 2) +
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24  ),
        legend.position = "none")+
  scale_color_manual(values = c("young" = "darkseagreen4",  "old" = "aquamarine3"))+
  labs(title = "",x = "", y = "space-on-time effect")


###

nb<-ggplot(df_all[df_all$task == "-0.5",], aes(x = nback, y = diff_irr)) +
  geom_point() +
  facet_wrap(~age)+
  geom_smooth(aes(color= age),method = "lm", se = FALSE, size = 2) +
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24  ),
        legend.position = "none")+
  scale_color_manual(values = c("young" = "darkseagreen4",  "old" = "aquamarine3"))+
  labs(title = "",x = "", y = "")



vls<-ggplot(df_all[df_all$task == "0.5",], aes(x = SumVLMT, y = diff_irr)) +
  geom_point() +
  facet_wrap(~age )+
  geom_smooth(aes(color=age),method = "lm", se = FALSE, size = 2) +
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24  ),
        legend.position = "none")+
  scale_color_manual(values = c("young" = "darkseagreen4",  "old" = "aquamarine3"))+
  labs(title = "",x = "VLMT score", y = "time-on-space effect")


###

nbs<-ggplot(df_all[df_all$task == "0.5",], aes(x = nback, y = diff_irr)) +
  geom_point() +
  facet_wrap(~age)+
  geom_smooth(aes(color= age),method = "lm", se = FALSE, size = 2) +
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24  ),
        legend.position = "none")+
  scale_color_manual(values = c("young" = "darkseagreen4",  "old" = "aquamarine3"))+
  labs(title = "",x = "2-back score", y = "")



vlmt<- ggplot(data = df_all, aes(x = age, y = SumVLMT, fill = age)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24)) +
  stat_summary(
    fun.data = function(x) {
      ymin = mean(x) - sd(x) / sqrt(length(x))
      ymax = mean(x) + sd(x) / sqrt(length(x))
      data.frame(ymin = ymin, ymax = ymax)
    },
    geom = "errorbar",
    width = 0.2,
    size = 1,
    position = position_dodge(width = 0.9),
    color = "skyblue4" ) +
  guides(fill = FALSE) +
  coord_cartesian(ylim =c(40,70))+
  labs(title = "", x = "age group", y = "VLMT score") +
  scale_x_discrete(labels = c("young", "old")) +
  scale_fill_manual(values = c( "darkseagreen4", "aquamarine3"))

nbac<- ggplot(data = df_all, aes(x = age, y = nback, fill = age)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  theme(axis.text = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.title = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24)) +
  stat_summary(
    fun.data = function(x) {
      ymin = mean(x) - sd(x) / sqrt(length(x))
      ymax = mean(x) + sd(x) / sqrt(length(x))
      data.frame(ymin = ymin, ymax = ymax)
    },
    geom = "errorbar",
    width = 0.2,
    size = 1,
    position = position_dodge(width = 0.9),
    color = "skyblue4" ) +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(2,5))+
  labs(title = "", x = "age group", y = "2-back score") +
  scale_x_discrete(labels = c("young", "old")) +
  scale_fill_manual(values = c("darkseagreen4",  "aquamarine3"))



vlmt + nbac  + vl + nb + vls + nbs + plot_layout(ncol = 2)

