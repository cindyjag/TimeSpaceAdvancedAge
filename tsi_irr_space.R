valid_ids <- unique(all_data$id)
# Funktion zur Berechnung der Differenz von irr0 und irr1 mit Originalverhältnissen
calculate_diff <- function(data, presentation_val, relevant_val, task_val) {
  temp <- data %>%
    filter(presentation == presentation_val, relevant == relevant_val, task == task_val) %>%
    group_by(id) %>%
    summarise(
      diff_irr = sum(ifelse(irrelevant == 0, ratio, -ratio)),
      ratio1 = sum(ifelse(irrelevant == 0, ratio, 0)),
      ratio2 = sum(ifelse(irrelevant == 0, 0, ratio)),
      .groups = "drop"
    ) %>%
    mutate(presentation = presentation_val, relevant = relevant_val, task = task_val)
  
  return(temp)
}



# Berechnung der Differenzen und Speicherung in separaten DataFrames
interf_df <- bind_rows(lapply(valid_ids, function(i) calculate_diff(all_data, "VR", 0, "space")))
interf_df_ <- bind_rows(lapply(valid_ids, function(i) calculate_diff(all_data, "VR", 1, "space")))
interf_dot_df <- bind_rows(lapply(valid_ids, function(i) calculate_diff(all_data, "desktop", 0, "space")))
interf_s_df <- bind_rows(lapply(valid_ids, function(i) calculate_diff(all_data, "desktop", 1, "space")))



# Benennung der Spalten
colnames(interf_df) <- colnames(interf_df_) <- colnames(interf_dot_df) <- colnames(interf_s_df) <- c("subj", "diff_irr", "ratio1", "ratio2", "presentation", "rel", "task")


#SUMMARISING

interf_df <- interf_df %>%
  group_by(subj, presentation, rel, task) %>%
  summarise(
    diff_irr = mean(diff_irr),
    ratio1 = mean(ratio1),
    ratio2 = mean(ratio2),
    mean_ratio =  mean(c(ratio1, ratio2))
  )

interf_df_ <- interf_df_ %>%
  group_by(subj, presentation, rel, task) %>%
  summarise(
    diff_irr = mean(diff_irr),
    ratio1 = mean(ratio1),
    ratio2 = mean(ratio2),
    mean_ratio =  mean(c(ratio1, ratio2))
  )

interf_dot_df <- interf_dot_df %>%
  group_by(subj, presentation, rel, task) %>%
  summarise(
    diff_irr = mean(diff_irr),
    ratio1 = mean(ratio1),
    ratio2 = mean(ratio2),
    mean_ratio =  mean(c(ratio1, ratio2))
  )

interf_s_df <- interf_s_df %>%
  group_by(subj, presentation, rel, task) %>%
  summarise(
    diff_irr = mean(diff_irr),
    ratio1 = mean(ratio1),
    ratio2 = mean(ratio2),
    mean_ratio =  mean(c(ratio1, ratio2))
  )


combined_diff_s <- rbind(interf_df, interf_df_, interf_dot_df, interf_s_df)


# Gruppieren und Zusammenfassen für combined_diff_info_t
combined_diff_info_s <- combined_diff_s %>%
  group_by(subj, presentation, rel, task) %>%
  summarise(
    diff_irr = mean(diff_irr),
    mean_ratio = mean(mean_ratio),
  )

# Gruppieren und Zusammenfassen für combined_diff_t
combined_diff_s <- combined_diff_s %>%
  group_by(subj) %>%
  summarise(
    diff_irr = mean(diff_irr),
    mean_ratio = mean(mean_ratio),
  )


unique(combined_diff_s$subj)

rm(interf_df, interf_df_, interf_dot_df, interf_s_df)
