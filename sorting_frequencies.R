# Read in full dataset
full <- read.csv("/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/full_dataset.csv", header=TRUE, as.is=T)

# Remove 90s and 99s from full dataset; 9s have to be removed column-by-column
full[full == 99] <- NA
full[full >= 90] <- NA

# EXAMPLE: Remove 9s from specific variable
full[full$Q06 == 9 | is.na(full$Q06), "Q06"] =  NA

# EXAMPLE: Apply weighting
days_unwell_wted <- group_by(full, study, Q55, Q34_CAID, Q34_NO) %>%
  summarise(
    count = n(),
    mean_num_days_active = weighted.mean(x = Q19a, w = WT, na.rm = TRUE), 
    sd = sd(Q19a, na.rm = TRUE),
  )

# EXAMPLE: Compare unweighted vs. weighted means by identifying specific variables
days_unwell$mean_num_days_active - days_unwell_wted$mean_num_days_active

# All crosstabs should have a weight applied

# CROSSTAB 1: Insurance Status x Medical Home
ins_medical_home <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q01 = mean(Q01, na.rm = T),
    sd = sd(Q01, na.rm = TRUE)
  )

# WEIGHTED CROSSTAB 1:
ins_medical_home_wt <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q01 = weighted.mean(x = Q01, w = WT, na.rm = T),
    sd = sd(Q01, na.rm = TRUE)
  )

### Most of my variables are categorical - how do I deal with that? ###

# CROSSTAB 2: Insurance Status (none, Medicaid, employer) x Number of ER Visits (Continuous)
ins_er_number <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q02Number = weighted.mean(x = Q02Number, w = WT, na.rm = T),
    sd = sd(Q02Number, na.rm = TRUE)
  )

# Write Crosstab 2 CSV
write.csv(ins_er_number, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_er_number.csv", quote = F, row.names = F)

#CROSSTAB 3: Insurance Status x Number of PCP Visits
ins_pcp_times <- group_by(full, study, Q34_NO, Q34_CAID,Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q04 = weighted.mean(x = Q04, w = WT, na.rm = T),
    sd = sd(Q04, na.rm = TRUE)
  )

# Write Crosstab 3 CSV
write.csv(ins_pcp_times, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_pcp_times.csv", quote = F, row.names = F)

#CROSSTAB 4: HEALTHY DAYS: Insurance Status x Poor Physical Health
ins_poor_physical <- group_by(full, study, Q34_NO, Q34_CAID,Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q19a = weighted.mean(x = Q19a, w = WT, na.rm = T),
    sd = sd(Q19a, na.rm = TRUE)
  )

view(ins_poor_physical)

ins_poor_physical_mcaidvsunins <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q19a = weighted.mean(x = Q19a, w = WT, na.rm = T),
    sd = sd(Q19a, na.rm = TRUE)
  )

write.csv(ins_poor_physical_mcaidvsunins, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_poor_physical_mcaidvsunins.csv", quote = F, row.names = F)

ins_poor_physical_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q19a = weighted.mean(x = Q19a, w = WT, na.rm = T),
    sd = sd(Q19a, na.rm = TRUE)
  )

write.csv(ins_poor_physical_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_poor_physical_totalpop.csv", quote = F, row.names = F)

# Write Crosstab 4 CSV
write.csv(ins_poor_physical, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_poor_physical.csv", quote = F, row.names = F)

# CROSSTAB 5: HEALTHY DAYS: Insurance Status x Physical limitations on activity
ins_physical_limited_activity <- group_by(full, study, Q34_NO, Q34_CAID,Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q19b = weighted.mean(x = Q19b, w = WT, na.rm = T),
    sd = sd(Q19b, na.rm = TRUE)
  )

view(ins_physical_limited_activity)

ins_limited_physical_mcaidvsunins <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q19b = weighted.mean(x = Q19b, w = WT, na.rm = T),
    sd = sd(Q19b, na.rm = TRUE)
  )

write.csv(ins_limited_physical_mcaidvsunins, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_limited_physical_mcaidvsunins.csv", quote = F, row.names = F)

ins_limited_physical_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q19b = weighted.mean(x = Q19b, w = WT, na.rm = T),
    sd = sd(Q19b, na.rm = TRUE)
  )

write.csv(ins_limited_physical_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_limited_physical_totalpop.csv", quote = F, row.names = F)

# Write Crosstab 5 CSV
write.csv(ins_physical_limited_activity, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_physical_limited_activity.csv", quote = F, row.names = F)

# CROSSTAB 6: HEALTHY DAYS: Insurance Status x Poor Mental Health
ins_poor_mental <- group_by(full, study, Q34_NO, Q34_CAID,Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q19c = weighted.mean(x = Q19c, w = WT, na.rm = T),
    sd = sd(Q19c, na.rm = TRUE)
  )

view(ins_poor_mental)

ins_poor_mental_mcaidvsunins <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q19c = weighted.mean(x = Q19c, w = WT, na.rm = T),
    sd = sd(Q19c, na.rm = TRUE)
  )

write.csv(ins_poor_mental_mcaidvsunins, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_poor_mental_mcaidvsunins.csv", quote = F, row.names = F)

ins_poor_mental_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q19c = weighted.mean(x = Q19c, w = WT, na.rm = T),
    sd = sd(Q19c, na.rm = TRUE)
  )

write.csv(ins_poor_mental_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_poor_mental_totalpop.csv", quote = F, row.names = F)


# Write Crosstab 6 CSV
write.csv(ins_poor_mental, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_poor_mental.csv", quote = F, row.names = F)

# CROSSTAB 7: HEALTHY DAYS: Insurance Status x Mental limitations on activity
ins_mental_limited_activity <- group_by(full, study, Q34_NO, Q34_CAID,Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q19d = weighted.mean(x = Q19d, w = WT, na.rm = T),
    sd = sd(Q19d, na.rm = TRUE)
  )

view(ins_mental_limited_activity)

# Write Crosstab 7 CSV
write.csv(ins_mental_limited_activity, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_mental_limited_activity.csv", quote = F, row.names = F)

ins_limited_mental_mcaidvsunins <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q19d = weighted.mean(x = Q19d, w = WT, na.rm = T),
    sd = sd(Q19d, na.rm = TRUE)
  )

write.csv(ins_limited_mental_mcaidvsunins, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_limited_mental_mcaidvsunins.csv", quote = F, row.names = F)

ins_limited_mental_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q19d = weighted.mean(x = Q19d, w = WT, na.rm = T),
    sd = sd(Q19d, na.rm = TRUE)
  )

write.csv(ins_limited_mental_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_limited_mental_totalpop.csv", quote = F, row.names = F)

# ER VISITS: Total Population
er_num_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q02Number = weighted.mean(x = Q02Number, w = WT, na.rm = T),
    sd = sd(Q02Number, na.rm = TRUE)
  )
view(er_num_totalpop)

write.csv(er_num_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/er_num_totalpop.csv", quote = F, row.names = F)

# ER Visits:  Uninsured 2013 vs Medicaid 2016
er_num_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q02Number = weighted.mean(x = Q02Number, w = WT, na.rm = T),
    sd = sd(Q02Number, na.rm = TRUE)
  )
view(er_num_uninsvsmcaid)

write.csv(er_num_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/er_num_uninsvsmcaid.csv", quote = F, row.names = F)

# REMOVE 8s and 9s from ordinal variables
full[full$Q08a == 9| is.na(full$Q08a), "Q08a"] =  NA
full[full$Q08a == 8| is.na(full$Q08a), "Q08a"] =  NA

full[full$Q08b == 9| is.na(full$Q08b), "Q08b"] =  NA
full[full$Q08b == 8| is.na(full$Q08b), "Q08b"] =  NA

full[full$Q08c == 9| is.na(full$Q08c), "Q08c"] =  NA
full[full$Q08c == 8| is.na(full$Q08c), "Q08c"] =  NA

full[full$Q08d == 9| is.na(full$Q08d), "Q08d"] =  NA
full[full$Q08d == 8| is.na(full$Q08d), "Q08d"] =  NA

full[full$Q08e == 9| is.na(full$Q08e), "Q08e"] =  NA
full[full$Q08e == 8| is.na(full$Q08e), "Q08e"] =  NA

full[full$Q09a == 9| is.na(full$Q09a), "Q09a"] =  NA
full[full$Q09a == 8| is.na(full$Q09a), "Q09a"] =  NA

full[full$Q09b == 9| is.na(full$Q09b), "Q09b"] =  NA
full[full$Q09b == 8| is.na(full$Q09b), "Q09b"] =  NA

full[full$Q09c == 9| is.na(full$Q09c), "Q09c"] =  NA
full[full$Q09c == 8| is.na(full$Q09c), "Q09c"] =  NA

full[full$Q09d == 9| is.na(full$Q09d), "Q09d"] =  NA
full[full$Q09d == 8| is.na(full$Q09d), "Q09d"] =  NA

full[full$Q09e == 9| is.na(full$Q09e), "Q09e"] =  NA
full[full$Q09e == 8| is.na(full$Q09e), "Q09e"] =  NA

# ORDINAL: Routine Checkup x total pop
checkup_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q08a = weighted.mean(x = Q08a, w = WT, na.rm = T),
    sd = sd(Q08a, na.rm = TRUE)
  )
view(checkup_totalpop)

write.csv(checkup_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/checkup_totalpop.csv", quote = F, row.names = F)

# Routine checkup x unins 2013 v Medicaid 2016
checkup_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q08a = weighted.mean(x = Q08a, w = WT, na.rm = T),
    sd = sd(Q08a, na.rm = TRUE)
  )
view(checkup_uninsvsmcaid)

write.csv(checkup_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/checkup_uninsvsmcaid.csv", quote = F, row.names = F)

# Dental exam total pop
dental_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q08b = weighted.mean(x = Q08b, w = WT, na.rm = T),
    sd = sd(Q08b, na.rm = TRUE)
  )
view(dental_totalpop)

write.csv(dental_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/dental_totalpop.csv", quote = F, row.names = F)

# Dental exam x unins 2013 v Medicaid 2016
dental_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q08b = weighted.mean(x = Q08b, w = WT, na.rm = T),
    sd = sd(Q08b, na.rm = TRUE)
  )
view(dental_uninsvsmcaid)

write.csv(dental_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/dental_uninsvsmcaid.csv", quote = F, row.names = F)

# Blood pressure total pop
bp_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q08c = weighted.mean(x = Q08c, w = WT, na.rm = T),
    sd = sd(Q08c, na.rm = TRUE)
  )
view(bp_totalpop)

write.csv(bp_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/bp_totalpop.csv", quote = F, row.names = F)

# Blood pressure x unins 2013 v Medicaid 2016
bp_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q08c = weighted.mean(x = Q08c, w = WT, na.rm = T),
    sd = sd(Q08c, na.rm = TRUE)
  )
view(bp_uninsvsmcaid)

write.csv(bp_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/bp_uninsvsmcaid.csv", quote = F, row.names = F)

# Cholesterol total pop
chol_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q08d = weighted.mean(x = Q08d, w = WT, na.rm = T),
    sd = sd(Q08d, na.rm = TRUE)
  )
view(chol_totalpop)

write.csv(chol_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/chol_totalpop.csv", quote = F, row.names = F)

# Cholesterol x unins 2013 v Medicaid 2016
chol_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q08d = weighted.mean(x = Q08d, w = WT, na.rm = T),
    sd = sd(Q08d, na.rm = TRUE)
  )
view(chol_uninsvsmcaid)

write.csv(chol_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/chol_uninsvsmcaid.csv", quote = F, row.names = F)

# Blood sugar total pop
sugar_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q08e = weighted.mean(x = Q08e, w = WT, na.rm = T),
    sd = sd(Q08e, na.rm = TRUE)
  )
view(sugar_totalpop)

write.csv(sugar_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/sugar_totalpop.csv", quote = F, row.names = F)

# Blood sugar x unins 2013 v Medicaid 2016
sugar_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q08e = weighted.mean(x = Q08e, w = WT, na.rm = T),
    sd = sd(Q08e, na.rm = TRUE)
  )
view(sugar_uninsvsmcaid)

write.csv(sugar_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/sugar_uninsvsmcaid.csv", quote = F, row.names = F)

# FOBT for Colon Cancer total pop
fobt_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q09a = weighted.mean(x = Q09a, w = WT, na.rm = T),
    sd = sd(Q09a, na.rm = TRUE)
  )
view(fobt_totalpop)

write.csv(fobt_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/fobt_totalpop.csv", quote = F, row.names = F)

# FOBT for Colon Cancer x unins 2013 v Medicaid 2016
fobt_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q09a = weighted.mean(x = Q09a, w = WT, na.rm = T),
    sd = sd(Q09a, na.rm = TRUE)
  )
view(fobt_uninsvsmcaid)

write.csv(fobt_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/fobt_uninsvsmcaid.csv", quote = F, row.names = F)

# Sigmoidoscopy or colonoscopy total pop
colonoscopy_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q09b = weighted.mean(x = Q09b, w = WT, na.rm = T),
    sd = sd(Q09b, na.rm = TRUE)
  )
view(colonoscopy_totalpop)

write.csv(colonoscopy_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/colonoscopy_totalpop.csv", quote = F, row.names = F)

# Sigmoidoscopy or colonoscopy x unins 2013 v Medicaid 2016
colonoscopy_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q09b = weighted.mean(x = Q09b, w = WT, na.rm = T),
    sd = sd(Q09b, na.rm = TRUE)
  )
view(colonoscopy_uninsvsmcaid)

write.csv(colonoscopy_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/colonoscopy_uninsvsmcaid.csv", quote = F, row.names = F)

# Prostate cancer screening total pop
prostate_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q09c = weighted.mean(x = Q09c, w = WT, na.rm = T),
    sd = sd(Q09c, na.rm = TRUE)
  )
view(prostate_totalpop)

write.csv(prostate_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/prostate_totalpop.csv", quote = F, row.names = F)

# Prostate cancer screening x unins 2013 v Medicaid 2016
prostate_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q09c = weighted.mean(x = Q09c, w = WT, na.rm = T),
    sd = sd(Q09c, na.rm = TRUE)
  )
view(prostate_uninsvsmcaid)

write.csv(prostate_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/prostate_uninsvsmcaid.csv", quote = F, row.names = F)

# Mammogram total pop
mammogram_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q09d = weighted.mean(x = Q09d, w = WT, na.rm = T),
    sd = sd(Q09d, na.rm = TRUE)
  )
view(mammogram_totalpop)

write.csv(mammogram_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/mammogram_totalpop.csv", quote = F, row.names = F)

# Mammogram x unins 2013 v Medicaid 2016
mammogram_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q09d = weighted.mean(x = Q09d, w = WT, na.rm = T),
    sd = sd(Q09d, na.rm = TRUE)
  )
view(mammogram_uninsvsmcaid)

write.csv(mammogram_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/mammogram_uninsvsmcaid.csv", quote = F, row.names = F)

# Pap test total pop
pap_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q09e = weighted.mean(x = Q09e, w = WT, na.rm = T),
    sd = sd(Q09e, na.rm = TRUE)
  )
view(pap_totalpop)

write.csv(pap_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/pap_totalpop.csv", quote = F, row.names = F)

# Pap test x unins 2013 v Medicaid 2016
pap_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q09e = weighted.mean(x = Q09e, w = WT, na.rm = T),
    sd = sd(Q09e, na.rm = TRUE)
  )
view(pap_uninsvsmcaid)

write.csv(pap_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/pap_uninsvsmcaid.csv", quote = F, row.names = F)

# Number of PCP visits total pop
pcp_num_totalpop <- group_by(full, study) %>%
  summarise(
    count_n = n(),
    mean_Q04 = weighted.mean(x = Q04, w = WT, na.rm = T),
    sd = sd(Q04, na.rm = TRUE)
  )
view(pcp_num_totalpop)

write.csv(pcp_num_totalpop, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/pcp_num_totalpop.csv", quote = F, row.names = F)

# Number of PCP visits x unins 2013 v Medicaid 2016
pcp_num_uninsvsmcaid <- group_by(full, study, Q34_NO, Q34_CAID) %>%
  summarise(
    count_n = n(),
    mean_Q04 = weighted.mean(x = Q04, w = WT, na.rm = T),
    sd = sd(Q04, na.rm = TRUE)
  )
view(pcp_num_uninsvsmcaid)

write.csv(pcp_num_uninsvsmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/pcp_num_uninsvsmcaid.csv", quote = F, row.names = F)

# Group by source of care, sum weights to determine total number of people represented by survey responses
source_care_totalpop_weighted <- group_by(full, study, Q01) %>%
  summarise(
    count_n = sum(WT,na.rm=T)
    )
view(source_care_totalpop_weighted)

write.csv(source_care_totalpop_weighted, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/source_care_totalpop_weighted.csv", quote = F, row.names = F)

# bar plot weighted source of care
p <- ggplot(source_care_totalpop_weighted, aes(x = factor(Q01), y = count_n)) +
  geom_bar(
    aes(color = factor(study), fill = factor(study)),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
    ) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF")) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))
p

# Group by emergent, sum weights to determine total number of people represented by survey responses
emergent_totalpop_weighted <- group_by(full, Q34_CAID, Q02a) %>%
  summarise(
    count_n = sum(WT,na.rm=T)
  )
view(emergent_totalpop_weighted)

write.csv(emergent_totalpop_weighted, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/emergent_totalpop_weighted.csv", quote = F, row.names = F)

# Group by source of care and insurance status, sum weights to determine total number of people represented by survey responses
source_care_mcaidvsunins_weighted <- group_by(full, study, Q34_NO, Q34_CAID, Q01) %>%
  summarise(
    count_n = sum(WT,na.rm=T)
  )
view(source_care_mcaidvsunins_weighted)

write.csv(source_care_mcaidvsunins_weighted, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ssource_care_mcaidvsunins_weighted.csv", quote = F, row.names = F)

# Group by primary care provider, sum weights to determine total number of people represented by survey responses
pcp_mcaidvsunins_weighted_grouped <- group_by(full, study, Q34_NO, Q34_CAID, Q03) %>%
  summarise(
    count_n = sum(WT,na.rm=T)
  )
view(pcp_mcaidvsunins_weighted_grouped)

write.csv(pcp_mcaidvsunins_weighted_grouped, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/pcp_mcaidvsunins_weighted_grouped.csv", quote = F, row.names = F)

# Group by dental home, sum weights to determine total number of people represented by survey responses
dentalhome_mcaidvsunins_weighted_grouped <- group_by(full, study, Q34_NO, Q34_CAID, Q06) %>%
  summarise(
    count_n = sum(WT,na.rm=T)
  )
view(dentalhome_mcaidvsunins_weighted_grouped)

write.csv(dentalhome_mcaidvsunins_weighted_grouped, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/dentalhome_mcaidvsunins_weighted_grouped.csv", quote = F, row.names = F)

