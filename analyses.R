library(dplyr)
library(rlang)
library(weights)

#TEST SCRIPTS

# TEST CROSSTAB 1: CATEGORICAL Dental Home
ins_dentist <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q06 = weighted.mean(x = Q06, w = WT, na.rm = T),
    sd = sd(Q06, na.rm = TRUE)
  )
view(ins_dentist)

# Remove 9s from Dental Home (Missing)
full[full$Q06 == 9 | is.na(full$Q06), "Q06"] =  NA

view(full)

write.csv(ins_dentist, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_dentist
          .csv", quote = F, row.names = F)

# TEST CROSSTAB 2: ORDINAL: Insurance Status x Routine Checkup
ins_checkup <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q08a = weighted.mean(x = Q08a, w = WT, na.rm = T),
    sd = sd(Q08a, na.rm = TRUE)
  )

view(ins_checkup)

# TESTCT2 OBSERVATION: Greatest length of time since last routine checkup highest among uninsured in both 2013 and 2016

write.csv(ins_checkup, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/ins_checkup.csv", quote = F, row.names = F)

# TEST CROSSTAB 3: ORDINAL: Insurance Status x Dental Exam
ins_dental <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q08b = weighted.mean(x = Q08b, w = WT, na.rm = T),
    sd = sd(Q08b, na.rm = TRUE)
  )

view(ins_dental)

# TESTCT3 OBSERVATION: Greatest length of time since last dental checkup highest among uninsured in both 2013 and 2016

# TEST CROSSTAB 4: ORDINAL: Insurance Status x Blood Pressure Check
ins_blood_pressure <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q08c = weighted.mean(x = Q08c, w = WT, na.rm = T),
    sd = sd(Q08c, na.rm = TRUE)
  )

view(ins_blood_pressure)

# TESTCT4 OBSERVATION: Greatest length of time since last bp check highest among uninsured in both 2013 and 2016

# TEST CROSSTAB 5: ORDINAL: Insurance Status x Cholesterol Test
ins_cholesterol <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q08d = weighted.mean(x = Q08d, w = WT, na.rm = T),
    sd = sd(Q08d, na.rm = TRUE)
  )

view(ins_cholesterol)

# Could be interesting to test number of PCP visits x blood pressure and cholesterol screenings by insurance status re: visit/prevenive services quality

# TESTCT5 OBSERVATION: Greatest length of time since last cholesterol check among uninsured and Medicaid

# TEST CROSSTAB 6: ORDINAL: Insurance Status x Blood Sugar Test
ins_blood_sugar <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q08e = weighted.mean(x = Q08e, w = WT, na.rm = T),
    sd = sd(Q08e, na.rm = TRUE)
  )

view(ins_blood_sugar)

# TEST CROSSTAB 7: ORDINAL: Insurance Status x FOBT for Colon Cancer
ins_fobt <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q09a = weighted.mean(x = Q09a, w = WT, na.rm = T),
    sd = sd(Q09a, na.rm = TRUE)
  )

view(ins_fobt)

# TEST CROSSTAB 8: ORDINAL: Insurance Status x Sigmoidoscopy or colonoscopy
ins_colon <- group_by(full, study, Q34_NO, Q34_CAID, Q34_EMP) %>%
  summarise(
    count_n = n(),
    mean_Q09b = weighted.mean(x = Q09b, w = WT, na.rm = T),
    sd = sd(Q09b, na.rm = TRUE)
  )

view(ins_colon)

# TEST: Study group x Insurance status x Number of ER visits
# Uninsured in 2013 vs. Medicaid-insured in 2016 x Number of ER visits
# Total pop in 2013 vs total pop 2016 x Number of ER visits

# ER number x 2013 uninsured
er_num_unins_13 <- full[ full$study == 7 & full$Q34_NO == 1, "Q02Number"]

# ER number x 2016 Medicaid
er_num_mcaid_16 <- full[ full$study == 8 & full$Q34_CAID == 1, "Q02Number"]

t.test(er_num_mcaid_16, er_num_unins_13)
# Welch Two Sample t-test
# 
# data:  er_num_mcaid_16 and er_num_unins_13
# t = 6.964, df = 97.93, p-value = 3.823e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.095942 1.969474
# sample estimates:
#   mean of x mean of y 
# 1.9838710 0.4511628 

# ER number x total pop 2013 v. 2016
# ER number x 2013 total pop
er_num_13 <- full[ full$study == 7, "Q02Number"]
# Weighted
er_num_13 <- full[ full$study == 7, "Q02Number"] * full[ full$study == 7, "WT"]

# ER number x 2016 total
er_num_16 <- full[ full$study == 8, "Q02Number"]
# Weighted
er_num_16 <- full[ full$study == 8, "Q02Number"] * full[ full$study == 8, "WT"]

t.test(er_num_16, er_num_13)

# Welch Two Sample t-test
# 
# data:  er_num_16 and er_num_13
# t = 22.475, df = 530.14, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.137869 1.355829
# sample estimates:
#   mean of x mean of y 
# 1.5090090 0.2621602 

# OBSERVATION: ER visits increased for total population

t.test(er_num_unins_13, er_num_mcaid_16)

# NON-PARAMETRIC: Wilcoxon Signed Rank test (independent 2-group Mann-Whitney U Test)
# ER number x 2013 uninsured v 2016 Medicaid
wilcox.test(er_num_unins_13, er_num_mcaid_16)

range(full$WT)

wilcox.test(er_num_13, er_num_16)


# HEALTHY DAYS: 
# # Total pop in 2013 vs. Total pop in 2016 x Physical Health Not Good
poor_physical_13 <- full[ full$study == 7, "Q19a"] * full[ full$study == 7, "WT"] / sum(full[ full$study == 7, "WT"], na.rm = T)
poor_physical_16 <- full[ full$study == 8, "Q19a"] * full[ full$study == 8, "WT"] / sum(full[ full$study == 8, "WT"], na.rm = T)

# NONPARAMETRIC 
wilcox.test(poor_physical_16, poor_physical_13)
# W = 2911300, p-value = 0.9626

# WEIGHTED AND NORMALIZED
# # Uninsured in 2013 vs. Medicaid-insured in 2016 x Physical Health Not Good
poor_physical_unins_13 <- full[ full$study == 7 & full$Q34_NO == 1, "Q19a"] * full[ full$study == 7 & full$Q34_NO == 1, "WT"] / sum(full[ full$study == 7 & full$Q34_NO == 1, "WT"], na.rm = T)
poor_physical_mcaid_16 <- full[ full$study == 8 & full$Q34_CAID == 1, "Q19a"] * full[ full$study == 8 & full$Q34_CAID == 1, "WT"] / sum(full[ full$study == 8 & full$Q34_CAID == 1, "WT"], na.rm = T)
view(poor_physical_unins_13) 

# # NON-PARAMETRIC: Wilcoxon Signed Rank test (independent 2-group Mann-Whitney U Test)
wilcox.test(poor_physical_unins_13, poor_physical_mcaid_16)
# W = 13454, p-value = 0.01313

# HEALTHY DAYS: 
# # Uninsured in 2013 vs. Medicaid-insured in 2016 x Physical Health Limited Activity
physical_limited_unins_13 <- full[ full$study == 7 & full$Q34_NO == 1, "Q19b"] * full[ full$study == 7 & full$Q34_NO == 1, "WT"] / sum(full[ full$study == 7 & full$Q34_NO == 1, "WT"], na.rm = T)
physical_limited_mcaid_16 <- full[ full$study == 8 & full$Q34_CAID == 1, "Q19b"] * full[ full$study == 8 & full$Q34_CAID == 1, "WT"] / sum(full[ full$study == 8 & full$Q34_CAID == 1, "WT"], na.rm = T)

wilcox.test(physical_limited_unins_13, physical_limited_mcaid_16)
# W = 12113, p-value = 1.331e-05

# HEALTHY DAYS:
# # Uninsured in 2013 vs. Medicaid-insured in 2016 x Mental Health Not Good
poor_mental_unins_13 <- full[ full$study == 7 & full$Q34_NO == 1, "Q19c"] * full[ full$study == 7 & full$Q34_NO == 1, "WT"] / sum(full[ full$study == 7 & full$Q34_NO == 1, "WT"], na.rm = T)
poor_mental_mcaid_16 <- full[ full$study == 8 & full$Q34_CAID == 1, "Q19c"] * full[ full$study == 8 & full$Q34_CAID == 1, "WT"] / sum(full[ full$study == 8 & full$Q34_CAID == 1, "WT"], na.rm = T)

# # NON-PARAMETRIC: Mental Health Not Good x 2013 ins v 2016 mcaid Wilcoxon Signed Rank test (independent 2-group Mann-Whitney U Test)
wilcox.test(poor_mental_unins_13, poor_mental_mcaid_16)
# W = 16829, p-value = 0.6862

# HEALTHY DAYS: 
# # Uninsured in 2013 vs. Medicaid-insured in 2016 x Mental Health Limited Activity
mental_limited_unins_13 <- full[ full$study == 7 & full$Q34_NO == 1, "Q19d"] * full[ full$study == 7 & full$Q34_NO == 1, "WT"] / sum(full[ full$study == 7 & full$Q34_NO == 1, "WT"], na.rm = T)
mental_limited_mcaid_16 <- full[ full$study == 8 & full$Q34_CAID == 1, "Q19d"] * full[ full$study == 8 & full$Q34_CAID == 1, "WT"] / sum(full[ full$study == 8 & full$Q34_CAID == 1, "WT"], na.rm = T)

wilcox.test(mental_limited_unins_13, mental_limited_mcaid_16)
# W = 15530, p-value = 0.1599

# PRIMARY CARE: 
# # Uninsured in 2013 vs. Medicaid-insured in 2016 x Primary care visits
pcptimes_unins_13 <- full[ full$study == 7 & full$Q34_NO == 1, "Q04"] * full[ full$study == 7 & full$Q34_NO == 1, "WT"] / sum(full[ full$study == 7 & full$Q34_NO == 1, "WT"], na.rm = T)
pcptimes_mcaid_16 <- full[ full$study == 8 & full$Q34_CAID == 1, "Q04"] * full[ full$study == 8 & full$Q34_CAID == 1, "WT"] / sum(full[ full$study == 8 & full$Q34_CAID == 1, "WT"], na.rm = T)

wilcox.test(pcptimes_unins_13, pcptimes_mcaid_16)
# W = 4545, p-value = 0.0007895


# ER VISITS:
# # Uninsured in 2013 vs. Medicaid-insured in 2016 x ER Visits
er_unins_13 <- full[ full$study == 7 & full$Q34_NO == 1, "Q02Number"] * full[ full$study == 7 & full$Q34_NO == 1, "WT"] / sum(full[ full$study == 7 & full$Q34_NO == 1, "WT"], na.rm = T)
er_mcaid_16 <- full[ full$study == 8 & full$Q34_CAID == 1, "Q02Number"] * full[ full$study == 8 & full$Q34_CAID == 1, "WT"] / sum(full[ full$study == 8 & full$Q34_CAID == 1, "WT"], na.rm = T)

wilcox.test(er_unins_13, er_mcaid_16)
# W = 1352, p-value < 2.2e-16

# Find weighted percentages of each source of care
## ISN'T WORKING: Says object "Q01" not found
# Error in UseMethod("mutate_") : no applicable method for 'mutate_' applied to an object of class "c('integer', 'numeric')"
source_care_totalpop_weighted_perc <- mutate(full$Q01,
                                             ernum_perc = Q01/sum(Q01))

install.packages("janitor")
library(janitor)

full %>% 
  group_by(Q01) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(percent = (n / n())*100) %>% 
  select(Q01, percent) %>% 
  distinct()

print(full %>% 
  filter(study == 7, Q34_NO == 1) %>%
  group_by(study, Q34_NO, Q01) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(percent = (n / n())*100) %>% 
  select(study,Q34_NO, Q01, percent) %>% 
  distinct(), n = Inf)

# weighted for 2013 survey
perc_source_care_2013 = full %>% 
        filter(study == 7, Q34_NO == 1) %>%
        group_by(study, Q34_NO, Q01) %>% 
        mutate(n = sum(WT)) %>% 
        ungroup() %>% 
        mutate(percent = (n / sum(WT))*100) %>% 
        select(study,Q34_NO, Q01, percent) %>% 
        distinct() %>%
        arrange(Q01)

# weighted for 2016 survey
perc_source_care_2016 = full %>% 
        filter(study == 8, Q34_CAID == 1) %>%
        group_by(study, Q34_CAID, Q01) %>% 
        mutate(n = sum(WT)) %>% 
        ungroup() %>% 
        mutate(percent = (n / sum(WT))*100) %>% 
        select(study,Q34_CAID, Q01, percent) %>% 
        distinct() %>%
        arrange(Q01)

# combine percents from both surveys vertically (adding rows to rows)
perc_source_care_uninsvmcaid = bind_rows(perc_source_care_2013,perc_source_care_2016) %>%
  mutate(Q01 = replace(Q01, Q01 == 0, "Some other place"),
         Q01 = replace(Q01, Q01 == 1, "Do not have regular place"),
         Q01 = replace(Q01, Q01 == 2, "Doctors office/private"),
         Q01 = replace(Q01, Q01 == 3, "Community health clinic"),
         Q01 = replace(Q01, Q01 == 4, "ER"),
         Q01 = replace(Q01, Q01 == 5, "Urgent care/clinic inside retail store"),
         Q01 = replace(Q01, Q01 == 6, "School"),
         Q01 = replace(Q01, Q01 == 8, "County HD"),
         Q01 = replace(Q01, Q01 == 9, "VA or Military"),
         Q01 = replace(Q01, Q01 == 11, "Community health center – FMC"),
         Q01 = replace(Q01, is.na(Q01), "No response"))

# PLOT: Source of care, unins v medicaid WEIGHTED
p <- ggplot(perc_source_care_uninsvmcaid, aes(x = factor(Q01,
                                                         levels = c("Do not have regular place",
                                                                    "Doctors office/private",
                                                                    "Community health clinic",
                                                                    "ER",
                                                                    "Urgent care/clinic inside retail store",
                                                                    "School",
                                                                    "County HD",
                                                                    "VA or Military",
                                                                    "Community health center – FMC",
                                                                    "Some other place",
                                                                    "No response")),
                                              y = percent)) +
  geom_bar(
    aes(fill = factor(study)),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  labs(x = "Source of Care", y = "% of given population") +
  ylim(c(0,100)) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"), 
                    name = "Study Year", labels = c(2013,2016)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p
ggsave(filename = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/graph_perc_source_care_uninsvmcaid.pdf",
       device = "pdf",plot = p, width = 10, height = 6, units = "in")


view(source_care_totalpop_weighted)

view(perc_source_care_uninsvmcaid)

write.csv(perc_source_care_uninsvmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_source_care_uninsvmcaid.csv", quote = F, row.names = F)

# REDO: # PLOT: Source of care, unins v medicaid WEIGHTED
# renamed, text resized
p <- ggplot(perc_source_care_uninsvmcaid, aes(x = factor(Q01,
                                                         levels = c("Do not have regular place",
                                                                    "Doctors office/private",
                                                                    "Community health clinic",
                                                                    "ER",
                                                                    "Urgent care/clinic inside retail store",
                                                                    "School",
                                                                    "County HD",
                                                                    "VA or Military",
                                                                    "Community health center – FMC",
                                                                    "Some other place",
                                                                    "No response")),
                                              y = percent)) +
geom_bar(
  aes(fill = factor(study)),
  stat = "identity", position = position_dodge(0.8),
  width = 0.7
  ) +
  labs(x = "Source of Care", y = "% of Study Population") +
  ylim(c(0,100)) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"), 
                    name = "Study Population", labels = c("Uninsured in 2013","Medicaid-Enrolled in 2016")) +
  theme_bw() +
  theme(axis.text.x=element_text(size=11,face="bold",angle=45, hjust=1))
p

ggsave(filename = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/graph_perc_source_care_uninsvmcaid_renamed.pdf",
       device = "pdf",plot = p, width = 12, height = 6, units = "in")


# ER for emergent conditions: Medicaid pop
perc_er_emergent_mcaid2016 = full %>% 
  filter(study == 8, Q34_CAID == 1, !is.na(Q02a)) %>%
  group_by(study, Q34_CAID, Q02a) %>% 
  mutate(n = sum(WT)) %>% 
  ungroup() %>% 
  mutate(percent = (n / sum(WT))*100) %>% 
  select(study,Q34_CAID, Q02a, percent) %>% 
  distinct() %>%
  arrange(Q02a)

view(perc_er_emergent_mcaid2016)

# ER emergent: 2016 total pop
perc_er_emergent_total2016 = full %>% 
  filter(study == 8, !is.na(Q02a)) %>%
  group_by(study, Q02a) %>% 
  mutate(n = sum(WT)) %>% 
  ungroup() %>% 
  mutate(percent = (n / sum(WT))*100) %>% 
  select(study,Q02a, percent) %>% 
  distinct() %>%
  arrange(Q02a)

view(perc_er_emergent_total2016)

# combine percents from ER emergent 2016 Medicaid vs total pop surveys vertically (adding rows to rows)
perc_er_emergent_mcaidvstotalpop2016 = bind_rows(perc_er_emergent_mcaid2016,perc_er_emergent_total2016) %>%
  mutate(Q02a = replace(Q02a, Q02a == 1, "Yes"),
         Q02a = replace(Q02a, Q02a == 2, "No"),
         Q02a = replace(Q02a, Q02a == 3, "Not sure"),
         Q34_CAID = replace (Q34_CAID, is.na(Q34_CAID),2))
view(perc_er_emergent_mcaidvstotalpop2016)

# PLOT: ER emergent
p <- ggplot(perc_er_emergent_mcaidvstotalpop2016, aes(x = factor(Q02a,
                                                         levels = c("Yes",
                                                                    "No",
                                                                    "Not sure")),
                                              y = percent)) +
  geom_bar(
    aes(fill = factor(Q34_CAID)),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  labs(x = "Response", y = "% of Study Population") +
  ylim(c(0,100)) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"), 
                    name = "Study Population", labels = c("Medicaid-Enrolled in 2016","Total Population in 2016")) +
  theme_bw() +
  theme(axis.text.x=element_text(size=11,face="bold",angle=45, hjust=1))
p

ggsave(filename = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_er_emergent_mcaidvstotalpop2016.pdf",
       device = "pdf",plot = p, width = 12, height = 6, units = "in")

write.csv(perc_er_emergent_mcaidvstotalpop2016, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_er_emergent_mcaidvstotalpop2016.csv", quote = F, row.names = F)


# Reporting a Primary Care Provider: Unins 2013
perc_pcp_unins2013 = full %>% 
  filter(study == 7, Q34_NO == 1, !is.na(Q03), !Q03 == 9) %>%
  group_by(study, Q34_NO, Q03) %>% 
  mutate(n = sum(WT)) %>% 
  ungroup() %>% 
  mutate(percent = (n / sum(WT))*100) %>% 
  select(study,Q34_NO, Q03, percent) %>% 
  distinct() %>%
  arrange(Q03)

view(perc_pcp_unins2013)

# Reporting a Primary Care Provider: Medicaid 2016
perc_pcp_mcaid2016 = full %>% 
  filter(study == 8, Q34_CAID == 1, !is.na(Q03), !Q03 == 9) %>%
  group_by(study, Q34_CAID, Q03) %>% 
  mutate(n = sum(WT)) %>% 
  ungroup() %>% 
  mutate(percent = (n / sum(WT))*100) %>% 
  select(study,Q34_CAID, Q03, percent) %>% 
  distinct() %>%
  arrange(Q03)

view(perc_pcp_mcaid2016)

# combine percents from ER emergent 2016 Medicaid vs total pop surveys vertically (adding rows to rows)
perc_pcp_uninsvmcaid = bind_rows(perc_pcp_unins2013,perc_pcp_mcaid2016) %>%
  mutate(Q03 = replace(Q03, Q03 == 1, "Yes"),
         Q03 = replace(Q03, Q03 == 2, "No"))
view(perc_pcp_uninsvmcaid)         

write.csv(perc_pcp_uninsvmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_pcp_uninsvmcaid.csv", quote = F, row.names = F)

# PLOT: PCP
p <- ggplot(perc_pcp_uninsvmcaid, aes(x = factor(Q03,
                                                                 levels = c("Yes",
                                                                            "No")),
                                                      y = percent)) +
  geom_bar(
    aes(fill = factor(study)),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  labs(x = "Response", y = "% of Study Population") +
  ylim(c(0,100)) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"), 
                    name = "Study Population", labels = c("Uninsured in 2013","Medicaid-Enrolled in 2016")) +
  theme_bw() +
  theme(axis.text.x=element_text(size=15,face="bold",angle=45, hjust=1))
p

ggsave(filename = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_pcp_uninsvmcaid.pdf",
       device = "pdf",plot = p, width = 11, height = 6, units = "in")

write.csv(perc_er_emergent_mcaidvstotalpop2016, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_er_emergent_mcaidvstotalpop2016.csv", quote = F, row.names = F)

# Reporting a Dental Provider: Unins 2013
perc_dental_unins2013 = full %>% 
  filter(study == 7, Q34_NO == 1, !is.na(Q06), !Q06 == 9) %>%
  group_by(study, Q34_NO, Q06) %>% 
  mutate(n = sum(WT)) %>% 
  ungroup() %>% 
  mutate(percent = (n / sum(WT))*100) %>% 
  select(study,Q34_NO, Q06, percent) %>% 
  distinct() %>%
  arrange(Q06)

view(perc_dental_unins2013)

# Reporting a Dental Provider: Medicaid 2016
perc_dental_mcaid2016 = full %>% 
  filter(study == 8, Q34_CAID == 1, !is.na(Q06), !Q06 == 9) %>%
  group_by(study, Q34_CAID, Q06) %>% 
  mutate(n = sum(WT)) %>% 
  ungroup() %>% 
  mutate(percent = (n / sum(WT))*100) %>% 
  select(study,Q34_CAID, Q06, percent) %>% 
  distinct() %>%
  arrange(Q06)

view(perc_dental_mcaid2016)

# combine percents from ER emergent 2016 Medicaid vs total pop surveys vertically (adding rows to rows)
perc_dental_uninsvmcaid = bind_rows(perc_dental_unins2013,perc_dental_mcaid2016) %>%
  mutate(Q06 = replace(Q06, Q06 == 1, "Yes"),
         Q06 = replace(Q06, Q06 == 2, "No"))
view(perc_dental_uninsvmcaid)         

write.csv(perc_dental_uninsvmcaid, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_dental_uninsvmcaid.csv", quote = F, row.names = F)

# PLOT: Dental
p <- ggplot(perc_dental_uninsvmcaid, aes(x = factor(Q06,
                                                 levels = c("Yes",
                                                            "No")),
                                      y = percent)) +
  geom_bar(
    aes(fill = factor(study)),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  labs(x = "Response", y = "% of Study Population") +
  ylim(c(0,100)) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"), 
                    name = "Study Population", labels = c("Uninsured in 2013","Medicaid-Enrolled in 2016")) +
  theme_bw() +
  theme(axis.text.x=element_text(size=15,face="bold",angle=45, hjust=1))
p

ggsave(filename = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_dental_uninsvmcaid.pdf",
       device = "pdf",plot = p, width = 11, height = 6, units = "in")

write.csv(perc_er_emergent_mcaidvstotalpop2016, file = "/Users/abigailworthen/Desktop/UCD/Spring 2019/Capstone/perc_er_emergent_mcaidvstotalpop2016.csv", quote = F, row.names = F)

#TEST CHI SQ: PCP reported, total pop 2013 vs 2016 ------------------ 

uninsvmcaid <- full %>% 
  as_tibble() %>% 
  dplyr::filter((study == 7 & Q34_NO == 1) | (study == 8 & Q34_CAID == 1))

vars <- c("01", 
          "03", 
          "06", 
          "08a",
          "08b", 
          "08c",
          "08d", 
          "08e", 
          "09a", 
          "09b", 
          "09c", 
          "09d", 
          "09e")
# vars <- paste0("Q", vars)
# 
# chisq_variables <- tibble(
#   variable = vars
# ) 

# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
# tragic :( 
# get_chisq <- function(col, df = uninsvmcaid) {
#   
#   # get which question we're working with
#   which_q <- stringr::str_sub(col, 3, 3) %>% as.integer()
#   
#   column <- rlang::quo(col)
#   # do some filtering 
#   if (which_q %in% c(3, 6)){
#     df <- df %>% 
#       dplyr::filter(!!column != 9) 
#   } else if (which_q %in% c(8, 9)) {
#     df <- df %>% 
#       dplyr::filter(!!column != 8) %>% 
#       dplyr::filter(!!column != 9)
#   }
#   
#   variable <- df %>% 
#     dplyr::select(!!column) %>% 
#     dplyr::pull()
#   
#   # get weighted chi squared test results 
#   test <- wtd.chi.sq(df$study, variable, weight = df$WT)
#   
#   return(test)
#   
# }
# 
# # results for questions except for 2 
# chisq_variables %>% 
#   mutate(chisq = purrr::map(variable, get_chisq)) %>% 
#   unnest() %>% 
#   mutate(what = rep(c("chi_sq", "df", "p_value"), nrow(chisq_variables))) %>% 
#   tidyr::spread(what, chisq)

# A tibble: 13 x 4

# variable chi_sq    df  p_value
# <chr>     <dbl> <dbl>    <dbl>
#   1 Q01        72.4     8 1.60e-12
# 2 Q03        49.9     2 1.47e-11
# 3 Q06        19.6     1 9.58e- 6
# 4 Q08a       38.8     6 7.79e- 7
# 5 Q08b       31.2     6 2.31e- 5
# 6 Q08c       32.1     6 1.54e- 5
# 7 Q08d       50.0     6 4.76e- 9
# 8 Q08e       49.5     6 5.87e- 9
# 9 Q09a       23.0     6 8.07e- 4
# 10 Q09b       44.1     6 7.17e- 8
# 11 Q09c       11.1     5 4.89e- 2
# 12 Q09d       14.3     6 2.66e- 2
# 13 Q09e       13.5     6 3.56e- 2

### WEIGHTED INDIVIDUAL CHI SQs



### Chisq          df     p.value 
# 15.38345354  6.00000000  0.01747506 
uninsvmcaid %>% dplyr::select(study, Q02a) %>% count(study, Q02a)
## CHI SQ: Emergency room emergent uninsvmcaid (Q02a) 

q2 <- full %>% 
  filter(study == 8) 

# results for Q02a 
wtd.chi.sq(q2$Q34_CAID, q2$Q02a, weight = q2$WT)

## Chisq         df    p.value 
# 8.24921757 2.00000000 0.01616982 
q2 %>% count(Q34_CAID, Q02a, study)

# CHI SQ: Medical home uninsvmcaid (Q01)
wtd.chi.sq(uninsvmcaid$study, uninsvmcaid$Q01, weight = uninsvmcaid$WT)
### Chisq           df      p.value 
# 7.243926e+01 8.000000e+00 1.603745e-12 

# CHI SQ: PCP uninsvmcaid (Q03)
pcp_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q03 != 9)
wtd.chi.sq(pcp_filtered$study, pcp_filtered$Q03, weight = pcp_filtered$WT)
# Chisq           df      p.value 
# 4.703203e+01 1.000000e+00 6.983593e-12

# CHI SQ: Dental home (Q06)
dental_home_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q06 != 9)
wtd.chi.sq(dental_home_filtered$study, dental_home_filtered$Q06, weight = dental_home_filtered$WT)
# Chisq           df      p.value 
# 1.955447e+01 1.000000e+00 9.777190e-06

# CHI SQ: Routine checkup (Q08a)
checkup_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q08a != 8,
                Q08a != 9)
wtd.chi.sq(checkup_filtered$study, checkup_filtered$Q08a, weight = checkup_filtered$WT)
# Chisq           df      p.value 
# 3.831428e+01 6.000000e+00 9.751157e-07 

# CHI SQ: dental exam (08b)
dental_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q08b != 8,
                Q08b != 9)
wtd.chi.sq(dental_filtered$study, dental_filtered$Q08b, weight = dental_filtered$WT)
# Chisq           df      p.value 
# 3.083232e+01 6.000000e+00 2.728785e-05 

# CHI SQ: blood pressure check (08c)
bp_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q08c != 8,
                Q08c != 9)
wtd.chi.sq(bp_filtered$study, bp_filtered$Q08c, weight = bp_filtered$WT)
# Chisq           df      p.value 
# 3.201042e+01 6.000000e+00 1.624273e-05 

# CHI SQ: blood cholesterol test (08d)
chol_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q08d != 8,
                Q08d != 9)
wtd.chi.sq(chol_filtered$study, chol_filtered$Q08d, weight = chol_filtered$WT)
# Chisq           df      p.value 
# 4.916206e+01 6.000000e+00 6.919313e-09 

# CHI SQ: blood sugar test (08e)
sugar_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q08e != 8,
                Q08e != 9)
wtd.chi.sq(sugar_filtered$study, sugar_filtered$Q08e, weight = sugar_filtered$WT)
# Chisq           df      p.value 
# 4.841580e+01 6.000000e+00 9.758199e-09 

# CHI SQ: fobt (09a)
fobt_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q09a != 8,
                Q09a != 9)
wtd.chi.sq(fobt_filtered$study, fobt_filtered$Q09a, weight = fobt_filtered$WT)
# Chisq           df      p.value 
# 2.286943e+01 6.000000e+00 8.414086e-04 

# CHI SQ: colonoscopy (09b)
colon_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q09b != 8,
                Q09b != 9)
wtd.chi.sq(colon_filtered$study, colon_filtered$Q09b, weight = colon_filtered$WT)
# Chisq           df      p.value 
# 4.359590e+01 6.000000e+00 8.889303e-08 

# CHI SQ: prostate screening (09c)
prostate_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q09c != 8,
                Q09c != 9)
wtd.chi.sq(prostate_filtered$study, prostate_filtered$Q09c, weight = prostate_filtered$WT)
# Chisq        df   p.value 
# 8.6098460 5.0000000 0.1256751

# CHI SQ: mammogram (09d)
mammo_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q09d != 8,
                Q09d != 9)
wtd.chi.sq(mammo_filtered$study, mammo_filtered$Q09d, weight = mammo_filtered$WT)
# Chisq          df     p.value 
# 16.47781489  6.00000000  0.01140664 

# CHI SQ: Pap test uninsvsmcaid (09e)
pap_filtered <- uninsvmcaid %>% 
  dplyr::filter(Q09e != 8, 
                Q09e != 9)
wtd.chi.sq(pap_filtered$study, pap_filtered$Q09e, weight = pap_filtered$WT)



# ---------------------------- 



