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

