library(tidyverse)

tl_long <- read_csv("../2026-updates/data/tl_long.csv")

# data analysis
hz <- read_csv("../../minerva/Hazards/Datasets/DT-hzcats-society-level.csv")

sh <- read_csv("../../2026-Sharing-Analyses/sharing-dataset-for-analysis.csv")
sh <- sh %>% select(ID, allhazards_nonfd_sev_exposure2_30, allhazards_nonfd_sev_exposure2_30_IA)

hz <- inner_join(hz, sh, by = c("ID"))
# remove outliers identified in cluster analysis  

hz <- hz %>% filter(ID != 73 & ID != 218)

hz <- hz %>%
  select(ID, OWC, normative_est_count_30, normative_est_count_30_IA, disaster_est_count_30, disaster_est_count_30_IA, allhazards_sev_exposure_30, allhazards_sev_exposure_30_IA, allhazards_H9a_sev_exposure_30, allhazards_H9a_sev_exposure_30_IA, 
         allhazards_nonfd_sev_exposure2_30, allhazards_nonfd_sev_exposure2_30_IA)

hz_vars <- c(
  "normative_est_count_30",
  "normative_est_count_30_IA",
  "disaster_est_count_30",
  "disaster_est_count_30_IA",
  "allhazards_sev_exposure_30",
  "allhazards_sev_exposure_30_IA",
  "allhazards_H9a_sev_exposure_30",
  "allhazards_H9a_sev_exposure_30_IA",
  "allhazards_nonfd_sev_exposure2_30",
  "allhazards_nonfd_sev_exposure2_30_IA"
)

hz <- hz %>%
  select(ID, OWC, all_of(hz_vars)) %>%
  mutate(
    across(
      all_of(hz_vars),
      ~ as.numeric(scale(.x)),
      .names = "Z_{.col}"
    )
  )

tl_hz <- tl_long %>%
  full_join(hz, by = c("minerva_id" = "ID"))


# Disaster hazards
library(lme4)
# install.packages("lmerTest")
library(lmerTest)

m1_data <- tl_hz %>%
  filter(
    !is.na(Z_TL5),
    !is.na(Z_disaster_est_count_30),
    !is.na(minerva_id)
  )

summary(lmer(Z_TL5 ~ Z_disaster_est_count_30 + (1 | minerva_id),data = m1_data))


m1_data_IA <- tl_hz %>%
  filter(
    !is.na(Z_TL5),
    !is.na(Z_disaster_est_count_30_IA),
    !is.na(minerva_id)
  )

summary(lmer(Z_TL5 ~ Z_disaster_est_count_30_IA + (1 | minerva_id), data = m1_data_IA))

summary(lmer(Z_TL5 ~ Z_allhazards_H9a_sev_exposure_30 + (1 | minerva_id), data = tl_hz))
summary(lmer(Z_TL5 ~ Z_allhazards_H9a_sev_exposure_30_IA + (1 | minerva_id), data = tl_hz))

summary(lmer(Z_TL5 ~ Z_allhazards_sev_exposure_30 + (1 | minerva_id), data = tl_hz))
summary(lmer(Z_TL5 ~ Z_allhazards_sev_exposure_30_IA + (1 | minerva_id), data = tl_hz))

summary(lmer(Z_TL5 ~ Z_allhazards_nonfd_sev_exposure2_30 + (1 | minerva_id), data = tl_hz))
summary(lmer(Z_TL5 ~ Z_allhazards_nonfd_sev_exposure2_30_IA + (1 | minerva_id), data = tl_hz))

summary(lmer(Z_TL5 ~ Z_normative_est_count_30 + (1 | minerva_id), data = tl_hz))
summary(lmer(Z_TL5 ~ Z_normative_est_count_30_IA + (1 | minerva_id), data = tl_hz))

