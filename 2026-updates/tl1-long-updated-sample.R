library(tidyverse)

tl <- read_csv("../../minerva/Tight-Loose/Datasets/DT-TL-FinalData.csv") # some formatting issue with the csv


id <- read_csv("../../minerva/Sample/Minerva-Starting-Sample.csv") 

id <- id %>%
  rename(minerva_id = 4) %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  ) %>%
  select(OWC, SCCS.ID, minerva_id, eHRAF.Name, PSF.Cluster, EA.ID)


tl <- tl %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  )


test <- tl %>%
  select(OWC, SCCS)

tl1_cols <- c("General_TL1_avg_final","Soc_TL1_avg_final",
                "Gender_TL1_avg_final","Mar_TL1_avg_final",
                "Sex_TL1_avg_final", "FM_TL1_avg_final")
tl2_cols <- c("General_TL2_avg_final","Soc_TL2_avg_final",
              "Gender_TL2_avg_final","Mar_TL2_avg_final",
              "Sex_TL2_avg_final", "FM_TL2_avg_final")
tl3_cols <- c("General_TL3_avg_final","Soc_TL3_avg_final",
              "Gender_TL3_avg_final","Mar_TL3_avg_final",
              "Sex_TL3_avg_final", "FM_TL3_avg_final")
tl4_cols <- c("General_TL4_avg_final","Soc_TL4_avg_final",
              "Gender_TL4_avg_final","Mar_TL4_avg_final",
              "Sex_TL4_avg_final", "FM_TL4_avg_final")
tl5_cols <- c("General_TL5_avg_final","Soc_TL5_avg_final",
              "Gender_TL5_avg_final","Mar_TL5_avg_final",
              "Sex_TL5_avg_final", "FM_TL5_avg_final")



tl_cols <- c(tl1_cols, tl2_cols, tl3_cols, tl4_cols, tl5_cols)

domain_lookup <- tibble(
  Domain_prefix = c("General", "Soc", "Gender", "Mar", "Sex", "FM"),
  Domain_name = c("law", "social", "gender", "marriage", "sex", "funeral"),
  Domain_num = 1:6
)

tl_long <- tl %>%
  select(
    OWC, SCCS,
    all_of(tl_cols),
    General_TL5B_avg_final
  ) %>%
  full_join(id, ., by = "OWC") %>%
  select(-SCCS) %>%
  pivot_longer(
    cols = all_of(tl_cols),
    names_to = c("Domain_prefix", ".value"),
    names_pattern = "^(General|Soc|Gender|Mar|Sex|FM)_(TL[1-5])_avg_final$"
  ) %>%
  left_join(domain_lookup, by = "Domain_prefix") %>%
  mutate(
    Z_TL1 = as.numeric(scale(TL1)),
    Z_TL2 = as.numeric(scale(TL2)),
    Z_TL3 = as.numeric(scale(TL3)),
    Z_TL4 = as.numeric(scale(TL4)),
    Z_TL5 = as.numeric(scale(TL5))
  )



write.csv(tl_long, "data/tl_long.csv", row.names = FALSE)

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

