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



tl_cols <- c(tl1_cols, tl2_cols, tl3_cols, tl4_cols)

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
    names_pattern = "^(General|Soc|Gender|Mar|Sex|FM)_(TL[1-4])_avg_final$"
  ) %>%
  left_join(domain_lookup, by = "Domain_prefix")

write.csv(tl_long, "data/tl_long.csv", row.names = FALSE)

