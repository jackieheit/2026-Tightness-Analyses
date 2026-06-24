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
tl2_cols <- c("General_TL")

domain_lookup <- tibble(
  Domain_var = c(tl1_cols),
  Domain_name = c(
    "law",
    "social",
    "gender",
    "marriage",
    "sex",
    "funeral"
  ),
  Domain_num = 1:8
)


tl1 <- tl %>%
  select(OWC, SCCS, all_of(tl1_cols), General_TL5B_avg_final)

tl1_id <- full_join(id, tl1, by = "OWC") %>%
  select(-SCCS) %>%
  mutate(
    Ov_tl_avg = rowMeans(across(all_of(tl1_cols)), na.rm = TRUE),
    Ov_tl_avg = if_else(is.nan(Ov_tl_avg), NA_real_, Ov_tl_avg)
  )

tl1_long <- tl1_id %>%
  pivot_longer(
    cols = all_of(domain_lookup$Domain_var),
    values_to = "TL1",
    names_to = "Domain_var"
  ) %>%
  left_join(domain_lookup, by = "Domain_var")


write.csv(tl1_long, "data/tl1_long.csv", row.names = FALSE)

