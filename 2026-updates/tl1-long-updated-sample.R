library(tidyverse)

tl <- read_csv("../../minerva/Tight-Loose/Datasets/DT-TL-FinalData.csv") # some formatting issue with the csv


id <- read_csv("../../minerva/Sample/Minerva-Starting-Sample.csv") 

id <- id %>%
  rename(minerva_id = 4) %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  ) %>%
  select(OWC, SCCS.ID, minerva_id, eHRAF.Name, PSF.Cluster, EA.ID)


tl <- read_csv(path) %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  )


test <- tl %>%
  select(OWC, SCCS)

law_cols <- c("General_TL1_avg_final","General_TL2_avg_final",
                "General_TL3_avg_final","General_TL4_avg_final",
                "General_TL5_avg_final")  

tl1 <- tl %>%
  select(OWC, SCCS, all_of(law_cols))

tl1_id <-  full_join(id, tl1, by = c("OWC")) %>%
  select(-SCCS)

tl1_long <- pivot_longer(tl1_id, all_of(law_cols), values_to = "TL1", names_to = "Domain") %>%
  mutate(
    Domain = as.numeric(str_extract(Domain, "(?<=TL)\\d"))
  )

write.csv(tl1_long, "data/tl1_long.csv", row.names = FALSE)
