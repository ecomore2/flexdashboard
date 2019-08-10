library(dplyr)
library(magrittr)
library(padr)


# Parameters:
threshold <- 15 # filtering all obs. for which the difference between any pair
                # of dates is more than "threshold" days.


# Read the PACS data:
pacs <- readr::read_csv("../pacs/data/pacs.csv",
                        col_types = paste(c("icfnD", rep("c", 5), rep("D", 4), rep("f", 3)), collapse = ""))


# Calculating dates differences and filtering:
corrections <- pacs %>% 
  mutate(onset_hospitalization             = as.integer(onset           - hospitalization),
         onset_consultation                = as.integer(onset           - consultation),
         onset_sample_collection           = as.integer(onset           - sample_collection),
         hospitalization_consultation      = as.integer(hospitalization - consultation),
         hospitalization_sample_collection = as.integer(hospitalization - sample_collection),
         consultation_sample_collection    = as.integer(consultation    - sample_collection)) %>% 
  filter(abs(onset_hospitalization)             < threshold,
         abs(onset_consultation)                < threshold,
         abs(onset_sample_collection)           < threshold,
         abs(hospitalization_consultation)      < threshold,
         abs(hospitalization_sample_collection) < threshold,
         abs(consultation_sample_collection)    < threshold) %>% 
  select(contains("onset_")) %>% 
  sapply(mean) %>% 
  round()


# Making data and writing to disk:
pacs %>% 
  mutate(onset2 = if_else(is.na(onset),
                          if_else(is.na(hospitalization),
                                  if_else(is.na(consultation),
                                          sample_collection + corrections["onset_sample_collection"],
                                          consultation + corrections["onset_consultation"]),
                                  hospitalization + corrections["onset_hospitalization"]),
                          onset),
         positive = pcr == "positive" | ns1 == "positive") %>% 
  select(-dob, -onset, -hospitalization, -consultation, -sample_collection) %>% 
  filter(! is.na(onset2)) %>% 
  thicken("week") %>% 
  select(-onset2) %>% 
  group_by(onset2_week) %>% 
  summarise(n = n(),
            male = sum(sex == "male"),
            age = mean(age, na.rm = TRUE),
            positive = sum(positive, na.rm = TRUE),
            prop_pos1 = mean(positive, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pad("week") %>% 
  mutate(positive = ifelse(is.nan(positive), NA, positive),
         prop_pos2 = positive / n,
         negative  = n - positive) %>% 
  rename(week = onset2_week) %>% 
  write.csv("data_weekly_epi_ts.csv", FALSE, row.names = FALSE)
