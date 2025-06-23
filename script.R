library(tidyverse)
fisc <- read_csv("FiSC-Full-Dataset-2022-Update.csv")

fisc_small <- fisc %>%
  select(city_name, year, rev_general_city, spending_general_city)

fisc_filtered <- fisc_small %>%
  filter(year >= 2015)

fisc_mutated <- fisc %>%
  mutate(annual_balance = rev_general_city - spending_general_city)

fisc_summary <- fisc_mutated %>%
  group_by(city_name) %>%
  summarize(avg_annual_balance = mean(annual_balance, na.rm = TRUE))

write_csv(fisc_summary, "annual_balance_summary.csv")