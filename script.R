# Load libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(showtext)
library(scales)
library(sf)
library(maps)
library(stringr)
library(patchwork)

# Enable Google font
font_add_google("Source Serif 4", "serif4")
showtext_auto()

# Load and clean data
fisc <- read_csv("FiSC-Full-Dataset-2022-Update.csv")

fisc_clean <- fisc %>%
  filter(year >= 2012, year <= 2022, !is.na(city_types))

# Create fiscal indicators
fisc_indicators <- fisc_clean %>%
  mutate(
    debt_burden_ratio = debt_outstanding / rev_general,
    debt_service_ratio = interest_general / spending_total,
    op_balance_ratio = (rev_general - spending_general) / rev_general
  )

# Revenue volatility
rev_volatility <- fisc_indicators %>%
  group_by(city_name) %>%
  summarize(rev_volatility = sd(rev_general, na.rm = TRUE), .groups = "drop")

fisc_indicators <- fisc_indicators %>%
  left_join(rev_volatility, by = "city_name")

# Weighted composite fiscal index (2019 only)
weights <- c(op_balance_ratio = 0.5, debt_burden_ratio = 0.2, debt_service_ratio = 0.2, rev_volatility = 0.1)
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

fisc_2019 <- fisc_indicators %>%
  filter(year == 2019) %>%
  mutate(
    city_short = str_to_title(str_trim(str_extract(city_name, "(?<=: ).*"))),
    state_abbr = str_extract(city_name, "^[A-Z]{2}"),
    norm_op = normalize(op_balance_ratio),
    norm_db = 1 - normalize(debt_burden_ratio),
    norm_ds = 1 - normalize(debt_service_ratio),
    norm_rv = 1 - normalize(rev_volatility),
    fiscal_index = weights["op_balance_ratio"] * norm_op +
      weights["debt_burden_ratio"] * norm_db +
      weights["debt_service_ratio"] * norm_ds +
      weights["rev_volatility"] * norm_rv
  )

# Join coordinates
data("us.cities", package = "maps")
us_cities <- us.cities %>%
  mutate(name1 = sub(" [A-Z]{2}$", "", name))

fisc_map_data <- fisc_2019 %>%
  left_join(us_cities, by = c("city_short" = "name1", "state_abbr" = "country.etc"))

# Prepare map data
us_states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
st_crs(us_states_sf) <- 4326

fisc_map_data_filtered <- fisc_map_data %>%
  filter(!state_abbr %in% c("AK", "HI"), !is.na(long), !is.na(lat)) %>%
  mutate(fiscal_percentile = percent_rank(fiscal_index)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

us_states_proj <- st_transform(us_states_sf, 2163)
fisc_map_proj <- st_transform(fisc_map_data_filtered, 2163)

# Map visualization
ggplot() +
  geom_sf(data = us_states_proj, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_sf(data = fisc_map_proj,
          aes(color = fiscal_percentile, size = city_population),
          alpha = 0.85) +
  scale_color_gradientn(
    colours = c("#d73027", "#fee08b", "#1a9850"),
    name = "Fiscal Health\n(Percentile Rank)",
    limits = c(0, 1),
    labels = percent_format(accuracy = 1)
  ) +
  scale_size_continuous(
    range = c(1, 8),
    name = "City Population",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_minimal(base_size = 13, base_family = "serif4") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  labs(
    title = "Fiscal Health of U.S. Cities (2019)",
    subtitle = "Color = Percentile Rank; Size = Population"
  )

# Top 20 distressed and healthy cities
top_distressed <- fisc_map_data_filtered %>%
  arrange(fiscal_index) %>%
  select(city_name, city_types, city_population, fiscal_index) %>%
  slice(1:20)

top_healthy <- fisc_map_data_filtered %>%
  arrange(desc(fiscal_index)) %>%
  select(city_name, city_types, city_population, fiscal_index) %>%
  slice(1:20)

print(top_distressed)
print(top_healthy)

# Prepare data for Chester vs. peers bar chart
viz_data <- fisc_indicators %>%
  filter(city_name %in% c("PA: Chester", 
                          "Average for All Cities", 
                          "Average for Legacy Cities")) %>%
  mutate(city = case_when(
    city_name == "PA: Chester" ~ "Chester, PA",
    city_name == "Average for All Cities" ~ "National Average",
    city_name == "Average for Legacy Cities" ~ "Legacy Cities Avg."
  )) %>%
  select(city, 
         avg_db = debt_burden_ratio, 
         avg_ds = debt_service_ratio, 
         avg_op = op_balance_ratio, 
         avg_rv = rev_volatility)

viz_long <- viz_data %>%
  pivot_longer(cols = -city,
               names_to = "indicator",
               values_to = "value") %>%
  mutate(value = ifelse(indicator == "avg_rv", log(value + 1), value)) %>%
  mutate(indicator = recode(indicator,
                            avg_db = "Debt Burden",
                            avg_ds = "Debt Service",
                            avg_op = "Operating Balance",
                            avg_rv = "Revenue Volatility")) %>%
  group_by(indicator) %>%
  mutate(scaled_value = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup()

viz_long$indicator <- factor(viz_long$indicator, 
                             levels = c("Operating Balance", "Debt Burden", "Debt Service", "Revenue Volatility"))

chester_data <- viz_long %>%
  filter(city %in% c("Chester, PA", "Legacy Cities Avg.", "National Average")) %>%
  mutate(city = factor(city, levels = c("Chester, PA", "Legacy Cities Avg.", "National Average")))

chester_plot <- ggplot(chester_data, aes(y = indicator, x = scaled_value, fill = city)) +
  geom_col(position = "dodge") +
  labs(
    title = "Chester, PA vs Peers (2012–2022)",
    subtitle = "Rescaled indicator values (0–1)",
    x = "Rescaled Value", y = NULL,
    fill = "City/Group"
  ) +
  scale_fill_manual(values = c(
    "Chester, PA" = "#d73027",
    "Legacy Cities Avg." = "#fb8072",
    "National Average" = "gray40"
  )) +
  theme_minimal(base_size = 13, base_family = "serif4") +
  theme(panel.grid.major.y = element_blank())

# Time series for Chester
line_data <- fisc_indicators %>%
  filter(city_name %in% c("PA: Chester", 
                          "Average for All Cities",
                          "Average for Legacy Cities")) %>%
  select(year, city_name, op_balance_ratio) %>%
  mutate(city = recode(city_name,
                       "PA: Chester" = "Chester, PA",
                       "Average for All Cities" = "National Average",
                       "Average for Legacy Cities" = "Legacy Cities Avg."))

line_plot <- ggplot(line_data, aes(x = year, y = op_balance_ratio, color = city)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "How Has Chester Managed Its Budget Over Time?",
    subtitle = "Tracking Chester’s financial balance compared to national and legacy city averages (2012–2022)",
    x = NULL, y = "Budget Balance (Operating Ratio)",
    color = "City/Group"
  ) +
  scale_color_manual(values = c(
    "Chester, PA" = "#d73027",
    "Legacy Cities Avg." = "#fb8072",
    "National Average" = "gray40"
  )) +
  scale_x_continuous(breaks = 2012:2022) +
  theme_minimal(base_size = 13, base_family = "serif4") +
  theme(panel.grid.major.y = element_blank())

chester_plot + line_plot

# Clustering analysis for early warning
early_data <- fisc_indicators %>%
  filter(year %in% 2012:2016) %>%
  group_by(city_name) %>%
  summarize(
    avg_op = mean(op_balance_ratio, na.rm = TRUE),
    avg_db = mean(debt_burden_ratio, na.rm = TRUE),
    avg_ds = mean(debt_service_ratio, na.rm = TRUE),
    avg_rv = mean(rev_volatility, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(avg_rv = log(avg_rv + 1))

early_scaled <- early_data %>%
  mutate(across(c(avg_op, avg_db, avg_ds, avg_rv), normalize))

set.seed(123)
k_result <- kmeans(early_scaled[, -1], centers = 4, nstart = 25)
early_scaled$cluster <- as.factor(k_result$cluster)

outcome_2019 <- fisc_indicators %>%
  filter(year == 2019) %>%
  select(city_name, fiscal_index_2019 = fiscal_index)

cluster_outcome <- early_scaled %>%
  left_join(outcome_2019, by = "city_name")

ggplot(cluster_outcome, aes(x = cluster, y = fiscal_index_2019, fill = cluster)) +
  geom_boxplot(alpha = 0.8) +
  geom_point(
    data = cluster_outcome %>% filter(city_name == "PA: Chester"),
    aes(x = cluster, y = fiscal_index_2019),
    color = "#d73027", size = 3, shape = 21, fill = "#d73027", stroke = 1.5
  ) +
  labs(
    title = "Could Chester's Early Fiscal Signals Have Predicted Trouble?",
    subtitle = "Cities grouped by 2012–2016 indicators and their 2019 outcomes.",
    x = "Cluster Based on 2012–2016 Indicators",
    y = "Fiscal Index in 2019"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13, base_family = "serif4") +
  theme(panel.grid.major.x = element_blank())