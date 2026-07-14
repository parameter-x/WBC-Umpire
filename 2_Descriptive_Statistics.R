install.packages(c("tidyverse", "openxlsx"))

library(tidyverse)
library(openxlsx)


# ============================================================
# 1. Path
# ============================================================

PROJECT_DIR <- "" # insert your directory

DATA_FILE <- file.path(
  PROJECT_DIR,
  "data_processed",
  "WBC_umpire_analysis_dataset.rds"
)

OUT_DIR <- file.path(PROJECT_DIR, "results_descriptive")

if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}

OUT_XLSX <- file.path(
  OUT_DIR,
  "WBC_Descriptive_Statistics.xlsx"
)


# ============================================================
# 2. Road Data
# ============================================================

df <- readRDS(DATA_FILE)


# ============================================================
# 3. Context
# ============================================================

cnt_order <- c(
  "0-0", "0-1", "0-2",
  "1-0", "1-1", "1-2",
  "2-0", "2-1", "2-2",
  "3-0", "3-1", "3-2"
)

continent_order <- c(
  "North America",
  "Asia",
  "Europe",
  "Latin America",
  "Oceania"
)


# ============================================================
# 4. inch 
# ============================================================

df <- df %>%
  mutate(
    border_dist_inch_floor = floor(border_dist_inch),
    
    inch_bin = case_when(
      is.na(border_dist_inch) ~ NA_character_,
      border_dist_inch < 0 ~ "Negative",
      border_dist_inch >= 12 ~ "12+",
      TRUE ~ paste0(
        floor(border_dist_inch),
        "-",
        floor(border_dist_inch) + 1
      )
    ),
    
    inch_bin = factor(
      inch_bin,
      levels = c(
        paste0(0:11, "-", 1:12),
        "12+",
        "Negative"
      )
    )
  )


# ============================================================
# 5. frequency & percentile
# ============================================================

# 5-1
type_count <- df %>%
  count(error_type, name = "n") %>%
  mutate(
    pct = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(error_type)


# 5-2
overall_error <- df %>%
  summarise(
    n = n(),
    error_n = sum(error, na.rm = TRUE),
    error_rate = round(mean(error, na.rm = TRUE) * 100, 2)
  )


# 5-3
actual_count <- df %>%
  count(actual, name = "n") %>%
  mutate(
    pct = round(n / sum(n) * 100, 2)
  )


# 5-4
error_type_summary <- df %>%
  group_by(actual, called, error_type) %>%
  summarise(
    n = n(),
    error_n = sum(error, na.rm = TRUE),
    error_rate = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )


# 6. Miscall by Continent

# 6-1. BCS: Miscall by batter's continent
desc_bcs_batter <- df %>%
  filter(actual == "ball") %>%
  group_by(batter_continent) %>%
  summarise(
    n_bcs = n(),
    err_bcs = sum(error, na.rm = TRUE),
    rate_bcs = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(factor(batter_continent, levels = continent_order))

# ------------------------------------------------------------
# 6-2. SCB: Miscall by pitcher's continent
# ------------------------------------------------------------

desc_scb_pitcher <- df %>%
  filter(actual == "strike") %>%
  group_by(pitcher_continent) %>%
  summarise(
    n_scb = n(),
    err_scb = sum(error, na.rm = TRUE),
    rate_scb = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(factor(pitcher_continent, levels = continent_order))


# ============================================================
# 7. miscall rate of ball counts 
# ============================================================

desc_count <- df %>%
  group_by(count_label) %>%
  summarise(
    n = n(),
    err = sum(error, na.rm = TRUE),
    rate = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  left_join(
    df %>%
      filter(actual == "ball") %>%
      group_by(count_label) %>%
      summarise(
        n_bcs = n(),
        err_bcs = sum(error, na.rm = TRUE),
        rate_bcs = round(mean(error, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ),
    by = "count_label"
  ) %>%
  left_join(
    df %>%
      filter(actual == "strike") %>%
      group_by(count_label) %>%
      summarise(
        n_scb = n(),
        err_scb = sum(error, na.rm = TRUE),
        rate_scb = round(mean(error, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ),
    by = "count_label"
  ) %>%
  mutate(count_label = factor(count_label, levels = cnt_order)) %>%
  arrange(count_label)


# ============================================================
# 8. border_dist statistics
# ============================================================

desc_dist <- df %>%
  group_by(error_type) %>%
  summarise(
    n = n(),
    mean = round(mean(border_dist, na.rm = TRUE), 3),
    sd = round(sd(border_dist, na.rm = TRUE), 3),
    min = round(min(border_dist, na.rm = TRUE), 3),
    q1 = round(quantile(border_dist, 0.25, na.rm = TRUE), 3),
    median = round(median(border_dist, na.rm = TRUE), 3),
    q3 = round(quantile(border_dist, 0.75, na.rm = TRUE), 3),
    max = round(max(border_dist, na.rm = TRUE), 3),
    mean_inch = round(mean(border_dist_inch, na.rm = TRUE), 3),
    sd_inch = round(sd(border_dist_inch, na.rm = TRUE), 3),
    .groups = "drop"
  )


desc_dist_actual <- df %>%
  group_by(actual) %>%
  summarise(
    n = n(),
    mean = round(mean(border_dist, na.rm = TRUE), 3),
    sd = round(sd(border_dist, na.rm = TRUE), 3),
    min = round(min(border_dist, na.rm = TRUE), 3),
    median = round(median(border_dist, na.rm = TRUE), 3),
    max = round(max(border_dist, na.rm = TRUE), 3),
    mean_inch = round(mean(border_dist_inch, na.rm = TRUE), 3),
    sd_inch = round(sd(border_dist_inch, na.rm = TRUE), 3),
    .groups = "drop"
  )


# 9. BCS / SCB miscall rate by inch

# 9-1. BCS: inch 

inch_bcs_all <- df %>%
  filter(actual == "ball") %>%
  group_by(inch_bin) %>%
  summarise(
    n_bcs = n(),
    err_bcs = sum(error, na.rm = TRUE),
    rate_bcs = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(inch_bin)


# 9-2. SCB: inch 

inch_scb_all <- df %>%
  filter(actual == "strike") %>%
  group_by(inch_bin) %>%
  summarise(
    n_scb = n(),
    err_scb = sum(error, na.rm = TRUE),
    rate_scb = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(inch_bin)


# 9-3. BCS: batter continent × inch

inch_bcs_batter <- df %>%
  filter(actual == "ball") %>%
  group_by(batter_continent, inch_bin) %>%
  summarise(
    n_bcs = n(),
    err_bcs = sum(error, na.rm = TRUE),
    rate_bcs = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(
    factor(batter_continent, levels = continent_order),
    inch_bin
  )


# ------------------------------------------------------------
# 9-4. SCB: pitcher continent × inch 
# ------------------------------------------------------------

inch_scb_pitcher <- df %>%
  filter(actual == "strike") %>%
  group_by(pitcher_continent, inch_bin) %>%
  summarise(
    n_scb = n(),
    err_scb = sum(error, na.rm = TRUE),
    rate_scb = round(mean(error, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(
    factor(pitcher_continent, levels = continent_order),
    inch_bin
  )


