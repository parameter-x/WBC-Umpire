install.packages(c("tidyverse"))
library(tidyverse)


# ============================================================
# 1. Path
# ============================================================

PROJECT_DIR <- "" # insert your directory

RAW_FILE <- file.path(PROJECT_DIR, "umpire.csv")

OUT_DIR <- file.path(PROJECT_DIR, "data_processed")

if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}


OUT_CSV <- file.path(OUT_DIR, "WBC_umpire_analysis_dataset.csv")


# ============================================================
# 2. Road Data
# ============================================================

df_raw <- read_csv(RAW_FILE, show_col_types = FALSE)

cat("Raw Data Road\n")
cat("Raw :", nrow(df_raw), "\n")
cat("Column :", ncol(df_raw), "\n\n")


# ============================================================
# 3. Main Variables
# ============================================================

required_vars <- c(
  "Type",
  "inning_topbot",
  "away_team", "home_team",
  "game_type",
  "plate_x", "plate_z",
  "sz_bot", "sz_top",
  "balls", "strikes",
  "stand", "p_throws",
  "Ump_HP", "game_pk"
)

missing_vars <- setdiff(required_vars, names(df_raw))

if (length(missing_vars) > 0) {
  stop(
    "No Main Variables:\n",
    paste(missing_vars, collapse = ", ")
  )
}

cat("Variables Complete \n\n")


# ============================================================
# 4. Assign nation Continent
# ============================================================

cont_map <- c(
  USA = "North America",
  CAN = "North America",
  MEX = "North America",
  
  PUR = "Latin America",
  DOM = "Latin America",
  VEN = "Latin America",
  CUB = "Latin America",
  PAN = "Latin America",
  COL = "Latin America",
  NCA = "Latin America",
  BRA = "Latin America",
  
  JPN = "Asia",
  KOR = "Asia",
  TPE = "Asia",
  
  ITA = "Europe",
  NED = "Europe",
  GBR = "Europe",
  CZE = "Europe",
  ISR = "Europe",
  
  AUS = "Oceania"
)


# ============================================================
# 5. pmax / pmin function
# ============================================================

safe_pmax2 <- function(x, z) {
  if_else(
    is.na(x) & is.na(z),
    NA_real_,
    pmax(x, z, na.rm = TRUE)
  )
}

safe_pmin2 <- function(x, z) {
  if_else(
    is.na(x) & is.na(z),
    NA_real_,
    pmin(x, z, na.rm = TRUE)
  )
}


# ============================================================
# 6. make new variables ----
# ============================================================

df <- df_raw %>%
  mutate(
    actual = case_when(
      Type %in% c("ball-ball", "ball-st") ~ "ball",
      Type %in% c("st-ball", "st-st") ~ "strike",
      TRUE ~ NA_character_
    ),

    called = case_when(
      Type %in% c("ball-ball", "st-ball") ~ "ball",
      Type %in% c("ball-st", "st-st") ~ "strike",
      TRUE ~ NA_character_
    ),
    
    # error:
    #   ball-st = BCS, st-ball = SCB
    error = as.integer(Type %in% c("ball-st", "st-ball")),
    
    # error_type:
    error_type = Type,
    
    batter_team = if_else(inning_topbot == "Top", away_team, home_team),
    pitcher_team = if_else(inning_topbot == "Top", home_team, away_team),
    
    batter_continent = unname(cont_map[batter_team]),
    pitcher_continent = unname(cont_map[pitcher_team]),
    
    round = if_else(game_type == "F", "Preliminary", "Tournament"),
    
    dist_x = abs(plate_x) - 0.7083,
 
    dist_z = case_when(
      plate_z < sz_bot ~ sz_bot - plate_z,
      plate_z > sz_top ~ plate_z - sz_top,
      plate_z >= sz_bot & plate_z <= sz_top ~
        -pmin(plate_z - sz_bot, sz_top - plate_z),
      TRUE ~ NA_real_
    ),
    
    # pmax_dist:
    pmax_dist = safe_pmax2(dist_x, dist_z),
    
    # border_dist:
    border_dist = case_when(
      actual == "ball" ~ safe_pmax2(dist_x, dist_z),
      actual == "strike" ~ abs(safe_pmax2(dist_x, dist_z)),
      TRUE ~ NA_real_
    ),
    
    # border_dist_inch:
    border_dist_inch = border_dist * 12,
    
    # plate_x_abs:
    plate_x_abs = abs(plate_x),
    
    # plate_z_norm:
    plate_z_norm = (plate_z - sz_bot) / (sz_top - sz_bot),
    
    # balls & strikes
    count_label = paste0(balls, "-", strikes)
  )


# ============================================================
# 7. Dummy variables
# ============================================================

df <- df %>%
  mutate(
    # batter's direction
    stand_L = as.integer(stand == "L"),
    
    # pitcher's direction
    p_throws_L = as.integer(p_throws == "L"),
    
    # Round
    round_T = as.integer(round == "Tournament"),
    
    # Batter Criterion Group: North America
    bat_Asia = as.integer(batter_continent == "Asia"),
    bat_Europe = as.integer(batter_continent == "Europe"),
    bat_LatAm = as.integer(batter_continent == "Latin America"),
    bat_Oceania = as.integer(batter_continent == "Oceania"),
    

    # Pitcher Criterion Group: North America
    pit_Asia = as.integer(pitcher_continent == "Asia"),
    pit_Europe = as.integer(pitcher_continent == "Europe"),
    pit_LatAm = as.integer(pitcher_continent == "Latin America"),
    pit_Oceania = as.integer(pitcher_continent == "Oceania")
  )


# ============================================================
# 8. Context
# ============================================================

cnt_order <- c(
  "0-0", "0-1", "0-2",
  "1-0", "1-1", "1-2",
  "2-0", "2-1", "2-2",
  "3-0", "3-1", "3-2"
)

df <- df %>%
  mutate(
    actual = factor(actual, levels = c("ball", "strike")),
    called = factor(called, levels = c("ball", "strike")),
    
    batter_continent = factor(
      batter_continent,
      levels = c("North America", "Asia", "Europe", "Latin America", "Oceania")
    ),
    
    pitcher_continent = factor(
      pitcher_continent,
      levels = c("North America", "Asia", "Europe", "Latin America", "Oceania")
    ),
    
    round = factor(round, levels = c("Preliminary", "Tournament")),
    count_label = factor(count_label, levels = cnt_order),
    
    stand = factor(stand),
    p_throws = factor(p_throws)
  )


# ============================================================
# 9. Save Data
# ============================================================

# RDS:
saveRDS(df, OUT_RDS)

# CSV:
write_csv(df, OUT_CSV)
