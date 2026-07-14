# install.packages(c("tidyverse", "logistf", "sandwich", "lmtest", "openxlsx"))

library(tidyverse)
library(logistf)
library(sandwich)
library(lmtest)
library(openxlsx)


# 1. path

PROJECT_DIR <- "" # insert your directory

DATA_FILE <- file.path(
  PROJECT_DIR,
  "data_processed",
  "WBC_umpire_analysis_dataset.rds"
)

OUT_DIR <- file.path(PROJECT_DIR, "results_models")

if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}

OUT_XLSX <- file.path(OUT_DIR, "WBC_Firth_Model_Results.xlsx")
OUT_RDS  <- file.path(OUT_DIR, "WBC_Firth_Model_Objects.rds")


# 2. Road Data

df <- readRDS(DATA_FILE)


# 3. Check variables
# ============================================================

required_vars <- c(
  "error", "actual",
  "balls", "strikes",
  "border_dist", "border_dist_inch", "plate_z_norm",
  "bat_Asia", "bat_Europe", "bat_LatAm", "bat_Oceania",
  "pit_Asia", "pit_Europe", "pit_LatAm", "pit_Oceania",
  "stand_L", "p_throws_L", "round_T",
  "Ump_HP"
)

missing_vars <- setdiff(required_vars, names(df))

if (length(missing_vars) > 0) {
  stop(
    "No variables:\n",
    paste(missing_vars, collapse = ", ")
  )
}

cat("Variable Complete\n\n")


# ============================================================
# 4. border dist varaible

DIST_VAR <- "border_dist_inch"


# 5. Firth logit + cluster-robust SE 
run_firth_cluster <- function(formula, data, cluster_var = "Ump_HP") {
  
  # 5-1. 
  mf <- model.frame(formula, data = data, na.action = na.omit)
  used_rows <- as.integer(rownames(mf))
  data_used <- data[used_rows, ]
  
  # 5-2. Firth logistic regression
  fit_f <- logistf(
    formula,
    data = data_used,
    firth = TRUE,
    control = logistf.control(maxit = 200)
  )
  
  # 5-3. cluster-robust SE logit
  fit_g <- glm(
    formula,
    data = data_used,
    family = binomial()
  )
  
  cl <- data_used[[cluster_var]]
  
  vcov_cl <- vcovCL(fit_g, cluster = cl, type = "HC1")
  se_cl <- sqrt(diag(vcov_cl))
  
  # 5-4. Firth coeficient + cluster SE 
  b <- coef(fit_f)
  
  common_names <- intersect(names(b), names(se_cl))
  
  b_use <- b[common_names]
  se_use <- se_cl[common_names]
  
  z_cl <- b_use / se_use
  p_cl <- 2 * pnorm(-abs(z_cl))
  
  OR <- exp(b_use)
  CI_lo <- exp(b_use - 1.96 * se_use)
  CI_hi <- exp(b_use + 1.96 * se_use)
  
  p_firth <- fit_f$prob[common_names]
  
  sig <- function(p) {
    case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1   ~ ".",
      TRUE      ~ ""
    )
  }
  
  result <- tibble(
    Variable = common_names,
    Estimate = round(b_use, 5),
    SE_cluster = round(se_use, 5),
    OR = round(OR, 4),
    CI_lo = round(CI_lo, 4),
    CI_hi = round(CI_hi, 4),
    z = round(z_cl, 3),
    p = round(p_cl, 4),
    sig = sig(p_cl),
    p_firth = round(p_firth, 4)
  )
  
  # 5-5. AUC 
  pred <- predict(fit_f, type = "response")
  y <- data_used$error
  
  n1 <- sum(y == 1, na.rm = TRUE)
  n0 <- sum(y == 0, na.rm = TRUE)
  
  auc <- ifelse(
    n1 > 0 & n0 > 0,
    (sum(rank(pred)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0),
    NA_real_
  )
  
  # 5-6. models
  model_info <- tibble(
    n = nrow(data_used),
    n_error = sum(data_used$error, na.rm = TRUE),
    error_rate = round(mean(data_used$error, na.rm = TRUE) * 100, 2),
    auc = round(auc, 4),
    n_cluster = n_distinct(cl)
  )
  
  list(
    fit_f = fit_f,
    fit_g = fit_g,
    result = result,
    model_info = model_info,
    data_used = data_used
  )
}


# 6. Study 1: Firth Main effect model 

cat("\n====== STUDY 1: MAIN EFFECT MODELS ======\n")

form_s1_base <- error ~
  balls + strikes +
  bat_Asia + bat_Europe + bat_LatAm + bat_Oceania +
  pit_Asia + pit_Europe + pit_LatAm + pit_Oceania +
  stand_L + p_throws_L + round_T

m1_data <- df %>% filter(actual == "ball")
m2_data <- df %>% filter(actual == "strike")


# Study 1-1. BCS model
cat("\n[Study 1 - BCS]\n")

out_s1_bcs_base <- run_firth_cluster(
  formula = form_s1_base,
  data = m1_data,
  cluster_var = "Ump_HP"
)

print(out_s1_bcs_base$model_info)

out_s1_bcs_base$result %>%
  filter(Variable != "(Intercept)") %>%
  print(n = Inf)


# Study 1-2. SCB model
cat("\n[Study 1 - SCB]\n")

out_s1_scb_base <- run_firth_cluster(
  formula = form_s1_base,
  data = m2_data,
  cluster_var = "Ump_HP"
)

print(out_s1_scb_base$model_info)

out_s1_scb_base$result %>%
  filter(Variable != "(Intercept)") %>%
  print(n = Inf)


# 7. Study 2: border_dist interaction model 
cat("\n====== STUDY 2: BORDER DIST INTERACTION MODELS ======\n")

# 7-1. interaction
df <- df %>%
  mutate(
    ix_batAsia_dist    = bat_Asia    * .data[[DIST_VAR]],
    ix_batEurope_dist  = bat_Europe  * .data[[DIST_VAR]],
    ix_batLatAm_dist   = bat_LatAm   * .data[[DIST_VAR]],
    ix_batOceania_dist = bat_Oceania * .data[[DIST_VAR]],
    
    ix_pitAsia_dist    = pit_Asia    * .data[[DIST_VAR]],
    ix_pitEurope_dist  = pit_Europe  * .data[[DIST_VAR]],
    ix_pitLatAm_dist   = pit_LatAm   * .data[[DIST_VAR]],
    ix_pitOceania_dist = pit_Oceania * .data[[DIST_VAR]]
  )

m1_data <- df %>% filter(actual == "ball")
m2_data <- df %>% filter(actual == "strike")


# 7-2. Study 2 BCS model
form_s2_bcs <- as.formula(
  paste(
    "error ~",
    DIST_VAR, "+ plate_z_norm +",
    "balls + strikes +",
    "bat_Asia + bat_Europe + bat_LatAm + bat_Oceania +",
    "pit_Asia + pit_Europe + pit_LatAm + pit_Oceania +",
    "stand_L + p_throws_L + round_T +",
    "ix_batAsia_dist + ix_batEurope_dist +",
    "ix_batLatAm_dist + ix_batOceania_dist"
  )
)


# 7-3. Study 2 SCB model
form_s2_scb <- as.formula(
  paste(
    "error ~",
    DIST_VAR, "+ plate_z_norm +",
    "balls + strikes +",
    "bat_Asia + bat_Europe + bat_LatAm + bat_Oceania +",
    "pit_Asia + pit_Europe + pit_LatAm + pit_Oceania +",
    "stand_L + p_throws_L + round_T +",
    "ix_pitAsia_dist + ix_pitEurope_dist +",
    "ix_pitLatAm_dist + ix_pitOceania_dist"
  )
)


# 7-4. Study 2 BCS model
cat("\n[Study 2 - BCS: Batter Continent × border_dist]\n")

out_s2_bcs <- run_firth_cluster(
  formula = form_s2_bcs,
  data = m1_data,
  cluster_var = "Ump_HP"
)

print(out_s2_bcs$model_info)

out_s2_bcs$result %>%
  filter(Variable != "(Intercept)") %>%
  print(n = Inf)


# 7-5. Study 2 SCB Model
cat("\n[Study 2 - SCB: Pitcher Continent × border_dist]\n")

out_s2_scb <- run_firth_cluster(
  formula = form_s2_scb,
  data = m2_data,
  cluster_var = "Ump_HP"
)

print(out_s2_scb$model_info)

out_s2_scb$result %>%
  filter(Variable != "(Intercept)") %>%
  print(n = Inf)


# 8. Results

s1_bcs_result <- out_s1_bcs_base$result %>%
  mutate(Model = "Study 1 BCS") %>%
  relocate(Model)

s1_scb_result <- out_s1_scb_base$result %>%
  mutate(Model = "Study 1 SCB") %>%
  relocate(Model)

s2_bcs_result <- out_s2_bcs$result %>%
  mutate(Model = "Study 2 BCS") %>%
  relocate(Model)

s2_scb_result <- out_s2_scb$result %>%
  mutate(Model = "Study 2 SCB") %>%
  relocate(Model)

all_model_results <- bind_rows(
  s1_bcs_result,
  s1_scb_result,
  s2_bcs_result,
  s2_scb_result
)

model_summary <- bind_rows(
  out_s1_bcs_base$model_info %>%
    mutate(Model = "Study 1 BCS"),
  
  out_s1_scb_base$model_info %>%
    mutate(Model = "Study 1 SCB"),
  
  out_s2_bcs$model_info %>%
    mutate(Model = "Study 2 BCS"),
  
  out_s2_scb$model_info %>%
    mutate(Model = "Study 2 SCB")
) %>%
  relocate(Model) %>%
  mutate(distance_variable_study2 = DIST_VAR)

key_terms <- c(
  "bat_Asia", "bat_Europe", "bat_LatAm", "bat_Oceania",
  "pit_Asia", "pit_Europe", "pit_LatAm", "pit_Oceania",
  DIST_VAR,
  "ix_batAsia_dist", "ix_batEurope_dist",
  "ix_batLatAm_dist", "ix_batOceania_dist",
  "ix_pitAsia_dist", "ix_pitEurope_dist",
  "ix_pitLatAm_dist", "ix_pitOceania_dist"
)

key_model_results <- all_model_results %>%
  filter(Variable %in% key_terms)
