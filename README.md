# Replication Materials

This repository contains the data, variable definitions, and R scripts supporting the manuscript:

**Unequal Zones: Spatial and Directional Asymmetries in Umpire Ball–Strike Decisions at the 2026 World Baseball Classic**

## Repository Contents

### Data

* **umpire.csv**

  * Raw pitch-level Statcast data used in this study.

### Variable Definitions

* **Variable_Definitions.xlsx**

  * Definitions of the variables included in the raw dataset and the variables used in the statistical analyses.

### R Scripts

The R scripts should be executed in the following order:

1. **1_Preprocessing.R**

   * Cleans the raw data, constructs the analysis variables, and creates the analysis dataset.

2. **2_Descriptive_Statistics.R**

   * Produces the descriptive statistics reported in the manuscript.

3. **3_Firth_Models.R**

   * Performs the Firth penalized logistic regression analyses presented in the manuscript.

## Data Source

The raw data were obtained from the publicly available Baseball Savant Statcast platform.

https://baseballsavant.mlb.com/statcast_search/

## Running the Scripts

Before running the scripts, set `PROJECT_DIR` in each R script to the local directory containing the repository files.

The required R packages are specified within each script.
