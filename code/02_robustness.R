################################################################################
# Temperature and Aggression Revisited -- Robustness checks
#
# Reproduces the seven robustness tests referenced in the manuscript. Each
# specification is estimated as a standalone feols() call with the formula
# written out in full, then a single etable() call prints all results.
#
# Inputs:  data/data.rds
# Outputs: none -- the combined table is printed to the console.
#
# Run from the archive root:
#   setwd("/path/to/replication_PNAS_Nexus")
#   source("code/02_robustness.R")
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
})

setFixest_etable(signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))

d <- readRDS("data/data.rds") |> as_tibble()


# --- Baseline quadratic ------------------------------------------------------
m_baseline <- feols(
  total_cards ~ temp_max + I(temp_max^2) | season^league^zipcode +
    referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- (i) Cubic specification -------------------------------------------------
m_cubic <- feols(
  total_cards ~ temp_max + I(temp_max^2) + I(temp_max^3) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- (ii) Weather-station elevation control ----------------------------------
m_elevation <- feols(
  total_cards ~ temp_max + I(temp_max^2) + station_elevation_m |
    season^league^zipcode + referee + team_home + team_away,
  data    = d |> filter(!is.na(station_elevation_m)),
  cluster = ~cluster_zip_day
)

# --- (iii) Apparent temperature (heat index) ---------------------------------
m_apparent <- feols(
  total_cards ~ apparent_temp_max + I(apparent_temp_max^2) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d |> filter(!is.na(apparent_temp_max)),
  cluster = ~cluster_zip_day
)

# --- (iv) Adult matches only -------------------------------------------------
m_adult <- feols(
  total_cards ~ temp_max + I(temp_max^2) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d |> filter(age_bracket == "Adult"),
  cluster = ~cluster_zip_day
)

# --- (v) Postal-code-level clustering ----------------------------------------
m_plz_cluster <- feols(
  total_cards ~ temp_max + I(temp_max^2) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d,
  cluster = ~zipcode
)

# --- (vi) Precipitation control ----------------------------------------------
m_precipitation <- feols(
  total_cards ~ temp_max + I(temp_max^2) + precip_any |
    season^league^zipcode + referee + team_home + team_away,
  data    = d |> filter(!is.na(precip_any)),
  cluster = ~cluster_zip_day
)

# --- (vii) Day-of-week and kickoff-hour controls -----------------------------
m_schedule <- feols(
  total_cards ~ temp_max + I(temp_max^2) +
    i(kickoff_hour) + i(day_of_week) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d |> filter(!is.na(kickoff_hour) & !is.na(day_of_week)),
  cluster = ~cluster_zip_day
)

# --- Female leagues only -----------------------------------------------------
m_female <- feols(
  total_cards ~ temp_max + I(temp_max^2) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d |> filter(gender == "Women"),
  cluster = ~cluster_zip_day
)


# --- Output ------------------------------------------------------------------

models <- list(
  m_baseline, m_cubic, m_elevation, m_apparent, m_adult,
  m_plz_cluster, m_precipitation, m_schedule, m_female
)
headers <- c(
  "Baseline", "(i) Cubic", "(ii) Elevation", "(iii) Apparent",
  "(iv) Adult only", "(v) PLZ cluster", "(vi) Precipitation",
  "(vii) DoW+hour", "Female only"
)

etable(models,
       headers = headers,
       drop    = "kickoff_hour|day_of_week")
