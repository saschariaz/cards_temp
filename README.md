# Replication archive — Temperature and Aggression Revisited

Paper: **Temperature and Aggression Revisited: Evidence from 1 Million Amateur
Football Matches**
Authors: Sascha Riaz, Maurice Baudet von Gersdorff, Heike Klüver
Journal: PNAS Nexus (manuscript PNASNEXUS-2026-00494)

## Contents

```
replication_PNAS_Nexus/
├── README.md
├── data/
│   └── data.rds                 Cleaned, analysis-ready match panel.
└── code/
    ├── 01_main_analysis.R       Table 1 + Figure 1.
    └── 02_robustness.R          Seven robustness tests + female subset.
```

## Requirements

R ≥ 4.3 with `dplyr`, `fixest` (≥ 0.12), `ggplot2`, `patchwork`.

```r
install.packages(c("dplyr", "fixest", "ggplot2", "patchwork"))
```

## How to run

```r
setwd("/path/to/replication_PNAS_Nexus")
source("code/01_main_analysis.R")
source("code/02_robustness.R")
```

## Data dictionary — `data/data.rds`

One row per match. 997,586 matches × 20 columns.

| Column | Type | Description |
|---|---|---|
| `total_cards`         | numeric   | Total cards (yellow + red + second yellow, both teams). |
| `total_yellow`        | numeric   | Yellow cards (both teams). |
| `total_red`           | numeric   | Straight red cards (both teams). |
| `temp_max`            | numeric   | Maximum air temperature (°C) during the 3-hour window starting at kickoff. |
| `extreme_heat_32`     | numeric   | Binary indicator = 1 if `temp_max ≥ 32 °C`. |
| `apparent_temp_max`   | numeric   | Apparent temperature / heat index (°C). |
| `station_elevation_m` | integer   | Elevation of the nearest DWD weather station (metres). |
| `precip_any`          | integer   | Binary indicator for any measurable precipitation during the match window |
| `kickoff_hour`        | integer   | Hour of match kickoff (0–23). |
| `day_of_week`         | integer   | Day of the week (1 = Monday … 7 = Sunday). |
| `season`              | character | Football season. Levels: `"2223"`, `"2324"`, `"2425"`. |
| `league`              | character | Division × age bracket × gender (e.g. `"Kreisliga A_Adult_Men"`). |
| `zipcode`             | character | Postal code of the home venue. |
| `referee`             | character | Referee identifier. |
| `team_home`           | character | Home team identifier. |
| `team_away`           | character | Away team identifier. |
| `cluster_zip_day`     | character | `zipcode` × match date. Clustering unit for standard errors. |
| `age_bracket`         | character | `Adult`, `U17`, or `U19`. |
| `gender`              | character | `Men` or `Women`. |
| `month`               | integer   | Calendar month (1–12). |
