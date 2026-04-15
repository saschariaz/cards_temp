################################################################################
#
# Script Reproduces Table 1 and Figure 1 of the manuscript
# Input:  data/data.rds
#
# Run from the archive root:
#   setwd("/path/to/replication_PNAS_Nexus")
#   source("code/01_main_analysis.R")
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(ggplot2)
  library(patchwork)
})

setFixest_etable(signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05))

d <- readRDS("data/data.rds") |> as_tibble()


# --- Table 1, Column 1: Season x League --------------------------------------
m1 <- feols(
  total_cards ~ extreme_heat_32 | season^league,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- Table 1, Column 2: + Referee --------------------------------------------
m2 <- feols(
  total_cards ~ extreme_heat_32 | season^league + referee,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- Table 1, Column 3: + Home and away team --------------------------------
m3 <- feols(
  total_cards ~ extreme_heat_32 |
    season^league + referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- Table 1, Column 4: Season x League x Postal code (preferred) ------------
m4 <- feols(
  total_cards ~ extreme_heat_32 |
    season^league^zipcode + referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- Table 1, Column 5: + Calendar month ------------------------------------
m5 <- feols(
  total_cards ~ extreme_heat_32 |
    season^league^zipcode^month + referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- Print Table 1 to the console --------------------------------------------
etable(m1, m2, m3, m4, m5,
       headers = c("(1)", "(2)", "(3)", "(4)", "(5)"))


# --- Figure 1: 1 C temperature bins used by all three panels ----------------
breaks   <- seq(floor(min(d$temp_max)), ceiling(max(d$temp_max)), by = 1)
bin_mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
d <- d |>
  mutate(temp_bin_idx = cut(temp_max, breaks = breaks,
                            include.lowest = TRUE, labels = FALSE),
         temp_bin_mid = bin_mids[temp_bin_idx])


# --- Figure 1, Panel A: quadratic in temperature, total cards ----------------
q_total <- feols(
  total_cards ~ temp_max + I(temp_max^2) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- Figure 1, Panel B: quadratic in temperature, yellow cards only ---------
q_yellow <- feols(
  total_yellow ~ temp_max + I(temp_max^2) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)

# --- Figure 1, Panel C: quadratic in temperature, red cards only -------------
q_red <- feols(
  total_red ~ temp_max + I(temp_max^2) |
    season^league^zipcode + referee + team_home + team_away,
  data    = d,
  cluster = ~cluster_zip_day
)


# --- Helper: turn one quadratic fit into one ggplot panel --------------------
# Residualises the outcome on the preferred FE, builds a 1 C binned scatter
# (dropping bins with fewer than 50 matches), overlays the parametric
# quadratic + pointwise 95% CI, and centres everything at the estimated
# turning point -b1/(2*b2).
make_panel <- function(outcome, fit_q, title) {

  # Residualise on the preferred FE. fixef.rm="none" keeps singletons so
  # the residual vector aligns row-for-row with d.
  fit_r <- feols(as.formula(paste(outcome, "~ 1 |",
                                  "season^league^zipcode + referee",
                                  "+ team_home + team_away")),
                 data = d, fixef.rm = "none")

  binned <- tibble(temp_bin_mid = d$temp_bin_mid,
                   resid        = residuals(fit_r)) |>
    filter(!is.na(temp_bin_mid)) |>
    summarise(y_mean = mean(resid), n = n(), .by = temp_bin_mid) |>
    arrange(temp_bin_mid) |>
    filter(n >= 50)

  # Coefficients by position: fit_q has {temp_max, I(temp_max^2)} in order.
  # fixest (>= 0.12) relabels I() terms so we avoid name-based lookup.
  cf <- coef(fit_q)
  V  <- vcov(fit_q)[1:2, 1:2]
  b1 <- cf[1]; b2 <- cf[2]
  tp <- -b1 / (2 * b2)  # turning point: derivative of b1*T + b2*T^2 = 0

  # Fitted curve + delta-method 95% CI on a fine grid.
  # Var(b1*T + b2*T^2) = T^2*V11 + T^4*V22 + 2*T^3*V12.
  grid    <- seq(min(binned$temp_bin_mid), max(binned$temp_bin_mid), by = 0.5)
  y_hat   <- b1 * grid + b2 * grid^2
  se_grid <- sqrt(grid^2 * V[1, 1] + grid^4 * V[2, 2] + 2 * grid^3 * V[1, 2])

  # Centre the fit and the bin means at the turning point.
  offset  <- y_hat[which.min(abs(grid - tp))]
  curve   <- tibble(temp   = grid,
                    fitted = y_hat - offset,
                    ci_lo  = y_hat - 1.96 * se_grid - offset,
                    ci_hi  = y_hat + 1.96 * se_grid - offset)
  binned  <- binned |>
    mutate(y_centered = y_mean - y_mean[which.min(abs(temp_bin_mid - tp))])

  ggplot() +
    geom_ribbon(data = curve,  aes(x = temp, ymin = ci_lo, ymax = ci_hi),
                fill = "grey75", alpha = 0.5) +
    geom_line  (data = curve,  aes(x = temp, y = fitted),
                linewidth = 0.7, colour = "black") +
    geom_point (data = binned, aes(x = temp_bin_mid, y = y_centered, size = n),
                shape = 21, fill = "grey40", colour = "white",
                stroke = 0.3, alpha = 0.8) +
    geom_vline (xintercept = tp, linetype = "dashed",
                colour = "grey50", linewidth = 0.4) +
    scale_size_continuous(range = c(0.5, 3.5), guide = "none") +
    scale_x_continuous(breaks = seq(-5, 35, by = 5)) +
    labs(x = expression("Max. temperature during match (" * degree * "C)"),
         y = "Residualized cards\n(relative to turning point)",
         title = title) +
    theme_minimal(base_size = 8) +
    theme(plot.title       = element_text(face = "bold", size = 9, hjust = 0.5),
          axis.title       = element_text(size = 7),
          axis.text        = element_text(size = 6),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.2, colour = "grey90"),
          plot.margin      = margin(4, 6, 4, 4))
}


# --- Assemble Figure 1 with patchwork ---------------------------------------
panel_A <- make_panel("total_cards",  q_total,  "Total Cards")
panel_B <- make_panel("total_yellow", q_yellow, "Yellow Cards")
panel_C <- make_panel("total_red",    q_red,    "Red Cards")

figure1 <- panel_A + panel_B + panel_C +
  plot_layout(ncol = 3) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 10))
  )

print(figure1)
