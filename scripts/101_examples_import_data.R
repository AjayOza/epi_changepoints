
# See README for further details

# ---------------------------
# Libraries
# ---------------------------
library(tidyverse)  # includes ggplot2, dplyr, readr, etc.
library(lubridate)  # for epiweek()

# ---------------------------
# Parameters
# ---------------------------
data_path <- file.path("data", "reports.csv")
lambda_impute <- 1.26
base_season <- "2024/2025"
base_floor_date <- as.Date("2024-09-07")  # base date for day=0 (or day=1 depending on your data)

# ---------------------------
# Read data
# ---------------------------
df_infected_all_seasons <- read.csv(data_path, stringsAsFactors = FALSE)

# Optional: basic column check (fails early if schema changed)
required_cols <- c("day", "reports_raw", "season_name")
missing_cols <- setdiff(required_cols, names(df_infected_all_seasons))
if (length(missing_cols) > 0) {
  stop(
    sprintf(
      "Missing required column(s): %s",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

# Ensure expected types
df_infected_all_seasons <- df_infected_all_seasons %>%
  mutate(
    day = as.integer(day),
    reports_raw = suppressWarnings(as.numeric(reports_raw))
  )

# ---------------------------
# Impute missing reports_raw using Poisson(lambda)
# ---------------------------
z_blanks <- is.na(df_infected_all_seasons$reports_raw)
n_blanks <- sum(z_blanks)

if (n_blanks > 0) {
  df_infected_all_seasons$reports[z_blanks] <- rpois(n = n_blanks, lambda = lambda_impute)
  message(sprintf("Imputed %d missing reports_raw value(s) using Poisson(lambda = %.2f).", n_blanks, lambda_impute))
} else {
  message("No missing reports_raw values found. No imputation performed.")
}


### ### #######
##      NOTE!!!
##
##      df_infected_all_seasons is now the object with data used in the article
##
### ### #######

# ---------------------------
# Create a calendar mapping (day -> date + epiweek) using a reference season
# ---------------------------
df_calendar <- df_infected_all_seasons %>%
  filter(season_name == base_season) %>%
  transmute(
    day,
    case_date = base_floor_date + day,
    wk = epiweek(case_date)
  ) %>%
  distinct(day, .keep_all = TRUE)

# ---------------------------
# Plot (similar to your Figure 2)
# ---------------------------
df_infected_all_seasons %>%
  left_join(df_calendar, by = "day") %>%
  ggplot(aes(x = case_date, y = reports, colour = season_name)) +
  geom_line() +
  scale_x_date(
    date_breaks = "1 week",
    labels = function(x) lubridate::epiweek(x)
  ) +
  labs(
    x = "Day (week numbers shown)",
    y = "Reports",
    colour = "Season"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3)
  )
