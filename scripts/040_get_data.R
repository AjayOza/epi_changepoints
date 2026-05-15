# ---------------------------
# Parameters
# ---------------------------
data_path <- file.path(cfg$paths$data_dir, "reports.csv")
lambda_impute <- cfg$run$lambda_impute
base_season <- cfg$run$base_season
base_floor_date <- cfg$run$base_floor_date  # base date for day=0 (or day=1 depending on your data)

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
