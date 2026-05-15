
## See README for further details

## Only run this one script.
## try to run each section one at time to see what it does.


# ____________________________
#
## -  Section 1: Load environment, models, functions ----
#     always run
# ____________________________

source("scripts/010_load_environment.R") # libraries and configuration (parameters)
source("scripts/020_models.R")
source("scripts/030_functions.R")

# set seed here owing to imputing needs in data 
set.seed(cfg$run$seed)

source("scripts/040_get_data.R") # all data - all seasons
source("scripts/050_initialisation.R") # creates model based on base season


# ____________________________
#
## -  Section 2: Show data ----
# ____________________________

# ---------------------------
# Plot (similar to Figure 2 in the article)
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



# ____________________________
#
## -  Section 3: Basic optimisation ----
# ____________________________

# set up tibble with a given seq of changepoints (vector of c/p locations)
df_example_runs1 <-  tibble(run = 1)
df_example_runs1$seq <- list(cfg$run$given_seq)

# function requires at least 1 seq to run plus teh pomp model and the data series
# not that this is _max_ which runs slower than _min_ version (will work in Posit server)
df_optim_runs1 <- fun_max_optim_beta_multi_runner(df_example_runs1, fluASEIR, df_infected)

# two data frames are returned: trajectory (including epochs) & info (log likelihood, vector of beta values)

# vector of beta values
df_optim_runs1$df_info$beta

# join reports
df_to_plot <-
  df_infected %>% 
  dplyr::select(day, reports) %>% 
  right_join(df_optim_runs1$df_covar_traj, join_by(day))

# quick plot
df_to_plot %>% 
ggplot() +
  geom_vline(data = df_to_plot %>% filter(is_changepoint == 1), aes(xintercept = day),
             col = "#E1ADE0", linewidth = 1) +
  geom_line(aes(x=day, y=traj), colour="#C63B36", linewidth = 1) +
  geom_line(aes(x=day, y=reports), colour="black") +
  theme_minimal()
