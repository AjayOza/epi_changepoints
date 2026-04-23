# libraries 
library(tidyverse)
library(ggplot2)

# read the csv file
df_infected_all_seasons <- read.csv("data/reports.csv", stringsAsFactors = FALSE)

# replace NA in reports_raw with randint 0–4 using Poisson with mean of 1.25
z_blanks <- is.na(df_infected_all_seasons$reports_raw)
df_infected_all_seasons$reports[z_blanks] <- rpois(n = sum(z_blanks), lambda = 1.25)

# the floor date of the 2024/2025 season
param_base_floor_date <- as.Date("2024-09-07", format="%Y-%m-%d")

# create a temp calendar to store dates
df_calendar <- 
  df_infected_all_seasons %>% 
  filter(season_name=="2024/2025") %>% 
  mutate(case_date = param_base_floor_date + day) %>% 
  mutate(wk=epiweek(param_base_floor_date + day)) %>% 
  select(wk, case_date, day)

# plot similar to figure 2 in article
df_infected_all_seasons %>% 
left_join(df_calendar, join_by(day))  %>% 
  ggplot(aes(x=case_date, y=reports, colour=season_name)) +
  geom_line() +
  scale_x_date(
    date_breaks = "1 week",
    labels = function(x)epiweek(x),
  ) +
  labs(x="Day with week numbers shown", y="Reports", colour="Season") +
  theme_bw() +
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth  = 0.3))
