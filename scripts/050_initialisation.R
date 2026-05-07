## Initialises the model for the first time
## assuming env, models, funs, data already loaded

# filter data for the base season
df_infected <- 
  df_infected_all_seasons %>% 
  filter(season_name == cfg$run$base_season) %>% 
  select(day, reports)


# Get covar based on a sequence (initially), from a given sequence
df_covar <- fun_get_covar(given_seq=cfg$run$given_seq)


######
# deterministic approach with multiple betas
######

fluASEIR <- model_Flu_deter_multi(
  data = df_infected,
  df_covar = df_covar
)

# load script will have initialised THETA and seir_rinit
# update in the pomp and the params
fluASEIR <- 
  fluASEIR %>%
  pomp(rinit= seir_rinit,
       statenames=c("S","E","I","H0","H1","R","C"),
       params = THETA)
