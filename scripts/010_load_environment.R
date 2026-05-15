# ____________________________
#
## - Load libraries ----
# ____________________________

library(tidyverse)
library(lubridate)
library(tictoc)
library(pomp)
library(doFuture)
library(foreach)



# ____________________________
#
## - configuration: parameters etc ----
# ____________________________

# Define cfg defaults only once (so re-sourcing doesn't wipe overrides)
# Some are more meta-parameters for various runs
if (!exists("cfg", inherits = FALSE)) {
  cfg <- list(
    run = list(base_season = "2024/2025", 
               base_floor_date = as.Date("2024-09-07"),
               lambda_impute = 1.26,
               given_seq = c(1, 43, 63, 120, 175),
               seed = 31425),
    paths = list(data_dir = "data", 
                 out_dir = "outputs"),
    metaparams = list(national_population = 5e+06,
                      lower_bound = 0.01,
                      upper_bound = 4,
                      num_slices = 11,
                      tolerance = 1e-03,
                      max_iterations = 200,
                      ts_duration = 175,
                      init_candidates = 100,
                      num_selected = 20,
                      pert_expansions = 10,
                      pert_range = 4,
                      deletion_prob = 0.2,
                      num_resampled = 100,
                      num_total_iterations = 50)
  )
}


## pomp model based parameters

# all the parameters as per paper, includes initial beta values
THETA <- c(
  epsilon=0.5, 
  delta=1,
  h=0.06,
  gamma0=0.25,
  gamma1=0.1,
  rf=0.425,
  k=1000,
  N=cfg$metaparams$national_population,
  beta1=0.4830,
  beta2=0.21288,
  beta3=0.2827,
  beta4=0.1077,
  beta5=0,
  epoch_test=0) # default should be 0

# R function to initialise states (used in pomp); note initial I cases
seir_rinit <- function(N=cfg$metaparams$national_population, H0=0, H1=0, I=1, C=0,...){
  c(S = N - H1 - I - H0,
    E = 0,
    I = I,
    H0 = H0,
    H1 = H1,
    R = 0,
    C = C)
}
