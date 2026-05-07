# Define cfg defaults only once (so re-sourcing doesn't wipe overrides)
if (!exists("cfg", inherits = FALSE)) {
  cfg <- list(
    run = list(year = "2024/2025", seed = 31425),
    paths = list(data_dir = "data", out_dir = "outputs"),
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
