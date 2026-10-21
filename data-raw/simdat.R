# Builds data/simdat.rda: a simulated 3-group dataset with known graphs.
#
# K = 3 groups, p = 50 variables, n = 150 per group, scale-free graphs from
# simulate_ssjgl_data(). (This replaces an earlier version generated with
# BDgraph internals, whose script was not runnable.)
#
# Run from the package root:  Rscript data-raw/simdat.R

devtools::load_all(quiet = TRUE)

sim <- simulate_ssjgl_data(K = 3, p = 50, n = 150, graph_type = "scale-free",
                           seed = 2022)
simdat <- list(Y = sim$data_list, g = sim$adj_list, Omega = sim$Omega_list)

usethis::use_data(simdat, overwrite = TRUE, version = 2, compress = "xz")
