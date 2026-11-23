# Generates tests/testthat/fixtures/jgl-reference.rds: inputs and
# JGL::JGL() (JGL 2.3.2) outputs, used to check that spikeyglass::jgl()
# reproduces the archived JGL package.
#
# Run from the package root with JGL 2.3.2 installed:
#   Rscript data-raw/make-jgl-reference.R
devtools::load_all(quiet = TRUE)
library(JGL)

cases <- list()
add <- function(name, K, penalty, lambda1, lambda2, ..., noise = FALSE) {
  seed <- 100 + length(cases)
  if (noise) {
    # 8 band-graph variables plus 4 independent ones, so that a large enough
    # lambda1 leaves some variables unconnected (diagonal-only problem)
    sim <- simulate_ssjgl_data(K = K, p = 8, n = 60, graph_type = "band", seed = seed)
    set.seed(seed)
    Y <- lapply(sim$data_list, function(m) cbind(m, matrix(rnorm(60 * 4), 60, 4)))
  } else {
    sim <- simulate_ssjgl_data(K = K, p = 12, n = 60, graph_type = "band", seed = seed)
    Y <- sim$data_list
  }
  args <- list(Y = Y, penalty = penalty, lambda1 = lambda1, lambda2 = lambda2, ...)
  fit <- do.call(JGL::JGL, args)
  cases[[name]] <<- list(args = args, fit = unclass(fit))
}
for (K in 2:3) for (pen in c("fused", "group")) for (pd in c(FALSE, TRUE)) {
  for (whole in c(TRUE, FALSE)) {
    add(sprintf("K%d_%s_pd%s_whole%s", K, pen, pd, whole), K, pen, 0.15, 0.05,
        penalize.diagonal = pd, return.whole.theta = whole)
  }
}
# sample-size weights, a larger penalty (more unconnected variables),
# matrix-valued penalties, and DiNA's call (defaults + whole theta)
add("K2_fused_sample.size", 2, "fused", 0.1, 0.05, weights = "sample.size",
    return.whole.theta = TRUE)
add("K2_group_sparse", 2, "group", 0.3, 0.1, return.whole.theta = FALSE)
L1 <- matrix(0.12, 12, 12); L1[1:4, 1:4] <- 0.05
L2 <- matrix(0.04, 12, 12); L2[5:8, 5:8] <- 0.2
add("K2_fused_matrix_lambda", 2, "fused", L1, L2, return.whole.theta = TRUE)
add("K3_fused_matrix_lambda_pd", 3, "fused", L1, L2, penalize.diagonal = TRUE,
    return.whole.theta = TRUE)
add("dina_default", 2, "fused", 0.08, 0.02, return.whole.theta = TRUE)
# cases with unconnected variables
for (K in 2:3) for (pen in c("fused", "group")) for (pd in c(FALSE, TRUE)) {
  for (whole in c(TRUE, FALSE)) {
    add(sprintf("unconnected_K%d_%s_pd%s_whole%s", K, pen, pd, whole), K, pen,
        if (pen == "fused") 0.4 else 0.35, 0.05, penalize.diagonal = pd,
        return.whole.theta = whole, noise = TRUE)
  }
}
add("unconnected_K2_fused_sample.size", 2, "fused", 0.2, 0.05,
    weights = "sample.size", return.whole.theta = FALSE, noise = TRUE)

saveRDS(cases, "tests/testthat/fixtures/jgl-reference.rds", version = 2)
message("saved ", length(cases), " cases")
