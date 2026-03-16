# Profiling script for spikeyglass
#
# Usage:
#   Rscript inst/profiling/profile_ssjgl.R
#
# Or interactively with profvis:
#   source("inst/profiling/profile_ssjgl.R")

library(spikeyglass)

cat("=== Profiling SSJGL ===\n\n")

# --- Setup ---
cat("Generating test data...\n")
sim <- simulate_ssjgl_data(K = 2, p = 30, n = 100, graph_type = "band", seed = 42)
cat(sprintf("  K=%d, p=%d, n=%d\n", sim$K, sim$p, sim$n[1]))

v0s <- seq(0.01, 0.001, length.out = 5)

# --- Baseline timing ---
cat("\nRunning baseline fit (fused, 5 v0 steps)...\n")
t0 <- system.time({
  fit <- ssjgl(
    Y = sim$data_list,
    penalty = "fused",
    lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
    v0s = v0s,
    doubly = TRUE,
    a = 1, b = 30,
    maxitr.em = 100, tol.em = 1e-4,
    maxitr.jgl = 100, tol.jgl = 1e-5,
    normalize = TRUE,
    impute = FALSE
  )
})

cat(sprintf("\nTotal time: %.1f seconds\n", t0["elapsed"]))
cat(sprintf("Per v0 step: %.1f seconds\n", t0["elapsed"] / length(v0s)))
cat(sprintf("EM iterations: %s\n", paste(fit$itrlist, collapse = ", ")))
cat(sprintf("Per-step times: %s\n",
            paste(round(fit$timelist, 2), collapse = ", ")))

# --- Rprof profiling ---
cat("\n--- Running Rprof ---\n")
prof_file <- tempfile("ssjgl_profile_", fileext = ".out")
Rprof(prof_file, interval = 0.02)

fit2 <- ssjgl(
  Y = sim$data_list,
  penalty = "fused",
  lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
  v0s = v0s,
  doubly = TRUE,
  a = 1, b = 30,
  maxitr.em = 100, tol.em = 1e-4,
  maxitr.jgl = 100, tol.jgl = 1e-5,
  normalize = TRUE,
  impute = FALSE
)

Rprof(NULL)

cat("\nTop functions by self time:\n")
prof_summary <- summaryRprof(prof_file)
print(head(prof_summary$by.self, 20))

cat("\nTop functions by total time:\n")
print(head(prof_summary$by.total, 20))

# --- Component timing ---
cat("\n--- Component-level timing ---\n")

# E-step timing
p <- sim$p
theta_test <- fit$thetalist[[length(fit$thetalist)]]
t_estep <- system.time({
  for (i in 1:50) {
    gete(p, theta_test, lambda1 = 0.5, lambda2 = 0.5,
         v0 = 0.005, v1 = 1, pi_delta = 0.3, pi_xi = 0.3,
         penalty = "fused", doubly = TRUE)
  }
})
cat(sprintf("E-step (50 calls): %.3f s (%.4f s each)\n",
            t_estep["elapsed"], t_estep["elapsed"] / 50))

cat("\n=== Profiling complete ===\n")
cat(sprintf("Profile data saved to: %s\n", prof_file))
cat("To view interactively: profvis::profvis(prof = '<path>')\n")
