# Generates reference-fits.rds: reference ssjgl() outputs used by
# test-reference-fits.R to check that numerical results do not change.
#
# First generated at commit 0e9694e, before JGL was vendored (with the R ADMM
# fallback calling JGL:::flsa.general and igraph for block detection); the
# vendored code reproduced those fits to 1e-10 (commit 6ce48bb).
# Regenerated after the pi fixes (pi floor and convergence on pi), which
# change the iteration counts of some first ladder steps; the final-step
# estimates were unchanged to 1e-14.
#
# Run from the package root:  Rscript tests/testthat/fixtures/make-reference-fits.R

reference_cases <- function() {
  cases <- list()
  for (K in c(2, 3)) for (pen in c("fused", "group")) for (dbl in c(FALSE, TRUE)) {
    cases[[sprintf("K%d_%s_%s", K, pen, if (dbl) "doubly" else "single")]] <-
      list(K = K, penalty = pen, doubly = dbl, impute = FALSE)
  }
  cases[["K2_fused_doubly_impute"]] <- list(K = 2, penalty = "fused", doubly = TRUE, impute = TRUE)
  cases[["K3_fused_single_impute"]] <- list(K = 3, penalty = "fused", doubly = FALSE, impute = TRUE)
  cases
}

reference_data <- function(case) {
  sim <- simulate_ssjgl_data(K = case$K, p = 10, n = 80, graph_type = "band", seed = 11)
  Y <- sim$data_list
  if (case$impute) {
    Y[[1]][2, 3] <- NA; Y[[1]][7, c(1, 9)] <- NA
    Y[[2]][4, 5] <- NA
    if (case$K > 2) Y[[3]][10, 2] <- NA
  }
  Y
}

fit_reference_case <- function(case) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(ssjgl(
    reference_data(case), penalty = case$penalty,
    lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5, v0s = c(0.1, 0.03, 0.01),
    doubly = case$doubly, b = 10, impute = case$impute)))
  out[c("thetalist", "pi1list", "pi2list", "itrlist", "problist1", "penlist1",
        "problist2", "penlist2", "imputed", "missed")]
}

if (sys.nframe() == 0L) {
  devtools::load_all(quiet = TRUE)
  fits <- lapply(reference_cases(), fit_reference_case)
  saveRDS(fits, "tests/testthat/fixtures/reference-fits.rds", version = 2)
}
