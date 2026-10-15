# Generates jgl-flsa-general.rds: inputs and JGL:::flsa.general (JGL 2.3.2)
# outputs (saved to tests/testthat/fixtures/), used to test the C++ port
# flsa_general_cpp(). Values are rounded
# so that exact and near (< 1e-4) ties across groups occur often.
#
# Run from the package root with JGL 2.3.2 installed:
#   Rscript data-raw/make-jgl-flsa-general.R
set.seed(2026)
cases <- list()
for (K in 3:5) for (rep in 1:4) {
  p <- 6
  A <- replicate(K, {
    m <- matrix(round(rnorm(p * p), 1), p, p)
    m[sample(length(m), 5)] <- m[sample(length(m), 5)] + 5e-5
    m
  }, simplify = FALSE)
  rho <- c(0.5, 1, 2, 1)[rep]
  lam1 <- matrix(runif(p * p, 0, 0.5), p, p)
  lam2 <- matrix(runif(p * p, 0, 0.5), p, p)
  Z <- JGL:::flsa.general(A, rho, lam1, lam2, penalize.diagonal = TRUE)
  cases[[length(cases) + 1]] <- list(A = A, rho = rho, lam1 = lam1, lam2 = lam2, Z = Z)
}
saveRDS(cases, "tests/testthat/fixtures/jgl-flsa-general.rds", version = 2)
