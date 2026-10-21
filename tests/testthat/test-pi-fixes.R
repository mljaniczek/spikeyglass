# pi handling in the EM: floor, convergence on pi, collapse warning, carry_pi.
# Background: data-raw/pip-collapse-diagnostic.md.

collapse_data <- function() {
  sim <- simulate_ssjgl_data(K = 2, p = 30, n = 100, graph_type = "scale-free", seed = 3)
  lapply(sim$data_list, function(m) m * 1.3)       # raw, unstandardised scale
}
dina_ladder <- 1 / (1 + 5 * (1:3))                 # lambda1/v0 = 6, 11, 16

ssjgl_R_path <- function() {
  f <- ssjgl
  b <- deparse(body(f))
  i <- grep("use_cpp_em <- ", b, fixed = TRUE)[1]
  b[i] <- "use_cpp_em <- FALSE"
  body(f) <- parse(text = paste(b, collapse = "\n"))[[1]]
  environment(f) <- asNamespace("spikeyglass")
  f
}

test_that("a collapsing ladder warns, and pi stays inside its bounds", {
  skip_on_cran()
  Y <- collapse_data()
  expect_warning(
    fit <- ssjgl(Y, lambda0 = 1, lambda1 = 1, lambda2 = 1, v0s = dina_ladder,
                 doubly = TRUE, impute = FALSE),
    "PIPs may have collapsed")
  floor <- 1 / (30 * 29 / 2)
  pis <- c(unlist(fit$pi1list), unlist(fit$pi2list))
  expect_true(all(pis >= floor - 1e-15 & pis <= 1 - floor + 1e-15))
  # pi_xi used to reach exactly 0 (log(0) = -Inf in the E-step)
  expect_true(all(unlist(fit$pi2list) > 0))
  expect_false(anyNA(unlist(fit$problist2)))
})

test_that("the default ladder does not collapse on the same data", {
  skip_on_cran()
  Y <- collapse_data()
  expect_no_warning(
    fit <- ssjgl(Y, lambda0 = 1, lambda1 = 1, lambda2 = 1, doubly = TRUE,
                 impute = FALSE))
  pr <- fit$problist1[[3]]
  expect_gt(sum(pr[upper.tri(pr)] > 0.5), 10)
})

test_that("convergence also requires pi to settle (tol.pi)", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 15, n = 80, seed = 4)
  args <- list(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
               v0s = 0.05, doubly = TRUE, impute = FALSE, standardize = TRUE)
  loose <- suppressWarnings(do.call(ssjgl, c(args, tol.pi = Inf)))  # theta only
  strict <- suppressWarnings(do.call(ssjgl, c(args, tol.pi = 1e-6)))
  expect_gte(strict$itrlist[1], loose$itrlist[1])
  expect_equal(strict$args$tol.pi, 1e-6)
})

test_that("C++ and R EM paths agree with the pi fixes, incl. carry_pi", {
  skip_on_cran()
  Y <- collapse_data()
  fR <- ssjgl_R_path()
  for (carry in c(FALSE, TRUE)) for (pen in c("fused", "group")) {
    args <- list(Y, penalty = pen, lambda0 = 1, lambda1 = 1, lambda2 = 1,
                 v0s = c(0.1, 0.03, 0.01), doubly = TRUE, impute = FALSE,
                 carry_pi = carry)
    a <- suppressWarnings(do.call(ssjgl, args))
    r <- suppressWarnings(do.call(fR, args))
    expect_equal(r$itrlist, a$itrlist)
    expect_equal(unlist(r$pi1list), unlist(a$pi1list), tolerance = 1e-10)
    expect_equal(unlist(r$pi2list), unlist(a$pi2list), tolerance = 1e-10)
    # On this harder (raw-scale) problem the two paths already differed by
    # ~3e-10 in theta before the pi fixes (floating-point accumulation in the
    # ADMM); the 1e-10 agreement test on easier data is in test-bugfixes.R.
    for (k in 1:2) expect_equal(unname(r$thetalist[[3]][[k]]),
                                unname(a$thetalist[[3]][[k]]), tolerance = 1e-8)
  }
})

test_that("carry_pi = TRUE changes the later ladder steps", {
  skip_on_cran()
  Y <- collapse_data()
  args <- list(Y, lambda0 = 1, lambda1 = 1, lambda2 = 1, doubly = TRUE,
               impute = FALSE)
  f0 <- suppressWarnings(do.call(ssjgl, args))
  f1 <- suppressWarnings(do.call(ssjgl, c(args, carry_pi = TRUE)))
  expect_equal(f1$thetalist[[1]], f0$thetalist[[1]])     # first step identical
  expect_false(identical(f1$itrlist[-1], f0$itrlist[-1]) &&
                 isTRUE(all.equal(f1$pi1list[-1], f0$pi1list[-1])))
})
