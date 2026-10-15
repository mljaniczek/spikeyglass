# Regression tests for the correctness fixes listed under "Bug fixes" in
# NEWS.md. Each test failed before its fix.

quiet_fit <- function(...) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(ssjgl(...)))
  out
}

# Copy of ssjgl() that always takes the R EM path (never the C++ EM loop).
ssjgl_R_path <- function() {
  f <- ssjgl
  b <- deparse(body(f))
  i <- grep("use_cpp_em <- ", b, fixed = TRUE)[1]
  b[i] <- "use_cpp_em <- FALSE"
  body(f) <- parse(text = paste(b, collapse = "\n"))[[1]]
  environment(f) <- asNamespace("spikeyglass")
  f
}

# --- 1. lambda2 = 0 / lambda1 = 0 in the E-step -----------------------------

test_that("gete() drops the lambda2 prior terms when lambda2 = 0", {
  p <- 4
  theta <- list(diag(p), diag(p))
  theta[[1]][1, 2] <- theta[[1]][2, 1] <- 0.3
  theta[[2]][1, 2] <- theta[[2]][2, 1] <- 0.25
  for (dbl in c(FALSE, TRUE)) for (pen in c("fused", "group")) {
    out <- gete(p, theta, lambda1 = 0.5, lambda2 = 0, v0 = 0.01, v1 = 1,
                pi_delta = 0.5, pi_xi = 0.5, penalty = pen, doubly = dbl)
    expect_false(anyNA(out$prob1))
    expect_false(anyNA(out$d1))
    # The non-zero edge is far from the spike, so it is in the slab
    expect_gt(out$prob1[1, 2], 0.5)
  }
  # Non-doubly with lambda2 = 0 equals the lambda1-only spike-and-slab
  out <- gete(p, theta, lambda1 = 0.5, lambda2 = 0, v0 = 0.01, v1 = 1,
              pi_delta = 0.5, pi_xi = 0.5, penalty = "fused", doubly = FALSE)
  abssum <- abs(theta[[1]]) + abs(theta[[2]])
  l1 <- -0.5 * abssum + 2 * log(0.5)
  l0 <- -0.5 / 0.01 * abssum + 2 * log(0.5 / 0.01)
  expect_equal(out$prob1[1, 2], 1 / (1 + exp(l0[1, 2] - l1[1, 2])))
})

test_that("gete() drops the lambda1 prior terms when lambda1 = 0", {
  p <- 4
  theta <- list(diag(p), diag(p))
  theta[[1]][1, 2] <- theta[[1]][2, 1] <- 0.3
  out <- gete(p, theta, lambda1 = 0, lambda2 = 0.5, v0 = 0.01, v1 = 1,
              pi_delta = 0.5, pi_xi = 0.5, penalty = "fused", doubly = FALSE)
  expect_false(anyNA(out$prob1))
  expect_gt(out$prob1[1, 2], 0.5)
})

test_that("ssjgl() with lambda2 = 0 gives informative PIPs (C++ and R paths)", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 8, n = 80, graph_type = "band", seed = 1)
  fR <- ssjgl_R_path()
  for (dbl in c(FALSE, TRUE)) {
    fit <- quiet_fit(sim$data_list, lambda0 = 1, lambda1 = 0.5, lambda2 = 0,
                     v0s = 0.01, doubly = dbl, impute = FALSE, maxitr.em = 50)
    pr <- fit$problist1[[1]][upper.tri(fit$problist1[[1]])]
    expect_false(anyNA(pr))
    expect_gt(max(pr), 0.5)

    fitR <- NULL
    utils::capture.output(fitR <- suppressMessages(fR(
      sim$data_list, lambda0 = 1, lambda1 = 0.5, lambda2 = 0, v0s = 0.01,
      doubly = dbl, impute = FALSE, maxitr.em = 50)))
    expect_equal(unname(fitR$problist1[[1]]), unname(fit$problist1[[1]]), tolerance = 1e-10)
  }
})

# --- 2. getmissing() with one missing cell in a group ------------------------

test_that("getmissing() handles a group with exactly one missing cell", {
  set.seed(1)
  p <- 4
  Y <- list(matrix(rnorm(40), 10, p), matrix(rnorm(40), 10, p))
  Y[[1]][3, 2] <- NA
  Y[[2]][5, 1] <- NA
  Y[[2]][5, 4] <- NA
  missed <- rbind(c(1, 3, 2), c(2, 5, 1), c(2, 5, 4))
  theta <- list(diag(p), diag(p))
  expect_no_error(out <- getmissing(Y, theta, missed))
  expect_false(anyNA(out$YY[[1]]))
  expect_false(anyNA(out$YY[[2]]))
})

test_that("ssjgl() imputes when a group has a single missing cell", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 6, n = 40, graph_type = "band", seed = 2)
  Y <- sim$data_list
  Y[[1]][4, 3] <- NA
  expect_no_error(fit <- quiet_fit(Y, lambda0 = 1, lambda1 = 0.5,
                                   lambda2 = 0.5, v0s = 0.01, maxitr.em = 20))
  expect_length(fit$imputed, 1)
  expect_false(anyNA(fit$imputed))
})

# --- 3. imputed values with normalize = FALSE --------------------------------

test_that("imputed values are returned on the original scale", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 6, n = 60, graph_type = "band", seed = 3)
  Y <- sim$data_list
  Y[[1]][1, 2] <- NA; Y[[1]][5, 6] <- NA
  Y[[2]][3, 4] <- NA; Y[[2]][7, 1] <- NA
  shift <- c(10, -5, 3, 7, 0, 2)
  Ys <- lapply(Y, function(m) sweep(m, 2, shift, "+"))
  args <- list(lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5, v0s = 0.01,
               maxitr.em = 20)

  f0 <- do.call(quiet_fit, c(list(Y), args))
  fs <- do.call(quiet_fit, c(list(Ys), args))
  fn <- do.call(quiet_fit, c(list(Ys), args, normalize = TRUE))

  expect_false(anyNA(fs$imputed))
  # Location-equivariant: shifting the data shifts the imputations
  expect_equal(fs$imputed, f0$imputed + shift[fs$missed[, 3]], tolerance = 1e-8)
  # normalize only centres, so it must not change the imputed values
  expect_equal(fn$imputed, fs$imputed, tolerance = 1e-8)
  expect_equal(fs$thetalist[[1]], f0$thetalist[[1]], tolerance = 1e-8)
})

# --- 4. M-step uses E[S | observed] = S(imputed) + addvar / n ----------------

test_that("JGL.adaptive() uses addvar in the M-step, not only in screening", {
  set.seed(4)
  p <- 4; n <- 200
  Y <- list(matrix(rnorm(n * p), n, p), matrix(rnorm(n * p), n, p))
  addvar <- list(diag(c(50, 0, 0, 0)), diag(c(0, 0, 80, 0)))
  lam <- matrix(1e-8, p, p)
  fit <- JGL.adaptive(Y, addvar = addvar, penalty = "fused", lambda0 = 1e-8,
                      lambda1 = lam, lambda2 = lam, maxiter = 5000,
                      tol = 1e-10, return.whole.theta = TRUE)
  for (k in 1:2) {
    S_aug <- cov(Y[[k]]) * (n - 1) / n + addvar[[k]] / n
    expect_equal(unname(fit$theta[[k]]), solve(S_aug), tolerance = 1e-3)
  }
})

# --- 5. warm / warm.connected removed ----------------------------------------

test_that("ssjgl() no longer accepts the unused warm arguments", {
  expect_false(any(c("warm", "warm.connected") %in% names(formals(ssjgl))))
})

# --- 6. log-sum-exp in the non-doubly R gete() -------------------------------

test_that("gete() is numerically stable when both log-densities underflow", {
  p <- 3
  theta <- list(diag(p), diag(p))
  theta[[1]][1, 2] <- theta[[1]][2, 1] <- 400
  theta[[2]][1, 2] <- theta[[2]][2, 1] <- 400
  out <- gete(p, theta, lambda1 = 1, lambda2 = 1, v0 = 1e-3, v1 = 1,
              pi_delta = 0.5, pi_xi = 0.5, penalty = "fused", doubly = FALSE)
  expect_false(anyNA(out$prob1))
  expect_equal(out$prob1[1, 2], 1)
  expect_equal(out$d1[1, 2], 1)
})

# --- 7. C++ EM path and R EM path agree --------------------------------------

test_that("C++ and R EM paths agree for fused/group x doubly, K = 2", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 8, n = 80, graph_type = "band", seed = 1)
  fR <- ssjgl_R_path()
  for (pen in c("fused", "group")) for (dbl in c(FALSE, TRUE)) {
    args <- list(Y = sim$data_list, penalty = pen, lambda0 = 1,
                 lambda1 = 0.5, lambda2 = 0.5, v0s = c(0.1, 0.03, 0.01),
                 doubly = dbl, b = 8, impute = FALSE)
    a <- do.call(quiet_fit, args)
    r <- NULL
    utils::capture.output(r <- suppressMessages(do.call(fR, args)))
    label <- paste(pen, "doubly =", dbl)
    for (i in seq_along(args$v0s)) {
      for (k in 1:2) {
        expect_equal(unname(r$thetalist[[i]][[k]]), unname(a$thetalist[[i]][[k]]),
                     tolerance = 1e-10, label = label)
      }
      expect_equal(unname(r$problist1[[i]]), unname(a$problist1[[i]]), tolerance = 1e-10, label = label)
      expect_equal(unname(r$penlist1[[i]]), unname(a$penlist1[[i]]), tolerance = 1e-10, label = label)
      if (dbl) expect_equal(unname(r$problist2[[i]]), unname(a$problist2[[i]]), tolerance = 1e-10, label = label)
      expect_equal(r$pi1list[[i]], a$pi1list[[i]], tolerance = 1e-10, label = label)
      expect_equal(r$pi2list[[i]], a$pi2list[[i]], tolerance = 1e-10, label = label)
    }
    expect_equal(r$itrlist, a$itrlist, label = label)
  }
})
