test_that("extract functions work on ssjgl fit", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, graph_type = "band", seed = 1)
  fit <- suppressMessages(ssjgl(
    Y = sim$data_list,
    penalty = "fused",
    lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
    v0s = c(0.01),
    maxitr.em = 10, tol.em = 1e-3,
    maxitr.jgl = 50, tol.jgl = 1e-4,
    impute = FALSE
  ))

  # extract_precision
  theta <- extract_precision(fit)
  expect_type(theta, "list")
  expect_length(theta, 2)
  expect_equal(dim(theta[[1]]), c(5, 5))

  # extract_adjacency
  adj <- extract_adjacency(fit, threshold = 0.5)
  expect_length(adj, 2)
  expect_true(all(adj[[1]] %in% c(0L, 1L)))
  expect_true(all(diag(adj[[1]]) == 0))

  # extract_pcor
  pcor <- extract_pcor(fit)
  expect_length(pcor, 2)
  expect_equal(dim(pcor[[1]]), c(5, 5))
  # Diagonal should be 1
  expect_equal(unname(diag(pcor[[1]])), rep(1, 5))
  # Partial correlations should be in [-1, 1]
  expect_true(all(pcor[[1]] >= -1 & pcor[[1]] <= 1))

  # extract_probabilities
  probs <- extract_probabilities(fit)
  expect_type(probs, "list")
  expect_true(!is.null(probs$prob1))
})

test_that("S3 methods work on ssjgl fit", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, graph_type = "band", seed = 1)
  fit <- suppressMessages(ssjgl(
    Y = sim$data_list,
    penalty = "fused",
    lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
    v0s = c(0.01),
    maxitr.em = 10, tol.em = 1e-3,
    maxitr.jgl = 50, tol.jgl = 1e-4,
    impute = FALSE
  ))

  # print
  expect_output(print(fit), "Spike-and-Slab Joint Graphical Lasso")

  # summary
  s <- summary(fit)
  expect_s3_class(s, "summary.ssjgl")
  expect_equal(s$K, 2)
  expect_equal(s$p, 5)
  expect_output(print(s), "SSJGL Summary")

  # coef
  theta <- coef(fit)
  expect_type(theta, "list")
  expect_length(theta, 2)

  # fitted
  pcor <- fitted(fit)
  expect_type(pcor, "list")
  expect_length(pcor, 2)
  expect_equal(unname(diag(pcor[[1]])), rep(1, 5))
})

test_that("precision_to_pcor is correct on known input", {
  # 2x2 precision matrix: Theta = [[2, -0.5], [-0.5, 1]]
  Theta <- matrix(c(2, -0.5, -0.5, 1), 2, 2)
  pcor <- precision_to_pcor(Theta)

  # pcor[1,2] = -(-0.5) / sqrt(2*1) = 0.5/sqrt(2)
  expected_12 <- 0.5 / sqrt(2)
  expect_equal(pcor[1, 2], expected_12, tolerance = 1e-10)
  expect_equal(pcor[2, 1], expected_12, tolerance = 1e-10)
  expect_equal(diag(pcor), c(1, 1))
})

test_that("negloglik_Gaussian returns finite value for PD input", {
  S <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  Theta <- matrix(c(1.1, -0.3, -0.3, 1.1), 2, 2)
  nll <- negloglik_Gaussian(S, Theta)
  expect_true(is.finite(nll))
})

test_that("negloglik_Gaussian returns Inf for non-PD input", {
  S <- diag(2)
  Theta <- matrix(c(1, 2, 2, 1), 2, 2)  # not PD
  nll <- negloglik_Gaussian(S, Theta)
  expect_equal(nll, Inf)
})

test_that("make_v0_ladder produces correct output", {
  v0s <- make_v0_ladder(lambda1 = 1, n_steps = 10)

  expect_length(v0s, 10)
  # Should be decreasing by default
  expect_true(all(diff(v0s) < 0))
  # All positive
  expect_true(all(v0s > 0))
  # First value: v0 = lambda1/min_ratio = 1/5 = 0.2
  expect_equal(v0s[1], 1/5)
  # Last value: v0 = lambda1/max_ratio = 1/500 = 0.002
  expect_equal(v0s[10], 1/500)
})

test_that("make_v0_ladder scales with lambda1", {
  v0s_small <- make_v0_ladder(lambda1 = 0.01, n_steps = 5)
  v0s_large <- make_v0_ladder(lambda1 = 1, n_steps = 5)

  # Larger lambda1 should produce larger or equal v0 values
  expect_true(all(v0s_large >= v0s_small))

  # The smallest v0 should scale roughly with lambda1
  expect_true(min(v0s_large) > min(v0s_small))
})

test_that("make_v0_ladder start_sparse argument works", {
  v0s_dec <- make_v0_ladder(lambda1 = 1, n_steps = 5, start_sparse = TRUE)
  v0s_inc <- make_v0_ladder(lambda1 = 1, n_steps = 5, start_sparse = FALSE)

  expect_true(all(diff(v0s_dec) < 0))  # decreasing
  expect_true(all(diff(v0s_inc) > 0))  # increasing
  # Same values, just reversed
  expect_equal(sort(v0s_dec), sort(v0s_inc))
})

test_that("make_v0_ladder validates inputs", {
  expect_error(make_v0_ladder(lambda1 = -1))
  expect_error(make_v0_ladder(lambda1 = 1, n_steps = 1))
  expect_error(make_v0_ladder(lambda1 = 1, min_ratio = 0))
  expect_error(make_v0_ladder(lambda1 = 1, max_ratio = 3, min_ratio = 5))
})

test_that("plot_stability runs without error", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, graph_type = "band", seed = 1)
  v0s <- make_v0_ladder(lambda1 = 0.5, n_steps = 3)
  fit <- suppressMessages(ssjgl(
    Y = sim$data_list,
    penalty = "fused",
    lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
    v0s = v0s,
    maxitr.em = 10, tol.em = 1e-3,
    maxitr.jgl = 50, tol.jgl = 1e-4,
    impute = FALSE
  ))

  result <- plot_stability(fit, v0s)

  expect_type(result, "list")
  expect_equal(nrow(result$edge_counts), 3)
  expect_equal(ncol(result$edge_counts), 2)
  expect_length(result$mean_prob, 3)
  expect_length(result$edge_changes, 2)
  expect_true(all(result$mean_prob >= 0 & result$mean_prob <= 1))
})
