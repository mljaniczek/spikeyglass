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
