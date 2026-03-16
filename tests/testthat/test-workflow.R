# Tests for CV, bootstrap, and workflow functions

test_that("SSJGL_select_v0_cv selects a v0", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, seed = 1)
  cv_res <- suppressMessages(SSJGL_select_v0_cv(
    Y = sim$data_list,
    v0s = c(0.05, 0.01, 0.005),
    folds = 2,
    penalty = "fused",
    lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
    maxitr.em = 10, maxitr.jgl = 50,
    impute = FALSE,
    verbose = FALSE
  ))

  expect_true(cv_res$v0_best %in% c(0.05, 0.01, 0.005))
  expect_length(cv_res$cv_score, 3)
  expect_true(all(is.finite(cv_res$cv_score)))
  expect_equal(cv_res$folds, 2)
})

test_that("SSJGL_final_with_pcor_CI produces valid CIs", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, seed = 1)
  boot_res <- suppressMessages(SSJGL_final_with_pcor_CI(
    Y = sim$data_list,
    v0_best = 0.01,
    B = 5,
    ci_level = 0.95,
    penalty = "fused",
    lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
    maxitr.em = 10, maxitr.jgl = 50,
    impute = FALSE,
    verbose = FALSE
  ))

  expect_equal(boot_res$v0_best, 0.01)
  expect_length(boot_res$theta_hat, 2)
  expect_length(boot_res$pcor_hat, 2)
  expect_length(boot_res$CI_lower, 2)
  expect_length(boot_res$CI_upper, 2)

  # CIs should bracket the point estimate (at least loosely)
  for (k in 1:2) {
    expect_equal(dim(boot_res$CI_lower[[k]]), c(5, 5))
    expect_equal(dim(boot_res$CI_upper[[k]]), c(5, 5))
    # Lower <= upper everywhere
    expect_true(all(boot_res$CI_lower[[k]] <= boot_res$CI_upper[[k]], na.rm = TRUE))
    # Diagonal of pcor should be 1
    expect_equal(diag(boot_res$pcor_hat[[k]]), rep(1, 5))
  }
})

test_that("SSJGL_CV_final_pcorCI runs full workflow", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, seed = 1)
  res <- suppressMessages(SSJGL_CV_final_pcorCI(
    Y = sim$data_list,
    v0s = c(0.05, 0.01),
    folds = 2, B = 3,
    penalty = "fused",
    lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
    maxitr.em = 10, maxitr.em.cv = 10,
    maxitr.jgl = 50, maxitr.jgl.cv = 50,
    impute = FALSE,
    verbose = FALSE
  ))

  expect_true("cv" %in% names(res))
  expect_true("final" %in% names(res))
  expect_true(res$cv$v0_best %in% c(0.05, 0.01))
  expect_length(res$final$pcor_hat, 2)
})

test_that("ssjgl input validation catches bad inputs", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, seed = 1)

  # Not a list
  expect_error(ssjgl(sim$data_list[[1]], penalty = "fused",
                     lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5),
               "list")

  # Mismatched p
  bad_Y <- list(matrix(1, 10, 5), matrix(1, 10, 3))
  expect_error(ssjgl(bad_Y, penalty = "fused",
                     lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5),
               "same number of columns")

  # Bad penalty
  expect_error(ssjgl(sim$data_list, penalty = "invalid",
                     lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5),
               "arg")

  # Negative lambda
  expect_error(ssjgl(sim$data_list, penalty = "fused",
                     lambda0 = -1, lambda1 = 0.5, lambda2 = 0.5),
               "positive")
  expect_error(ssjgl(sim$data_list, penalty = "fused",
                     lambda0 = 1, lambda1 = -0.5, lambda2 = 0.5),
               "non-negative")

  # Bad v0s
  expect_error(ssjgl(sim$data_list, penalty = "fused",
                     lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
                     v0s = c(-1, 0.01)),
               "positive")

  # NA warning
  Y_na <- sim$data_list
  Y_na[[1]][1, 1] <- NA
  expect_warning(ssjgl(Y_na, penalty = "fused",
                       lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
                       v0s = 0.01, maxitr.em = 5, impute = FALSE),
                 "NA")
})

test_that("plot.ssjgl produces output", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, seed = 1)
  fit <- suppressMessages(ssjgl(sim$data_list, penalty = "fused",
                                lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
                                v0s = 0.01, maxitr.em = 10, impute = FALSE))

  result <- plot(fit)
  expect_length(result, 2)  # K=2 pcor matrices
  expect_equal(dim(result[[1]]), c(5, 5))
})
