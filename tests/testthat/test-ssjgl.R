test_that("ssjgl runs without error on small example", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, graph_type = "band", seed = 1)

  expect_no_error({
    fit <- suppressMessages(ssjgl(
      Y = sim$data_list,
      penalty = "fused",
      lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
      v0s = c(0.01, 0.005),
      doubly = FALSE,
      a = 1, b = 5,
      maxitr.em = 10, tol.em = 1e-3,
      maxitr.jgl = 50, tol.jgl = 1e-4,
      normalize = TRUE,
      impute = FALSE
    ))
  })
})

test_that("ssjgl returns correct structure", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, graph_type = "band", seed = 1)
  fit <- suppressMessages(ssjgl(
    Y = sim$data_list,
    penalty = "fused",
    lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
    v0s = c(0.01, 0.005),
    doubly = FALSE,
    a = 1, b = 5,
    maxitr.em = 10, tol.em = 1e-3,
    maxitr.jgl = 50, tol.jgl = 1e-4,
    normalize = TRUE,
    impute = FALSE
  ))

  expect_s3_class(fit, "ssjgl")
  expect_length(fit$thetalist, 2)  # 2 v0 steps
  expect_length(fit$thetalist[[1]], 2)  # K=2 groups
  expect_equal(dim(fit$thetalist[[1]][[1]]), c(5, 5))  # p=5
  expect_length(fit$pi1list, 2)
  expect_length(fit$itrlist, 2)
  expect_length(fit$timelist, 2)
  expect_true(all(fit$timelist > 0))

  # problist1 entries should be in [0, 1]
  prob <- fit$problist1[[1]]
  expect_true(all(prob >= 0 & prob <= 1))
  expect_true(all(diag(prob) == 0))
})

test_that("ssjgl works with group penalty", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, graph_type = "band", seed = 2)

  fit <- suppressMessages(ssjgl(
    Y = sim$data_list,
    penalty = "group",
    lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
    v0s = c(0.01),
    doubly = FALSE,
    maxitr.em = 10, tol.em = 1e-3,
    maxitr.jgl = 50, tol.jgl = 1e-4,
    impute = FALSE
  ))

  expect_s3_class(fit, "ssjgl")
  expect_length(fit$thetalist, 1)
})

test_that("ssjgl works with doubly spike-and-slab", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, graph_type = "band", seed = 3)

  fit <- suppressMessages(ssjgl(
    Y = sim$data_list,
    penalty = "fused",
    lambda0 = 0.1, lambda1 = 0.5, lambda2 = 0.5,
    v0s = c(0.01),
    doubly = TRUE,
    maxitr.em = 10, tol.em = 1e-3,
    maxitr.jgl = 50, tol.jgl = 1e-4,
    impute = FALSE
  ))

  expect_s3_class(fit, "ssjgl")
  # doubly should produce non-null problist2
  expect_false(is.null(fit$problist2[[1]]))
})
