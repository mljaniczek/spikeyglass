# jgl() reproduces JGL::JGL() (JGL 2.3.2), using saved outputs
# (data-raw/make-jgl-reference.R), so JGL itself is not needed.

test_that("jgl() reproduces JGL::JGL() on saved reference fits", {
  cases <- readRDS(test_path("fixtures", "jgl-reference.rds"))
  expect_gte(sum(grepl("unconnected", names(cases))), 10)
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    fit <- do.call(jgl, cs$args)
    ref <- cs$fit
    expect_s3_class(fit, "jgl")
    expect_identical(names(unclass(fit)), names(ref), label = nm)
    expect_identical(fit$connected, ref$connected, label = nm)
    expect_equal(fit$theta, ref$theta, tolerance = 1e-10, label = nm)
    expect_equal(fit$theta.unconnected, ref$theta.unconnected,
                 tolerance = 1e-10, label = nm)
  }
})

test_that("jgl() validates its inputs", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, seed = 1)
  Y <- sim$data_list
  expect_error(jgl(Y, lambda1 = -1, lambda2 = 0.1), "lambda1")
  expect_error(jgl(Y, lambda1 = 0.1, lambda2 = matrix(0, 3, 3)), "lambda2")
  expect_error(jgl(Y, lambda1 = 0.1, lambda2 = 0.1, screening = "memory.efficient"),
               "fast")
  expect_error(jgl(Y, lambda1 = 0.1, lambda2 = 0.1, weights = c(1, 2, 3)), "weights")
  expect_error(jgl(list(Y[[1]], Y[[2]][, 1:4]), lambda1 = 0.1, lambda2 = 0.1),
               "same number of columns")
})

test_that("jgl() keeps variable names", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 40, seed = 2)
  Y <- lapply(sim$data_list, function(m) { colnames(m) <- letters[1:5]; m })
  fit <- jgl(Y, lambda1 = 0.1, lambda2 = 0.05, return.whole.theta = TRUE)
  expect_identical(dimnames(fit$theta[[2]]), list(letters[1:5], letters[1:5]))
})

test_that("ggmtools::pcor_estimate() works on jgl() fits", {
  sim <- simulate_ssjgl_data(K = 2, p = 6, n = 60, seed = 1)
  Y <- lapply(sim$data_list, function(m) { colnames(m) <- letters[1:6]; m })
  fit <- jgl(Y, lambda1 = 0.1, lambda2 = 0.05, return.whole.theta = TRUE)
  P <- pcor_estimate(fit)
  expect_named(P, c("Group_1", "Group_2"))
  expect_equal(P$Group_2, precision_to_pcor(fit$theta[[2]]))
  expect_identical(rownames(P$Group_1), letters[1:6])
})
