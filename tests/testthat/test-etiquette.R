# CRAN etiquette: quiet by default, no global RNG side effects, par restored.

test_that("ssjgl() is silent by default and reports progress when verbose", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 30, seed = 1)
  expect_silent(ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
                      v0s = c(0.1, 0.05), maxitr.em = 10, impute = FALSE))
  expect_message(ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
                       v0s = 0.05, maxitr.em = 10, impute = FALSE, verbose = TRUE),
                 "Ladder step 1")
  Y <- sim$data_list; Y[[1]][2, 3] <- NA          # R EM path
  expect_silent(ssjgl(Y, lambda0 = 1, lambda1 = 1, lambda2 = 1, v0s = 0.05,
                      maxitr.em = 5))
  expect_message(ssjgl(Y, lambda0 = 1, lambda1 = 1, lambda2 = 1, v0s = 0.05,
                       maxitr.em = 5, verbose = TRUE), "Itr")
})

test_that("seeded functions leave the caller's RNG stream unchanged", {
  set.seed(99); ref <- runif(3)
  set.seed(99)
  a <- simulate_ssjgl_data(K = 2, p = 5, n = 10, seed = 1)
  expect_equal(runif(3), ref)
  b <- simulate_ssjgl_data(K = 2, p = 5, n = 10, seed = 1)
  expect_identical(a$data_list, b$data_list)

  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 40, seed = 2)
  set.seed(99)
  cv <- SSJGL_select_v0_cv(sim$data_list, v0s = c(0.1, 0.05), folds = 2,
                           lambda0 = 1, lambda1 = 1, lambda2 = 1,
                           maxitr.em = 5, impute = FALSE)
  expect_equal(runif(3), ref)
})

test_that("plot_path() restores par()", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 40, seed = 3)
  fit <- ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
               v0s = c(0.1, 0.05), maxitr.em = 10, impute = FALSE)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  before <- graphics::par("mfrow")
  expect_invisible(plot_path(fit$v0s, fit, par = c(1, 2)))
  expect_equal(graphics::par("mfrow"), before)
})
