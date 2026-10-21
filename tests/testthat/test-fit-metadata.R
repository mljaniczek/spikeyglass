# ssjgl() stores its settings and restores variable names on every path.

quiet_fit <- function(...) {
  out <- NULL
  # tiny fits may legitimately select no edge (collapse warning)
  utils::capture.output(out <- suppressWarnings(suppressMessages(ssjgl(...))))
  out
}

test_that("ssjgl() stores v0s, hyperparameters, dimensions and the call", {
  sim <- simulate_ssjgl_data(K = 2, p = 6, n = 40, seed = 1)
  fit <- quiet_fit(sim$data_list, penalty = "group", lambda0 = 1, lambda1 = 0.5,
                   lambda2 = 0.25, v0s = c(0.1, 0.02), maxitr.em = 20,
                   impute = FALSE, doubly = TRUE, b = 6)
  expect_equal(fit$v0s, c(0.1, 0.02))
  expect_equal(fit[c("lambda0", "lambda1", "lambda2", "v1", "a", "b")],
               list(lambda0 = 1, lambda1 = 0.5, lambda2 = 0.25, v1 = 1, a = 1, b = 6))
  expect_identical(fit$penalty, "group")
  expect_true(fit$doubly)
  expect_equal(fit$n, c(40, 40))
  expect_equal(fit$p, 6)
  expect_equal(fit$K, 2)
  expect_true(is.call(fit$call))
  expect_identical(fit$call[[1]], as.name("ssjgl"))
  expect_null(fit$center)
  expect_null(fit$scale)
})

test_that("variable and group names are restored on the C++ and R paths", {
  sim <- simulate_ssjgl_data(K = 2, p = 6, n = 40, seed = 2)
  vn <- paste0("gene", 1:6)
  Y <- lapply(sim$data_list, function(m) { colnames(m) <- vn; m })
  names(Y) <- c("tumour", "normal")
  Ymiss <- Y
  Ymiss[[1]][3, 2] <- NA                       # forces the R EM path
  for (YY in list(Y, Ymiss)) {
    fit <- quiet_fit(YY, lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
                     v0s = c(0.1, 0.02), maxitr.em = 20, doubly = TRUE)
    expect_identical(fit$varnames, vn)
    expect_identical(fit$groupnames, c("tumour", "normal"))
    for (i in 1:2) {
      expect_identical(names(fit$thetalist[[i]]), c("tumour", "normal"))
      expect_identical(dimnames(fit$thetalist[[i]]$tumour), list(vn, vn))
      expect_identical(dimnames(fit$problist1[[i]]), list(vn, vn))
      expect_identical(dimnames(fit$penlist1[[i]]), list(vn, vn))
      expect_identical(dimnames(fit$problist2[[i]]), list(vn, vn))
    }
  }
  # Unnamed input gets V1..Vp on both paths
  fit <- quiet_fit(sim$data_list, lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
                   v0s = 0.05, maxitr.em = 20, impute = FALSE)
  expect_identical(rownames(fit$thetalist[[1]][[1]]), paste0("V", 1:6))
  expect_null(names(fit$thetalist[[1]]))
})

test_that("plot_stability() uses the stored v0s by default", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 40, seed = 3)
  fit <- quiet_fit(sim$data_list, lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
                   v0s = c(0.1, 0.05, 0.02), maxitr.em = 20, impute = FALSE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  st <- plot_stability(fit)
  expect_equal(st$v0s, c(0.1, 0.05, 0.02))
})

test_that("standardize = TRUE fits the column-standardised data", {
  sim <- simulate_ssjgl_data(K = 2, p = 6, n = 50, seed = 4)
  Y <- lapply(sim$data_list, function(m) sweep(m, 2, c(1, 5, 0.2, 3, 10, 0.5), "*"))
  args <- list(lambda0 = 1, lambda1 = 1, lambda2 = 1, v0s = c(0.1, 0.03),
               maxitr.em = 50, impute = FALSE)
  fs <- do.call(quiet_fit, c(list(Y), args, standardize = TRUE))
  fm <- do.call(quiet_fit, c(list(lapply(Y, scale)), args))
  expect_equal(unname(fs$thetalist[[2]]), unname(fm$thetalist[[2]]), tolerance = 1e-10)
  expect_equal(fs$scale[1, ], apply(Y[[1]], 2, sd))
  expect_equal(fs$center[2, ], colMeans(Y[[2]]))
  # normalize = TRUE alone only centres, so the scale matters
  fn <- do.call(quiet_fit, c(list(Y), args, normalize = TRUE))
  expect_false(isTRUE(all.equal(unname(fn$thetalist[[2]]), unname(fs$thetalist[[2]]))))
})

test_that("standardize = TRUE returns imputed values on the original scale", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 2, p = 6, n = 60, seed = 5)
  Y <- sim$data_list
  Y[[1]][2, 3] <- NA; Y[[2]][5, 1] <- NA
  sc <- c(2, 0.5, 4, 1, 3, 10); sh <- c(1, -2, 0, 5, 3, 7)
  Yt <- lapply(Y, function(m) sweep(sweep(m, 2, sc, "*"), 2, sh, "+"))
  args <- list(lambda0 = 1, lambda1 = 1, lambda2 = 1, v0s = 0.05,
               maxitr.em = 20, standardize = TRUE)
  f0 <- do.call(quiet_fit, c(list(Y), args))
  ft <- do.call(quiet_fit, c(list(Yt), args))
  j <- ft$missed[, "col"]
  expect_equal(ft$imputed, f0$imputed * sc[j] + sh[j], tolerance = 1e-8)
})

test_that("standardize = TRUE rejects constant columns", {
  Y <- list(cbind(rnorm(10), 1, rnorm(10)), matrix(rnorm(30), 10, 3))
  expect_error(suppressMessages(ssjgl(Y, lambda0 = 1, lambda1 = 1, lambda2 = 1,
                                      v0s = 0.05, standardize = TRUE, impute = FALSE)),
               "positive variance")
})
