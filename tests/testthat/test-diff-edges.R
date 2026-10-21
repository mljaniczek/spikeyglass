# diff_edges(): permutation and bootstrap differential-edge inference

quiet_fit <- function(...) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(ssjgl(...)))
  out
}

# Two groups that share a band graph except for one strong edge (1, 2),
# present only in group 1.
diff_data <- function(p = 6, n = 150, seed = 1) {
  set.seed(seed)
  Om <- diag(p)
  for (j in 1:(p - 1)) Om[j, j + 1] <- Om[j + 1, j] <- 0.3
  Om2 <- Om
  Om[1, 3] <- Om[3, 1] <- 0.45                       # group-1-only edge
  Y <- lapply(list(Om, Om2), function(O) MASS::mvrnorm(n, rep(0, p), solve(O)))
  lapply(Y, function(m) { colnames(m) <- paste0("x", 1:p); m })
}

fit_args <- list(lambda0 = 1, lambda1 = 1, lambda2 = 1, standardize = TRUE,
                 impute = FALSE, maxitr.em = 50)

test_that(".pooled_perm_pvalues matches the pooled-null definition", {
  null <- matrix(c(0, 0.1, 0.2, 0.3, 0.4, 0.5), 2, 3)
  pp <- .pooled_perm_pvalues(c(0.25, 0.5, 0.6, 0), null)
  # (1 + #null >= obs) / (1 + #null)
  expect_equal(pp$pvalue, c(4, 2, 1, 7) / 7)
  expect_equal(pp$p_adj, p.adjust(pp$pvalue, "BH"))
})

test_that("diff_edges() permutation table has the documented structure", {
  skip_on_cran()
  Y <- diff_data()
  fit <- do.call(quiet_fit, c(list(Y), fit_args, doubly = TRUE))
  set.seed(1)
  de <- diff_edges(fit, Y = Y, B = 10, maxitr.em = 30)
  p <- 6
  expect_s3_class(de, "ssjgl_diff_edges")
  expect_s3_class(de, "data.frame")
  expect_equal(nrow(de), p * (p - 1) / 2)
  expect_named(de, c("i", "j", "node1", "node2", "est_g1", "est_g2", "diff",
                     "pvalue", "p_adj", "selected", "prob_dissimilar"))
  expect_true(all(de$i < de$j))
  expect_equal(de$node1, paste0("x", de$i))
  pc <- lapply(fit$thetalist[[length(fit$thetalist)]], precision_to_pcor)
  expect_equal(de$est_g1, unname(pc[[1]][upper.tri(pc[[1]])]))
  expect_equal(de$diff, de$est_g1 - de$est_g2)
  expect_equal(de$prob_dissimilar, unname(fit$problist2[[3]][upper.tri(fit$problist2[[3]])]))
  expect_true(all(de$pvalue > 0 & de$pvalue <= 1))
  expect_true(all(de$p_adj >= de$pvalue))
  expect_equal(de$selected, de$p_adj < 0.05)
  expect_equal(attr(de, "B"), 10)
  expect_length(attr(de, "null_scores"), 10 * nrow(de))
})

test_that("diff_edges() finds a strong group-specific edge", {
  skip_on_cran()
  Y <- diff_data(n = 200)
  fit <- do.call(quiet_fit, c(list(Y), fit_args))
  set.seed(2)
  de <- diff_edges(fit, Y = Y, B = 20, maxitr.em = 30)
  e13 <- de[de$i == 1 & de$j == 3, ]
  expect_true(e13$selected)
  expect_gt(abs(e13$diff), 0.1)
  expect_null(de$prob_dissimilar)          # single spike-and-slab
  # shared band edges should mostly not be called
  band <- de$j == de$i + 1
  expect_lte(sum(de$selected[band]), 1)
})

test_that("diff_edges() is reproducible with set.seed and recovers Y from the call", {
  skip_on_cran()
  Ydat <- diff_data(n = 80)
  fit <- NULL
  # called directly (not through quiet_fit) so that fit$call$Y is `Ydat`
  utils::capture.output(fit <- ssjgl(Ydat, lambda0 = 1, lambda1 = 1, lambda2 = 1,
                                     standardize = TRUE, impute = FALSE,
                                     maxitr.em = 30))
  set.seed(3); a <- diff_edges(fit, B = 5, maxitr.em = 20)      # Y from fit$call
  set.seed(3); b <- diff_edges(fit, Y = Ydat, B = 5, maxitr.em = 20)
  expect_identical(a$pvalue, b$pvalue)
  expect_error(diff_edges(fit, Y = Ydat[1], B = 2), "does not match")
})

test_that("diff_edges() gives the same answer sequentially and with future", {
  skip_on_cran()
  skip_if_not_installed("future.apply")
  Y <- diff_data(n = 80)
  fit <- do.call(quiet_fit, c(list(Y), fit_args))
  set.seed(4); a <- diff_edges(fit, Y = Y, B = 4, maxitr.em = 20)
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan))
  set.seed(4); b <- diff_edges(fit, Y = Y, B = 4, maxitr.em = 20)
  expect_identical(a$pvalue, b$pvalue)
})

test_that("diff_edges() bootstrap gives percentile intervals", {
  skip_on_cran()
  Y <- diff_data(n = 120)
  fit <- do.call(quiet_fit, c(list(Y), fit_args))
  set.seed(5)
  de <- diff_edges(fit, Y = Y, method = "bootstrap", B = 15, maxitr.em = 30,
                   score = "precision")
  expect_named(de, c("i", "j", "node1", "node2", "est_g1", "est_g2", "diff",
                     "lower", "upper", "selected"))
  expect_true(all(de$lower <= de$upper))
  expect_equal(de$selected, de$lower > 0 | de$upper < 0)
  D <- attr(de, "boot_diffs")
  expect_equal(dim(D), c(15, nrow(de)))
  expect_equal(de$lower, unname(apply(D, 2, quantile, 0.025)))
  th <- fit$thetalist[[3]]
  expect_equal(de$est_g1, unname(th[[1]][upper.tri(th[[1]])]))
})

test_that("diff_edges() compares any two of K > 2 groups, by index or name", {
  skip_on_cran()
  sim <- simulate_ssjgl_data(K = 3, p = 5, n = 60, seed = 6)
  Y <- setNames(sim$data_list, c("a", "b", "c"))
  fit <- do.call(quiet_fit, c(list(Y), fit_args))
  set.seed(6); d1 <- diff_edges(fit, Y = Y, groups = c(1, 3), B = 3, maxitr.em = 20)
  set.seed(6); d2 <- diff_edges(fit, Y = Y, groups = c("a", "c"), B = 3, maxitr.em = 20)
  expect_identical(d1$pvalue, d2$pvalue)
  expect_identical(attr(d1, "group_names"), c("a", "c"))
  expect_error(diff_edges(fit, Y = Y, groups = c(1, 1)), "distinct")
  expect_error(diff_edges(fit, Y = Y, groups = c(1, 4)), "distinct")
})

test_that("print and plot methods run", {
  skip_on_cran()
  Y <- diff_data(n = 100)
  fit <- do.call(quiet_fit, c(list(Y), fit_args))
  set.seed(7)
  de <- diff_edges(fit, Y = Y, B = 5, maxitr.em = 20)
  expect_output(print(de), "ssjgl_diff_edges")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_invisible(plot(de))
  expect_invisible(plot(de, which = "scatter"))
})
