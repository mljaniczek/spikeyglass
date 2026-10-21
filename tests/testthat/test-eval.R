test_that("confusion_at_threshold computes correct values", {
  # Known example: 3x3 matrices
  score <- matrix(c(0, 0.8, 0.2,
                     0.8, 0, 0.6,
                     0.2, 0.6, 0), 3, 3, byrow = TRUE)
  truth <- matrix(c(0, 1, 0,
                     1, 0, 1,
                     0, 1, 0), 3, 3, byrow = TRUE)

  cm <- confusion_at_threshold(score, truth, threshold = 0.5)

  # Upper triangle: (1,2)=0.8 vs 1, (1,3)=0.2 vs 0, (2,3)=0.6 vs 1
  # Predictions at 0.5: 1, 0, 1
  # Truth:              1, 0, 1
  expect_equal(cm$TP, 2)
  expect_equal(cm$TN, 1)
  expect_equal(cm$FP, 0)
  expect_equal(cm$FN, 0)
  expect_equal(cm$TPR, 1)
  expect_equal(cm$FPR, 0)
})

test_that("roc_auc returns valid AUC", {
  score <- matrix(c(0, 0.9, 0.1, 0.3,
                     0.9, 0, 0.8, 0.2,
                     0.1, 0.8, 0, 0.7,
                     0.3, 0.2, 0.7, 0), 4, 4, byrow = TRUE)
  truth <- matrix(c(0, 1, 0, 0,
                     1, 0, 1, 0,
                     0, 1, 0, 1,
                     0, 0, 1, 0), 4, 4, byrow = TRUE)

  roc <- roc_auc(score, truth)

  expect_type(roc, "list")
  expect_true(roc$auc >= 0 && roc$auc <= 1)
  expect_true(length(roc$FPR) == length(roc$TPR))
  # Perfect or near-perfect separation should give high AUC
  expect_true(roc$auc > 0.5)
})

test_that("roc_auc handles edge cases", {
  # All positives
  score <- matrix(c(0, 1, 1, 0), 2, 2)
  truth <- matrix(c(0, 1, 1, 0), 2, 2)
  roc <- roc_auc(score, truth)
  # Only 1 edge in upper tri, no negatives
  expect_true(is.na(roc$auc))
})

test_that("compute_metrics works end to end", {
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

  metrics <- compute_metrics(fit, true_adj = sim$adj_list,
                              true_omega = sim$Omega_list, threshold = 0.5)

  expect_type(metrics, "list")
  expect_length(metrics$per_group, 2)
  expect_length(metrics$roc, 2)

  # Overall metrics
  expect_true(metrics$overall$mean_TPR >= 0 && metrics$overall$mean_TPR <= 1)
  expect_true(metrics$overall$mean_FPR >= 0 && metrics$overall$mean_FPR <= 1)

  # Per-group metrics
  for (k in 1:2) {
    g <- metrics$per_group[[k]]
    expect_true(g$TP + g$FP + g$TN + g$FN > 0)
    expect_true(is.finite(g$frobenius_norm))
  }
})

test_that("shared functions are the ggmtools objects", {
  expect_identical(roc_auc, ggmtools::roc_auc)
  expect_identical(precision_to_pcor, ggmtools::precision_to_pcor)
  expect_identical(confusion_at_threshold, ggmtools::confusion_at_threshold)
  expect_identical(plot_roc, ggmtools::plot_roc)
})

test_that("pip_table() and pcor_estimate() methods for ssjgl", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = 40, seed = 3)
  Y <- setNames(lapply(sim$data_list, function(m) { colnames(m) <- letters[1:5]; m }),
                c("ctrl", "case"))
  fit <- NULL
  utils::capture.output(fit <- suppressMessages(ssjgl(
    Y, lambda0 = 1, lambda1 = 1, lambda2 = 1, standardize = TRUE,
    maxitr.em = 20, impute = FALSE)))
  pt <- pip_table(fit)
  expect_named(pt, c("i", "j", "node1", "node2", "group", "pip"))
  expect_equal(nrow(pt), 2 * 10)
  expect_equal(unique(pt$group), c("ctrl", "case"))
  pr <- fit$problist1[[3]]
  expect_equal(pt$pip[pt$group == "ctrl"], unname(pr[upper.tri(pr)]))
  expect_equal(pt$pip[pt$group == "case"], pt$pip[pt$group == "ctrl"])
  expect_equal(pt$node1[1:3], c("a", "a", "b"))
  pc <- pcor_estimate(fit)
  expect_named(pc, c("ctrl", "case"))
  expect_equal(pc$case, precision_to_pcor(fit$thetalist[[3]]$case))
  expect_equal(pip_table(fit, v0_index = 1)$pip[1:10],
               unname(fit$problist1[[1]][upper.tri(pr)]))
  fit$groupnames <- NULL
  expect_named(pcor_estimate(fit), c("Group_1", "Group_2"))
})
