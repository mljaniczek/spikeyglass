# Evaluation utilities for spikeyglass
# The shared metrics (roc_auc(), confusion_at_threshold(), plot_roc(),
# precision_to_pcor()) live in ggmtools and are re-exported (R/reexports.R).

#' Compute comprehensive evaluation metrics for an ssjgl fit
#'
#' Given an ssjgl fit object and the true adjacency/precision matrices,
#' computes edge-level metrics (TPR, FPR, AUC) and estimation error
#' (Frobenius norm, KL divergence).
#'
#' The edge indicator in SSJGL is shared by all groups, so every group is
#' scored with the same PIP matrix (against its own true graph).
#'
#' @param fit An object of class \code{ssjgl}.
#' @param true_adj List of K binary adjacency matrices (true graph structure).
#' @param true_omega List of K true precision matrices. Default \code{NULL}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL} uses
#'   the last step.
#' @param threshold Threshold for binarizing edge probabilities. Default 0.5.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{per_group}{List of K sublists, each with TP, FP, TN, FN, TPR, FPR,
#'       precision, F1, frobenius_norm, and KL_divergence.}
#'     \item{roc}{List of K ROC objects (from \code{\link{roc_auc}}).}
#'     \item{overall}{Named list with mean TPR, mean FPR, mean AUC across groups.}
#'   }
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' m <- compute_metrics(fit, sim$adj_list, sim$Omega_list)
#' m$overall
compute_metrics <- function(fit, true_adj, true_omega = NULL,
                            v0_index = NULL, threshold = 0.5) {
  if (is.null(v0_index)) v0_index <- length(fit$thetalist)
  theta_list <- fit$thetalist[[v0_index]]
  prob_mat <- fit$problist1[[v0_index]]
  K <- length(theta_list)

  per_group <- vector("list", K)
  roc_list <- vector("list", K)

  for (k in seq_len(K)) {
    # Edge-level metrics using probability of inclusion
    cm <- confusion_at_threshold(prob_mat, true_adj[[k]], threshold)

    # ROC/AUC
    roc_k <- roc_auc(prob_mat, true_adj[[k]])

    # Estimation error
    frob <- NA_real_
    kl <- NA_real_
    if (!is.null(true_omega)) {
      diff_mat <- theta_list[[k]] - true_omega[[k]]
      frob <- sqrt(sum(diff_mat^2))
      # KL(true || est) = 0.5 * (tr(Omega_est Sigma_true) - log|Omega_est Sigma_true| - p)
      prod_mat <- theta_list[[k]] %*% solve(true_omega[[k]])
      det_info <- determinant(prod_mat, logarithm = TRUE)
      if (det_info$sign > 0) {
        kl <- 0.5 * (sum(diag(prod_mat)) - as.numeric(det_info$modulus) - nrow(prod_mat))
      }
    }

    per_group[[k]] <- c(cm, list(frobenius_norm = frob, KL_divergence = kl))
    roc_list[[k]] <- roc_k
  }

  mean_tpr <- mean(vapply(per_group, function(x) x$TPR, numeric(1)))
  mean_fpr <- mean(vapply(per_group, function(x) x$FPR, numeric(1)))
  mean_auc <- mean(vapply(roc_list, function(x) x$auc, numeric(1)), na.rm = TRUE)

  list(
    per_group = per_group,
    roc = roc_list,
    overall = list(mean_TPR = mean_tpr, mean_FPR = mean_fpr, mean_AUC = mean_auc)
  )
}
