# Evaluation utilities for spikeyglass
# Functions adapted from multiGGMr (github.com/mljaniczek/multiGGMr)
# for consistent interface across multiple GGM packages.

#' Compute confusion matrix metrics at a threshold
#'
#' Given a score matrix (e.g., edge inclusion probabilities) and a binary
#' truth matrix, computes TP, FP, TN, FN, TPR, FPR, precision, and F1
#' at a given threshold. Only the upper triangle is used by default.
#'
#' @param score_mat Numeric matrix of scores (e.g., probabilities in \[0,1\]).
#' @param truth_mat Binary matrix of true edges (1 = edge, 0 = no edge).
#' @param threshold Numeric threshold; scores >= threshold are predicted as edges.
#'   Default 0.5.
#' @param upper_only Logical; if TRUE (default), only use upper triangle.
#'
#' @return A named list with elements: TP, FP, TN, FN, TPR (sensitivity),
#'   FPR (1-specificity), precision, F1.
#' @export
confusion_at_threshold <- function(score_mat, truth_mat, threshold = 0.5,
                                   upper_only = TRUE) {
  score_mat <- as.matrix(score_mat)
  truth_mat <- as.matrix(truth_mat)
  stopifnot(nrow(score_mat) == nrow(truth_mat),
            ncol(score_mat) == ncol(truth_mat))

  if (upper_only) {
    idx <- upper.tri(score_mat)
  } else {
    idx <- matrix(TRUE, nrow(score_mat), ncol(score_mat))
    diag(idx) <- FALSE
  }

  pred <- as.integer(score_mat[idx] >= threshold)
  truth <- as.integer(truth_mat[idx] != 0)

  TP <- sum(pred == 1 & truth == 1)
  FP <- sum(pred == 1 & truth == 0)
  TN <- sum(pred == 0 & truth == 0)
  FN <- sum(pred == 0 & truth == 1)

  TPR <- if ((TP + FN) > 0) TP / (TP + FN) else 0
  FPR <- if ((FP + TN) > 0) FP / (FP + TN) else 0
  prec <- if ((TP + FP) > 0) TP / (TP + FP) else 0
  F1 <- if ((prec + TPR) > 0) 2 * prec * TPR / (prec + TPR) else 0

  list(TP = TP, FP = FP, TN = TN, FN = FN,
       TPR = TPR, FPR = FPR, precision = prec, F1 = F1)
}


#' Compute ROC curve and AUC
#'
#' Computes the ROC curve (TPR vs FPR at varying thresholds) and the
#' area under the curve (AUC) for edge selection. Only the upper triangle
#' of the matrices is used.
#'
#' @param score_mat Numeric matrix of continuous scores (e.g., edge inclusion
#'   probabilities or absolute partial correlations).
#' @param truth_mat Binary matrix of true edges (1 = edge, 0 = no edge).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{FPR}{Numeric vector of false positive rates.}
#'     \item{TPR}{Numeric vector of true positive rates.}
#'     \item{thresholds}{Numeric vector of thresholds used.}
#'     \item{AUC}{Scalar area under the ROC curve (trapezoidal rule).}
#'   }
#' @export
roc_auc <- function(score_mat, truth_mat) {
  score_mat <- as.matrix(score_mat)
  truth_mat <- as.matrix(truth_mat)

  idx <- upper.tri(score_mat)
  scores <- score_mat[idx]
  truth <- as.integer(truth_mat[idx] != 0)

  n_pos <- sum(truth == 1)
  n_neg <- sum(truth == 0)

  if (n_pos == 0 || n_neg == 0) {
    return(list(FPR = c(0, 1), TPR = c(0, 1),
                thresholds = c(Inf, -Inf), AUC = NA_real_))
  }

  # Unique thresholds
  thresholds <- c(Inf, sort(unique(scores), decreasing = TRUE), -Inf)
  tpr_vec <- numeric(length(thresholds))
  fpr_vec <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    pred <- as.integer(scores >= thresholds[i])
    tpr_vec[i] <- sum(pred == 1 & truth == 1) / n_pos
    fpr_vec[i] <- sum(pred == 1 & truth == 0) / n_neg
  }

  # Sort by FPR for proper curve
  ord2 <- order(fpr_vec, tpr_vec)
  fpr_vec <- fpr_vec[ord2]
  tpr_vec <- tpr_vec[ord2]
  thresholds <- thresholds[ord2]

  # AUC via trapezoidal rule
  auc <- 0
  for (i in 2:length(fpr_vec)) {
    auc <- auc + (fpr_vec[i] - fpr_vec[i - 1]) * (tpr_vec[i] + tpr_vec[i - 1]) / 2
  }

  list(FPR = fpr_vec, TPR = tpr_vec, thresholds = thresholds, AUC = auc)
}


#' Compute comprehensive evaluation metrics for an ssjgl fit
#'
#' Given an ssjgl fit object and the true adjacency/precision matrices,
#' computes edge-level metrics (TPR, FPR, AUC) and estimation error
#' (Frobenius norm, KL divergence).
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
  mean_auc <- mean(vapply(roc_list, function(x) x$AUC, numeric(1)), na.rm = TRUE)

  list(
    per_group = per_group,
    roc = roc_list,
    overall = list(mean_TPR = mean_tpr, mean_FPR = mean_fpr, mean_AUC = mean_auc)
  )
}


#' Plot ROC curve
#'
#' Plots the ROC curve from a \code{\link{roc_auc}} result.
#'
#' @param roc_obj A list returned by \code{\link{roc_auc}}.
#' @param main Title for the plot. Default \code{"ROC Curve"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @return Invisible NULL. Called for side effect (plot).
#' @export
plot_roc <- function(roc_obj, main = "ROC Curve", ...) {
  graphics::plot(roc_obj$FPR, roc_obj$TPR, type = "l",
                 xlab = "False Positive Rate", ylab = "True Positive Rate",
                 main = main, xlim = c(0, 1), ylim = c(0, 1), ...)
  graphics::abline(a = 0, b = 1, lty = 2, col = "gray50")
  graphics::legend("bottomright",
                   legend = sprintf("AUC = %.3f", roc_obj$AUC),
                   bty = "n")
  invisible(NULL)
}
