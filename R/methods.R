# S3 methods for ssjgl objects

#' Print an ssjgl object
#'
#' @param x An object of class \code{ssjgl}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisible \code{x}.
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' print(fit)
print.ssjgl <- function(x, ...) {
  K <- length(x$thetalist[[1]])
  p <- nrow(x$thetalist[[1]][[1]])
  n_v0 <- length(x$thetalist)
  total_time <- sum(x$timelist)

  cat("Spike-and-Slab Joint Graphical Lasso (SSJGL)\n")
  cat(sprintf("  Groups (K): %d\n", K))
  cat(sprintf("  Variables (p): %d\n", p))
  cat(sprintf("  v0 ladder steps: %d\n", n_v0))
  cat(sprintf("  Total EM iterations: %d\n", sum(x$itrlist)))
  cat(sprintf("  Total time: %.1f seconds\n", total_time))

  # Edge counts at final v0 step
  theta_final <- x$thetalist[[n_v0]]
  prob_final <- x$problist1[[n_v0]]
  for (k in seq_len(K)) {
    adj <- theta_final[[k]]
    diag(adj) <- 0
    n_edges <- sum(adj != 0) / 2
    cat(sprintf("  Group %d: %d non-zero edges (from precision)\n", k, n_edges))
  }
  if (!is.null(prob_final)) {
    n_prob_edges <- sum(prob_final[upper.tri(prob_final)] > 0.5)
    cat(sprintf("  Edges with P(inclusion) > 0.5: %d\n", n_prob_edges))
  }

  invisible(x)
}


#' Summarize an ssjgl object
#'
#' @param object An object of class \code{ssjgl}.
#' @param ... Additional arguments (ignored).
#'
#' @return A list of class \code{summary.ssjgl} with summary information.
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' summary(fit)
summary.ssjgl <- function(object, ...) {
  K <- length(object$thetalist[[1]])
  p <- nrow(object$thetalist[[1]][[1]])
  n_v0 <- length(object$thetalist)

  # Edge counts per group per v0 step
  edge_counts <- matrix(NA, n_v0, K)
  for (i in seq_len(n_v0)) {
    for (k in seq_len(K)) {
      adj <- object$thetalist[[i]][[k]]
      diag(adj) <- 0
      edge_counts[i, k] <- sum(adj != 0) / 2
    }
  }
  colnames(edge_counts) <- paste0("Group", seq_len(K))
  rownames(edge_counts) <- paste0("v0_", seq_len(n_v0))

  out <- list(
    K = K,
    p = p,
    n_v0 = n_v0,
    itrlist = object$itrlist,
    timelist = object$timelist,
    pi1list = object$pi1list,
    pi2list = object$pi2list,
    edge_counts = edge_counts
  )
  class(out) <- "summary.ssjgl"
  out
}


#' Print summary of ssjgl
#' @param x A \code{summary.ssjgl} object.
#' @param ... Additional arguments (ignored).
#' @export
print.summary.ssjgl <- function(x, ...) {
  cat("SSJGL Summary\n")
  cat(sprintf("  K = %d groups, p = %d variables, %d v0 steps\n",
              x$K, x$p, x$n_v0))
  cat(sprintf("  Total time: %.1f s (mean %.1f s per step)\n",
              sum(x$timelist), mean(x$timelist)))
  cat("\nEdge counts per v0 step:\n")
  print(x$edge_counts)
  cat("\nEM iterations per step:", x$itrlist, "\n")
  cat("Pi_delta per step:", round(unlist(x$pi1list), 4), "\n")
  invisible(x)
}


#' Extract precision matrices from an ssjgl fit
#'
#' Equivalent to \code{\link{extract_precision}}.
#'
#' @param object An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step (most sparse).
#' @param ... Additional arguments (ignored).
#'
#' @return A list of K precision matrices (p x p).
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' theta <- coef(fit)
#' str(theta)  # list of K precision matrices
coef.ssjgl <- function(object, v0_index = NULL, ...) {
  if (is.null(v0_index)) v0_index <- length(object$thetalist)
  object$thetalist[[v0_index]]
}


#' Extract partial correlations from an ssjgl fit
#'
#' Equivalent to \code{\link{extract_pcor}}.
#'
#' @param object An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#' @param ... Additional arguments (ignored).
#'
#' @return A list of K partial correlation matrices (p x p).
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' pcor <- fitted(fit)
#' str(pcor)  # list of K partial correlation matrices
fitted.ssjgl <- function(object, v0_index = NULL, ...) {
  if (is.null(v0_index)) v0_index <- length(object$thetalist)
  lapply(object$thetalist[[v0_index]], precision_to_pcor)
}


#' Plot partial correlation heatmaps from an ssjgl fit
#'
#' Produces a side-by-side heatmap of estimated partial correlations for
#' each group at the specified v0 step. Edges with inclusion probability
#' below \code{threshold} are masked (set to zero).
#'
#' @param x An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#' @param threshold Numeric; edges with inclusion probability below this
#'   value are masked. Default 0.5. Set to 0 to show all edges.
#' @param zlim Numeric vector of length 2 for the color scale. Default
#'   \code{c(-1, 1)}.
#' @param col Color palette vector. Default uses \code{hcl.colors(50, "Blue-Red 3")}.
#' @param ... Additional arguments passed to \code{\link[graphics]{image}}.
#'
#' @return Invisible list of K partial correlation matrices (after masking).
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' plot(fit)
plot.ssjgl <- function(x, v0_index = NULL, threshold = 0.5,
                       zlim = c(-1, 1),
                       col = grDevices::hcl.colors(50, "Blue-Red 3"),
                       ...) {
  if (is.null(v0_index)) v0_index <- length(x$thetalist)
  K <- length(x$thetalist[[v0_index]])
  p <- nrow(x$thetalist[[v0_index]][[1]])
  prob_mat <- x$problist1[[v0_index]]

  pcor_list <- vector("list", K)
  old_par <- graphics::par(mfrow = c(1, K), mar = c(3, 3, 3, 1))
  on.exit(graphics::par(old_par))

  for (k in seq_len(K)) {
    pcor_k <- precision_to_pcor(x$thetalist[[v0_index]][[k]])
    # Mask edges below threshold
    mask <- prob_mat >= threshold
    diag(mask) <- TRUE
    pcor_k[!mask] <- 0
    diag(pcor_k) <- NA

    graphics::image(1:p, 1:p, t(pcor_k[p:1, ]),
                    col = col, zlim = zlim,
                    main = paste("Partial Correlations: Group", k),
                    xlab = "Variable", ylab = "Variable",
                    axes = FALSE, ...)
    graphics::axis(1, at = seq(1, p, by = max(1, p %/% 10)))
    graphics::axis(2, at = seq(1, p, by = max(1, p %/% 10)),
                   labels = seq(p, 1, by = -max(1, p %/% 10)))
    pcor_list[[k]] <- pcor_k
  }

  invisible(pcor_list)
}
