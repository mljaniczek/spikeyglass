# S3 methods for ssjgl objects

#' Print an ssjgl object
#'
#' @param x An object of class \code{ssjgl}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisible \code{x}.
#' @export
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
#' @param object An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step (most sparse).
#' @param ... Additional arguments (ignored).
#'
#' @return A list of K precision matrices (p x p).
#' @export
coef.ssjgl <- function(object, v0_index = NULL, ...) {
  if (is.null(v0_index)) v0_index <- length(object$thetalist)
  object$thetalist[[v0_index]]
}


#' Extract partial correlations from an ssjgl fit
#'
#' @param object An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#' @param ... Additional arguments (ignored).
#'
#' @return A list of K partial correlation matrices (p x p).
#' @export
fitted.ssjgl <- function(object, v0_index = NULL, ...) {
  if (is.null(v0_index)) v0_index <- length(object$thetalist)
  lapply(object$thetalist[[v0_index]], precision_to_pcor)
}
