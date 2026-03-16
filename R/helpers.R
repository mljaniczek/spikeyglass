# Result extraction and tuning helpers for ssjgl objects


#' Generate a v0 ladder for exploring sparsity levels
#'
#' Creates a decreasing sequence of spike variance values (\code{v0}) for use
#' with \code{\link{ssjgl}}. This is primarily useful for \strong{exploring}
#' how sparsity changes across v0 values, for diagnostics, or for replicating
#' the full dynamic posterior exploration strategy of Li et al. (2019).
#'
#' For routine use, a short ladder like \code{v0s = c(0.1, 0.03, 0.01)} is
#' recommended (see \code{\link{ssjgl}} defaults and
#' \code{vignette("parameter-exploration")} for details).
#'
#' @param lambda1 Numeric scalar; the off-diagonal penalty used in
#'   \code{\link{ssjgl}}. The v0 ladder is scaled to this value.
#' @param n_steps Integer number of v0 values in the ladder. Default 10.
#' @param min_ratio Numeric minimum effective penalty ratio
#'   \code{lambda1/v0} at the first (densest) step. Default 5.
#' @param max_ratio Numeric maximum effective penalty ratio
#'   \code{lambda1/v0} at the last (sparsest) step. Default 500.
#' @param start_sparse Logical. If \code{TRUE} (default), the first v0 is
#'   the largest (weakest penalty) and the sequence is decreasing, which is
#'   the correct direction for warm-starting. If \code{FALSE}, returns an
#'   increasing sequence.
#'
#' @return A numeric vector of length \code{n_steps} with v0 values,
#'   decreasing by default.
#'
#' @details
#' The effective penalty for edge sparsity is \code{lambda1/v0}. This
#' function creates a log-spaced ladder of v0 values such that the
#' effective penalty ranges from \code{min_ratio} to \code{max_ratio}.
#' Log-spacing concentrates more steps in the sparse (small v0) end
#' where the model is most sensitive.
#'
#' \strong{Interpreting v0}: The spike standard deviation \code{sqrt(v0)} sets the
#' scale below which partial correlations are treated as noise. For normalized
#' data where partial correlations live in [-1, 1]:
#' \itemize{
#'   \item \code{v0 = 0.1} (spike SD = 0.32): weak sparsity, broad spike
#'   \item \code{v0 = 0.01} (spike SD = 0.10): moderate sparsity, good default
#'   \item \code{v0 = 0.001} (spike SD = 0.03): aggressive sparsity
#'   \item \code{v0 = 0.0001} (spike SD = 0.01): very aggressive, may over-sparsify
#' }
#'
#' @seealso [ssjgl()], [plot_stability()]
#' @export
#'
#' @examples
#' # Exploration ladder for lambda1 = 0.5
#' v0s <- make_v0_ladder(lambda1 = 0.5, n_steps = 10)
#' data.frame(v0 = v0s, spike_sd = sqrt(v0s),
#'            eff_penalty = 0.5 / v0s)
make_v0_ladder <- function(lambda1, n_steps = 10, min_ratio = 5,
                           max_ratio = 500, start_sparse = TRUE) {
  stopifnot(lambda1 > 0, n_steps >= 2, min_ratio >= 1, max_ratio > min_ratio)
  log_ratios <- seq(log(min_ratio), log(max_ratio), length.out = n_steps)
  v0s <- lambda1 / exp(log_ratios)
  if (start_sparse) {
    v0s <- sort(v0s, decreasing = TRUE)
  } else {
    v0s <- sort(v0s)
  }
  v0s
}


#' Plot stability of graph structure across the v0 ladder
#'
#' Visualizes how the estimated graph changes across v0 steps to help
#' identify when the solution has stabilized. Shows edge counts, mean
#' inclusion probability, and (optionally) change in edges between
#' consecutive steps.
#'
#' @param fit An object of class \code{ssjgl}.
#' @param v0s Numeric vector of v0 values used in the fit (same as passed
#'   to \code{\link{ssjgl}}).
#' @param threshold Numeric threshold for counting edges from inclusion
#'   probabilities. Default 0.5.
#' @param what Character vector of panels to plot. Options: \code{"edges"}
#'   (edge count per group), \code{"prob"} (mean inclusion probability),
#'   \code{"change"} (absolute change in edges between steps). Default
#'   plots all three.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @return Invisible list with computed stability data:
#'   \describe{
#'     \item{v0s}{The v0 values.}
#'     \item{edge_counts}{Matrix of edge counts (n_steps x K).}
#'     \item{mean_prob}{Numeric vector of mean inclusion probabilities.}
#'     \item{edge_changes}{Numeric vector of edge count changes between steps
#'       (length n_steps - 1).}
#'   }
#'
#' @seealso [ssjgl()], [make_v0_ladder()], [plot_path()]
#' @export
#'
#' @examples
#' \dontrun{
#' sim <- simulate_ssjgl_data(K = 2, p = 15, n = 100, seed = 42)
#' v0s <- make_v0_ladder(lambda1 = 0.5, n_steps = 10)
#' fit <- ssjgl(Y = sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = v0s, normalize = TRUE)
#' plot_stability(fit, v0s)
#' }
plot_stability <- function(fit, v0s, threshold = 0.5,
                           what = c("edges", "prob", "change"), ...) {
  what <- match.arg(what, c("edges", "prob", "change"), several.ok = TRUE)
  n_steps <- length(fit$thetalist)
  K <- length(fit$thetalist[[1]])

  # Compute edge counts per group per step
  edge_counts <- matrix(NA_real_, n_steps, K)
  mean_prob <- numeric(n_steps)

  for (i in seq_len(n_steps)) {
    prob_mat <- fit$problist1[[i]]
    mean_prob[i] <- mean(prob_mat[upper.tri(prob_mat)])
    for (k in seq_len(K)) {
      adj <- (prob_mat >= threshold) * 1L
      diag(adj) <- 0L
      edge_counts[i, k] <- sum(adj[upper.tri(adj)])
    }
  }

  # Compute changes between consecutive steps
  total_edges <- rowSums(edge_counts)
  edge_changes <- abs(diff(total_edges))

  # Plot
  n_panels <- length(what)
  old_par <- graphics::par(mfrow = c(1, n_panels), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(old_par))

  colors <- c("steelblue", "tomato", "forestgreen", "purple",
               "orange", "brown")

  if ("edges" %in% what) {
    ylim <- c(0, max(edge_counts, na.rm = TRUE) * 1.1)
    graphics::plot(v0s, edge_counts[, 1], type = "b", pch = 19,
                   col = colors[1], ylim = ylim,
                   xlab = expression(v[0]), ylab = "Number of edges",
                   main = "Edge counts", ...)
    if (K > 1) {
      for (k in 2:K) {
        graphics::lines(v0s, edge_counts[, k], type = "b", pch = 19,
                        col = colors[k])
      }
    }
    graphics::legend("topright",
                     legend = paste("Group", seq_len(K)),
                     col = colors[seq_len(K)], lty = 1, pch = 19,
                     bty = "n", cex = 0.8)
  }

  if ("prob" %in% what) {
    graphics::plot(v0s, mean_prob, type = "b", pch = 19, col = "darkblue",
                   xlab = expression(v[0]),
                   ylab = "Mean P(inclusion)",
                   main = "Mean inclusion probability", ...)
  }

  if ("change" %in% what) {
    v0_mid <- (v0s[-1] + v0s[-n_steps]) / 2
    graphics::plot(v0_mid, edge_changes, type = "b", pch = 19,
                   col = "darkred",
                   xlab = expression(v[0]),
                   ylab = "Edges changed",
                   main = "Edge changes between steps", ...)
    graphics::abline(h = 0, lty = 2, col = "gray50")
  }

  invisible(list(
    v0s = v0s,
    edge_counts = edge_counts,
    mean_prob = mean_prob,
    edge_changes = edge_changes
  ))
}


#' Extract precision matrices from an ssjgl fit
#'
#' Equivalent to \code{coef(fit)}.
#'
#' @param fit An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#'
#' @return A list of K precision matrices (p x p).
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' theta <- extract_precision(fit)
#' dim(theta[[1]])
extract_precision <- function(fit, v0_index = NULL) {
  if (is.null(v0_index)) v0_index <- length(fit$thetalist)
  fit$thetalist[[v0_index]]
}


#' Extract binary adjacency matrices from an ssjgl fit
#'
#' Thresholds the edge inclusion probabilities to produce binary adjacency
#' matrices.
#'
#' @param fit An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#' @param threshold Numeric threshold for edge inclusion. Default 0.5.
#'
#' @return A list of K binary adjacency matrices (p x p, 0 diagonal).
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' adj <- extract_adjacency(fit, threshold = 0.5)
#' sum(adj[[1]][upper.tri(adj[[1]])])  # edge count
extract_adjacency <- function(fit, v0_index = NULL, threshold = 0.5) {
  if (is.null(v0_index)) v0_index <- length(fit$thetalist)
  prob_mat <- fit$problist1[[v0_index]]
  K <- length(fit$thetalist[[v0_index]])

  lapply(seq_len(K), function(k) {
    adj <- (prob_mat >= threshold) * 1L
    diag(adj) <- 0L
    adj
  })
}


#' Extract partial correlation matrices from an ssjgl fit
#'
#' Equivalent to \code{fitted(fit)}.
#'
#' @param fit An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#'
#' @return A list of K partial correlation matrices (p x p).
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' pcor <- extract_pcor(fit)
#' range(pcor[[1]])  # values in [-1, 1]
extract_pcor <- function(fit, v0_index = NULL) {
  if (is.null(v0_index)) v0_index <- length(fit$thetalist)
  lapply(fit$thetalist[[v0_index]], precision_to_pcor)
}


#' Extract edge inclusion probabilities from an ssjgl fit
#'
#' @param fit An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{prob1}{p x p matrix of edge inclusion probabilities P(delta=1).}
#'     \item{prob2}{p x p matrix of non-similarity probabilities P(xi=1),
#'       or NULL if not doubly spike-and-slab.}
#'   }
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = 0.01, maxitr.em = 10, impute = FALSE)
#' probs <- extract_probabilities(fit)
#' # Edges with > 50% inclusion probability
#' sum(probs$prob1[upper.tri(probs$prob1)] > 0.5)
extract_probabilities <- function(fit, v0_index = NULL) {
  if (is.null(v0_index)) v0_index <- length(fit$thetalist)
  list(
    prob1 = fit$problist1[[v0_index]],
    prob2 = fit$problist2[[v0_index]]
  )
}
