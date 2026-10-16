# Result extraction and tuning helpers for ssjgl objects


#' Generate a v0 ladder
#'
#' Creates a decreasing sequence of spike variances \code{v0} for
#' \code{\link{ssjgl}}, log-spaced in the ratio \code{lambda1/v0}. With the
#' defaults it returns the ladder that \code{ssjgl()} uses when \code{v0s} is
#' not given: three steps from \code{lambda1/v0 = 10} to \code{100}, i.e.
#' \code{lambda1 / c(10, 31.6, 100)}.
#'
#' @param lambda1 Positive scalar; the off-diagonal penalty used in
#'   \code{\link{ssjgl}}. The ladder is scaled to this value.
#' @param n_steps Integer number of v0 values in the ladder. Default 3.
#' @param min_ratio Ratio \code{lambda1/v0} at the first (weakest-spike,
#'   densest) step. Default 10.
#' @param max_ratio Ratio \code{lambda1/v0} at the last (strongest-spike,
#'   sparsest) step. Default 100.
#' @param start_sparse Logical. If \code{TRUE} (default), the sequence is
#'   decreasing in v0 (increasing in \code{lambda1/v0}), which is the
#'   direction the warm starts of the dynamic posterior exploration need. If
#'   \code{FALSE}, returns an increasing sequence.
#'
#' @return A numeric vector of \code{n_steps} v0 values, decreasing by
#'   default.
#'
#' @details
#' In SSJGL the spike and slab are \emph{Laplace} priors on each off-diagonal
#' precision entry \eqn{\omega_{jk}}, with rates \code{lambda1/v0} (spike)
#' and \code{lambda1/v1} (slab); the spike's scale is \code{v0/lambda1}. So
#' \code{v0} only matters through \code{lambda1/v0}, and this function is
#' parameterised by that ratio. (\code{sqrt(v0)} is not a standard deviation
#' of anything in the model.)
#'
#' Along the ladder, the spike penalty on a non-edge grows from
#' \code{lambda1 * min_ratio} to \code{lambda1 * max_ratio}. In the M-step it
#' soft-thresholds sample covariances (sample correlations, for standardised
#' data) at roughly \code{lambda1/(n v0)}, for group sample size \code{n}.
#'
#' The default end point \code{lambda1/v0 = 100} was chosen by a simulation
#' study (see \code{vignette("parameter-exploration")}): by that point the
#' solution path has stabilised and F1 at PIP > 0.5 has reached its plateau,
#' while longer ladders (up to 500) took longer and gave no better graphs.
#' For exploring the whole path, use e.g. \code{n_steps = 10, min_ratio = 5,
#' max_ratio = 500} with \code{\link{plot_stability}} and
#' \code{\link{plot_path}}.
#'
#' @seealso [ssjgl()], [plot_stability()]
#' @export
#'
#' @examples
#' # Default ladder (what ssjgl() uses) for lambda1 = 1
#' make_v0_ladder(lambda1 = 1)
#' # A longer exploration ladder
#' v0s <- make_v0_ladder(lambda1 = 1, n_steps = 10, min_ratio = 5, max_ratio = 500)
#' data.frame(v0 = v0s, ratio = 1 / v0s, spike_scale = v0s / 1)
make_v0_ladder <- function(lambda1, n_steps = 3, min_ratio = 10,
                           max_ratio = 100, start_sparse = TRUE) {
  stopifnot(is.numeric(lambda1), length(lambda1) == 1, lambda1 > 0,
            n_steps >= 1, min_ratio > 0, max_ratio >= min_ratio)
  if (n_steps == 1) return(lambda1 / max_ratio)
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
#' @param v0s Numeric vector of v0 values used in the fit. Defaults to
#'   \code{fit$v0s}, which \code{\link{ssjgl}} stores.
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
#' v0s <- make_v0_ladder(lambda1 = 1, n_steps = 10, min_ratio = 5, max_ratio = 500)
#' fit <- ssjgl(Y = sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 1, lambda2 = 1,
#'              v0s = v0s, standardize = TRUE, impute = FALSE)
#' plot_stability(fit)
#' }
plot_stability <- function(fit, v0s = fit$v0s, threshold = 0.5,
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
