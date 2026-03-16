# Result extraction and tuning helpers for ssjgl objects


#' Generate a v0 ladder scaled to lambda1
#'
#' Creates a decreasing sequence of spike variance values (\code{v0}) appropriate
#' for the dynamic posterior exploration strategy in \code{\link{ssjgl}}.
#' The v0 values are scaled relative to \code{lambda1} so that the effective
#' penalty ratio \code{lambda1/v0} increases smoothly. This follows the pattern
#' used in the reference implementation (Li et al., 2019).
#'
#' The formula is \code{v0 = lambda1 / (lambda1 + seq(from, to, length.out))},
#' producing a harmonic sequence that spans from a weak penalty (\code{v0}
#' close to \code{lambda1}) to a strong penalty (\code{v0} close to 0).
#'
#' @param lambda1 Numeric scalar; the off-diagonal penalty used in
#'   \code{\link{ssjgl}}. The v0 ladder is scaled to this value.
#' @param n_steps Integer number of v0 values in the ladder. Default 15.
#' @param max_mult Numeric maximum multiplier for the denominator. Controls
#'   how sparse the final (smallest) v0 is. Larger values produce sparser
#'   final models. Default 200. The smallest v0 will be approximately
#'   \code{lambda1 / max_mult}.
#' @param start_sparse Logical. If \code{TRUE} (default), the first v0 is
#'   the largest (weakest penalty) and the sequence is decreasing, which is
#'   the correct direction for warm-starting. If \code{FALSE}, returns an
#'   increasing sequence.
#'
#' @return A numeric vector of length \code{n_steps} with v0 values,
#'   decreasing by default.
#'
#' @details
#' \strong{Choosing \code{max_mult}}: This controls the effective penalty
#' range. The effective spike penalty at the final step is approximately
#' \code{max_mult}. For example:
#' \itemize{
#'   \item \code{max_mult = 50}: moderate sparsity (good starting point)
#'   \item \code{max_mult = 200}: strong sparsity (default, good for most cases)
#'   \item \code{max_mult = 1000}: very aggressive sparsity
#' }
#'
#' \strong{Relationship to lambdas}: What matters for the SSJGL algorithm is
#' the ratio \code{lambda1/v0}, not the absolute value of \code{v0}. This
#' function ensures that the v0 ladder produces a sensible range of effective
#' penalties regardless of how large or small \code{lambda1} is.
#'
#' @seealso [ssjgl()], [plot_stability()]
#' @export
#'
#' @examples
#' # For normalized data with lambda1 = 0.1
#' v0s <- make_v0_ladder(lambda1 = 0.1)
#' range(v0s)
#'
#' # For raw data with lambda1 = 1
#' v0s <- make_v0_ladder(lambda1 = 1, n_steps = 20, max_mult = 100)
#' range(v0s)
make_v0_ladder <- function(lambda1, n_steps = 15, max_mult = 200,
                           start_sparse = TRUE) {
  stopifnot(lambda1 > 0, n_steps >= 2, max_mult > 1)
  denom <- lambda1 + seq(0, max_mult, length.out = n_steps)
  v0s <- lambda1 / denom
  if (start_sparse) {
    v0s <- rev(sort(v0s))
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
#' @param fit An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#'
#' @return A list of K precision matrices (p x p).
#' @export
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
#' @param fit An object of class \code{ssjgl}.
#' @param v0_index Integer index into the v0 ladder. Default \code{NULL}
#'   uses the last step.
#'
#' @return A list of K partial correlation matrices (p x p).
#' @export
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
extract_probabilities <- function(fit, v0_index = NULL) {
  if (is.null(v0_index)) v0_index <- length(fit$thetalist)
  list(
    prob1 = fit$problist1[[v0_index]],
    prob2 = fit$problist2[[v0_index]]
  )
}
