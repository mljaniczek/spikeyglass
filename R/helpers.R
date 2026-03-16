# Result extraction helpers for ssjgl objects

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
