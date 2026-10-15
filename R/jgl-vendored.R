# Functions vendored from the JGL package
#
# JGL 2.3.2 (Danaher, Wang & Witten 2014), (c) Patrick Danaher, MIT licence.
# https://github.com/patrickjdanaher/JGL-package
# JGL was archived from CRAN on 2025-07-17. spikeyglass uses only a few of its
# internal routines, so they are reproduced here (R) or ported to C++ in
# src/admm_adaptive.cpp (flsa2 -> flsa2_update, dsgl -> dsgl_update,
# flsa.general -> flsa_general_update). See inst/COPYRIGHTS.

#' Expand a scalar penalty to a p x p matrix (internal)
#'
#' Vendored from \code{JGL:::penalty.as.matrix} (JGL 2.3.2).
#' @keywords internal
#' @noRd
penalty.as.matrix <- function(lambda, p, penalize.diagonal) {
  if (is.matrix(lambda)) {
    if (sum(lambda != t(lambda)) > 0) {
      stop("error: penalty matrix is not symmetric")
    }
    if (sum(abs(dim(lambda) - p)) != 0) {
      stop("error: penalty matrix has wrong dimension")
    }
  }
  if (length(lambda) == 1) {
    lambda = matrix(lambda, p, p)
  }
  if (!penalize.diagonal) {
    diag(lambda) = 0
  }
  return(lambda)
}

#' ADMM for unconnected (singleton) nodes (internal)
#'
#' Adapted from \code{JGL:::admm.iters.unconnected} (JGL 2.3.2): the same
#' iterations with
#' \code{lambda2 = 0} (which is how \code{JGL.adaptive} calls it), but takes
#' the vector of variances \code{S[[k]]} instead of the data. With
#' \code{lambda2 = 0} the fused and group Z-updates both reduce to
#' soft-thresholding at \code{lambda0/rho}.
#'
#' @param S List of K numeric vectors of (biased) variances.
#' @return List of K vectors: the diagonal precision entries (JGL's \code{Z}).
#' @keywords internal
#' @noRd
admm.iters.unconnected.S = function(S, lambda0, rho = 1, rho.increment = 1,
                                    weights, maxiter = 1000, tol = 1e-5) {
  K = length(S)
  p = length(S[[1]])
  n = weights
  theta = list(); for(k in 1:K){theta[[k]] = 1/S[[k]]}
  Z = list(); for(k in 1:K){Z[[k]] = rep(0, p)}
  W = list(); for(k in 1:K){W[[k]] = rep(0, p)}

  iter = 0
  diff_value = 10
  while((iter==0) || (iter<maxiter && diff_value > tol))
  {
    theta.prev = theta
    for(k in 1:K){
      B = n[k]*S[[k]] - rho*(Z[[k]] - W[[k]])
      theta[[k]] = 1/(2*rho) * (-B + sqrt(B^2 + 4*rho*n[k]))
    }
    for(k in 1:K){
      A = theta[[k]] + W[[k]]
      Z[[k]] = sign(A) * pmax(abs(A) - lambda0/rho, 0)
    }
    for(k in 1:K){W[[k]] = W[[k]] + (theta[[k]] - Z[[k]])}

    iter = iter + 1
    diff_value = 0
    for(k in 1:K) {diff_value = diff_value + sum(abs(theta[[k]] - theta.prev[[k]])) / sum(abs(theta.prev[[k]]))}
    rho = rho*rho.increment
  }
  Z
}
