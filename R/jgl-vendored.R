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
