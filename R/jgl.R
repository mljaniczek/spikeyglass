#' Joint graphical lasso
#'
#' Fits the joint graphical lasso (JGL) of Danaher, Wang & Witten (2014):
#' K precision matrices estimated together with a lasso penalty on each and a
#' fused or group penalty across them. This is a reimplementation of
#' \code{JGL::JGL()} (JGL 2.3.2, archived from CRAN in 2025) with the same
#' arguments, defaults and return value, using the C++ ADMM that
#' \code{\link{ssjgl}} uses for its M-step. It reproduces \code{JGL::JGL()}
#' to numerical precision (see the package tests).
#'
#' The objective is
#' \deqn{\sum_k w_k [\mathrm{tr}(S_k\Theta_k) - \log\det\Theta_k]
#'   + \lambda_1 \sum_k \sum_{i \ne j} |\theta^{(k)}_{ij}| + P_2(\Theta),}
#' where \eqn{S_k} is the (biased) sample covariance of group k and
#' \eqn{P_2} is \eqn{\lambda_2 \sum_{k < k'} \sum_{ij}
#' |\theta^{(k)}_{ij} - \theta^{(k')}_{ij}|} (fused) or \eqn{\lambda_2
#' \sum_{i \ne j} \sqrt{\sum_k (\theta^{(k)}_{ij})^2}} (group). As in JGL,
#' the fused penalty also applies to the diagonal, and the lasso penalty
#' applies to the diagonal only if \code{penalize.diagonal = TRUE}. The
#' problem is split into independent blocks by JGL's exact screening rule
#' before solving.
#'
#' @param Y List of K data matrices (n_k x p), same variables in the same
#'   column order. Each column is centred.
#' @param penalty \code{"fused"} (default) or \code{"group"}.
#' @param lambda1 Lasso penalty: a scalar or a symmetric p x p matrix.
#' @param lambda2 Fused or group penalty: a scalar or a symmetric p x p
#'   matrix.
#' @param rho ADMM step size. Default 1.
#' @param weights \code{"equal"} (default; \eqn{w_k = 1}),
#'   \code{"sample.size"} (\eqn{w_k = n_k / \sum n_k}), or a numeric vector
#'   of length K.
#' @param penalize.diagonal Logical. Whether the lasso penalty applies to the
#'   diagonal (and, for the group penalty, the group penalty too). Default
#'   \code{FALSE}.
#' @param maxiter Maximum ADMM iterations per block. Default 500.
#' @param tol ADMM convergence tolerance. Default 1e-5.
#' @param warm Optional list of K p x p warm-start precision matrices.
#' @param return.whole.theta Logical. If \code{TRUE}, \code{theta} holds the
#'   full p x p matrices; if \code{FALSE} (default), only the connected
#'   variables, with the unconnected ones' diagonal entries in
#'   \code{theta.unconnected}.
#' @param screening Only \code{"fast"} is supported. (JGL's
#'   \code{"memory.efficient"} option gives the same estimates with less
#'   memory.)
#' @param truncate Off-diagonal entries with absolute value below this are set
#'   to 0. Default 1e-5.
#'
#' @return An object of class \code{"jgl"}, as from \code{JGL::JGL()}: a list
#'   with \code{theta} (list of K precision matrices, with variable names,
#'   \code{V1, ..., Vp} if \code{Y} has none), \code{connected} (logical
#'   vector; \code{FALSE} for variables with no edges in any group) and, if
#'   \code{return.whole.theta = FALSE}, \code{theta.unconnected} (list of K
#'   vectors, or \code{NULL}). Print and plot methods for this class come
#'   from the JGL package, if it is loaded.
#'
#' @references
#' Danaher, P., Wang, P. and Witten, D. M. (2014). The joint graphical lasso
#' for inverse covariance estimation across multiple classes. \emph{Journal of
#' the Royal Statistical Society: Series B}, 76(2), 373--397.
#' \doi{10.1111/rssb.12033}
#'
#' @seealso [ssjgl()] for the spike-and-slab version with adaptive,
#'   edge-specific penalties.
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 100, seed = 1)
#' fit <- jgl(sim$data_list, penalty = "fused", lambda1 = 0.1, lambda2 = 0.05,
#'            return.whole.theta = TRUE)
#' round(fit$theta[[1]][1:4, 1:4], 2)
#' # partial correlations, one matrix per group
#' pc <- lapply(fit$theta, precision_to_pcor)
jgl <- function(Y, penalty = "fused", lambda1, lambda2, rho = 1,
                weights = "equal", penalize.diagonal = FALSE, maxiter = 500,
                tol = 1e-5, warm = NULL, return.whole.theta = FALSE,
                screening = "fast", truncate = 1e-5) {
  if (!is.list(Y) || length(Y) < 1 || !all(vapply(Y, is.matrix, logical(1))))
    stop("Y must be a list of data matrices")
  penalty <- match.arg(penalty, c("fused", "group"))
  if (!identical(screening, "fast"))
    stop("Only screening = \"fast\" is supported")
  p <- ncol(Y[[1]])
  K <- length(Y)
  if (any(vapply(Y, ncol, integer(1)) != p))
    stop("All matrices in Y must have the same number of columns")
  n <- vapply(Y, nrow, integer(1))
  check_lambda <- function(l, nm) {
    if (!is.numeric(l) || anyNA(l) || any(l < 0) ||
        !(length(l) == 1 || identical(dim(l), c(p, p))))
      stop(nm, " must be a non-negative scalar or p x p matrix")
  }
  check_lambda(lambda1, "lambda1")
  check_lambda(lambda2, "lambda2")

  varnames <- colnames(Y[[1]])
  if (length(varnames) == 0) varnames <- paste0("V", seq_len(p))

  if (length(weights) == 1 && identical(weights, "equal")) {
    weights <- rep(1, K)
  } else if (length(weights) == 1 && identical(weights, "sample.size")) {
    weights <- n / sum(n)
  }
  if (!is.numeric(weights) || length(weights) != K)
    stop("weights must be \"equal\", \"sample.size\" or a numeric vector of length K")

  # (biased) sample covariances; cov() centres the columns
  S <- lapply(Y, function(m) stats::cov(m) * (nrow(m) - 1) / nrow(m))

  # JGL's exact screening rule: variables connected in the block graph
  if (penalty == "fused") {
    if (K == 2) {
      crit <- (abs(S[[1]]) * weights[1] > lambda1 + lambda2) |
        (abs(S[[2]]) * weights[2] > lambda1 + lambda2) |
        (abs(weights[1] * S[[1]] + weights[2] * S[[2]]) > 2 * lambda1)
    } else {
      crit <- Reduce(`|`, lapply(seq_len(K), function(k)
        abs(S[[k]]) * weights[k] > lambda1))
    }
  } else {
    tempsum <- Reduce(`+`, lapply(seq_len(K), function(k)
      pmax(weights[k] * abs(S[[k]]) - lambda1, 0)^2))
    crit <- tempsum > lambda2^2
  }
  crit <- crit != 0
  crit[is.na(crit)] <- FALSE
  diag(crit) <- TRUE
  membership <- connected_components_cpp(crit)
  sizes <- tabulate(membership)
  connected <- sizes[membership] > 1
  unconnected <- !connected
  blocklist <- lapply(which(sizes > 1), function(b) which(membership == b))

  # unconnected variables: diagonal-only problem. JGL's vector ADMM performs
  # the same iterations as the matrix ADMM on the diagonal of S.
  theta.unconnected <- NULL
  if (any(unconnected)) {
    u <- which(unconnected)
    lam1u <- if (length(lambda1) == 1) rep(lambda1, length(u)) else diag(lambda1)[u]
    lam2u <- if (length(lambda2) == 1) rep(lambda2, length(u)) else diag(lambda2)[u]
    if (!penalize.diagonal) {
      lam1u <- lam1u * 0
      if (penalty == "group") lam2u <- lam2u * 0
    }
    Su <- lapply(S, function(s) diag(diag(s)[u], nrow = length(u)))
    res <- admm_iters_adaptive_cpp(Su, diag(lam1u, nrow = length(u)),
                                   diag(lam2u, nrow = length(u)), penalty,
                                   rho, 1, weights, maxiter, tol, NULL)
    theta.unconnected <- lapply(res$Z, function(z) {
      v <- diag(z)
      names(v) <- varnames[u]
      v
    })
  }

  # connected variables: one ADMM per block
  pc <- sum(connected)
  cidx <- integer(p)
  cidx[connected] <- seq_len(pc)
  theta <- lapply(seq_len(K), function(k)
    matrix(0, pc, pc, dimnames = list(varnames[connected], varnames[connected])))
  for (bl in blocklist) {
    lam1.bl <- if (length(lambda1) == 1) lambda1 else lambda1[bl, bl]
    lam2.bl <- if (length(lambda2) == 1) lambda2 else lambda2[bl, bl]
    lam1.bl <- penalty.as.matrix(lam1.bl, length(bl),
                                 penalize.diagonal = penalize.diagonal)
    lam2.bl <- penalty.as.matrix(lam2.bl, length(bl),
                                 penalize.diagonal = penalty == "fused" || penalize.diagonal)
    warm.bl <- if (length(warm) == 0) NULL else lapply(warm, function(w) w[bl, bl])
    res <- admm_iters_adaptive_cpp(lapply(S, function(s) s[bl, bl, drop = FALSE]),
                                   lam1.bl, lam2.bl, penalty, rho, 1, weights,
                                   maxiter, tol, warm.bl)
    for (k in seq_len(K)) theta[[k]][cidx[bl], cidx[bl]] <- res$Z[[k]]
  }

  if (pc > 0) {
    for (k in seq_len(K)) {
      small <- abs(theta[[k]]) < truncate
      diag(small) <- FALSE
      theta[[k]][small] <- 0
    }
  }

  if (!return.whole.theta) {
    out <- list(theta = theta, theta.unconnected = theta.unconnected,
                connected = connected)
  } else {
    whole <- lapply(seq_len(K), function(k) {
      m <- matrix(0, p, p, dimnames = list(varnames, varnames))
      if (any(unconnected)) diag(m)[unconnected] <- theta.unconnected[[k]]
      m[connected, connected] <- theta[[k]]
      m
    })
    out <- list(theta = whole, connected = connected)
  }
  class(out) <- "jgl"
  out
}
