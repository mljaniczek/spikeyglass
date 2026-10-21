#' ADMM iterations with adaptive (matrix-valued) penalties (internal)
#'
#' Implements ADMM for the fused or group graphical lasso M-step with
#' matrix-valued lambda1 and lambda2 penalties, via the C++ implementation
#' (RcppArmadillo) in \code{admm_iters_adaptive_cpp}. The fused Z-update uses
#' FLSA for K = 2 and a port of JGL's general-K FLSA for K > 2.
#'
#' Takes the list of K (biased) sample covariance matrices \code{S} rather
#' than the data, so that with missing data the M-step can use the expected
#' covariance E[S | observed] (Li et al. 2019, supplement B).
#'
#' @keywords internal
#' @noRd
admm.iters.adaptive = function(S, lam1, lam2, penalty = "fused", rho = 1,
                                rho.increment = 1, weights, maxiter = 1000,
                                tol = 1e-5, warm = NULL) {
  admm_iters_adaptive_cpp(
    S_list = S,
    lam1 = lam1,
    lam2 = lam2,
    penalty = penalty,
    rho = rho,
    rho_increment = rho.increment,
    weights = weights,
    maxiter = maxiter,
    tol = tol,
    warm_list = warm
  )
}
