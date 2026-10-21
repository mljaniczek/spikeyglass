#' Bayesian Spike-and-Slab Joint Graphical Lasso
#'
#' Estimates multiple related precision matrices (inverse covariance matrices)
#' across K groups using an EM algorithm with spike-and-slab priors. The method
#' encourages shared sparsity across groups via either fused or group penalties,
#' while allowing group-specific differences through adaptive, edge-specific
#' penalization.
#'
#' The algorithm follows the dynamic posterior exploration strategy of Li et al.
#' (2019), iterating over a decreasing ladder of spike variances \code{v0s}
#' (increasing spike penalty \code{lambda1/v0}) with warm-starting between
#' steps.
#'
#' @param Y List of K data matrices, each n_k x p. Missing values (\code{NA})
#'   are supported when \code{impute = TRUE}.
#' @param penalty Character: \code{"fused"} (penalizes pairwise differences
#'   between groups) or \code{"group"} (penalizes L2 norm across groups).
#' @param lambda0 Scalar penalty on diagonal entries of precision matrices.
#'   The method is relatively insensitive to this value; \code{lambda0 = 1} is
#'   recommended in most cases.
#' @param lambda1 Scalar (or matrix) base penalty on off-diagonal entries
#'   (edge-wise sparsity). In the prior, each off-diagonal precision entry
#'   has a Laplace prior with rate \code{lambda1/v0} in the spike and
#'   \code{lambda1/v1} in the slab, so sparsity is governed by the ratio
#'   \code{lambda1/v0} (see \code{v0s}). Li et al. (2019) use
#'   \code{lambda1 = 1} with variables on unit scale, and that is the
#'   recommended starting point with \code{standardize = TRUE}. If the data
#'   are not standardised, the meaning of \code{lambda1} depends on the
#'   variables' scales, because the penalty acts on precision entries.
#' @param lambda2 Scalar (or matrix) base penalty for the cross-group
#'   similarity term. Controls borrowing of strength across groups:
#'   \itemize{
#'     \item \code{lambda2 = 0}: no cross-group borrowing (separate estimation)
#'     \item \code{lambda2 = lambda1}: equal weight on sparsity and similarity
#'     \item \code{lambda2 > lambda1}: encourage more similar graphs across groups
#'     \item \code{lambda2 < lambda1}: allow groups to differ more
#'   }
#' @param v1 Numeric slab variance parameter. Default 1. Should generally be
#'   left at 1.
#' @param v0s Numeric vector of spike variances, \strong{decreasing}: the
#'   ladder for the dynamic posterior exploration of Li et al. (2019), each
#'   step warm-started from the previous one. The spike is a Laplace prior on
#'   the precision entries with rate \code{lambda1/v0}, i.e. scale
#'   \code{v0/lambda1}; it is not a normal prior, and \code{sqrt(v0)} has no
#'   partial-correlation interpretation. Only \code{lambda1/v0} matters. As a
#'   rough guide, a non-edge's precision entry is soft-thresholded in the
#'   M-step at about \code{lambda1/(n v0)} on the covariance scale (the
#'   correlation scale for standardised data), for group sample size
#'   \code{n}. Default \code{NULL} uses \code{make_v0_ladder(lambda1)}, the
#'   3-step ladder \code{lambda1/v0 = 10, 31.6, 100}, i.e.
#'   \code{v0s = c(0.1, 0.0316, 0.01)} at \code{lambda1 = 1}. This default was
#'   chosen by a simulation study; see \code{vignette("parameter-exploration")}
#'   and \code{\link{make_v0_ladder}}. It requires a positive scalar
#'   \code{lambda1}; otherwise supply \code{v0s}.
#' @param doubly Logical. If \code{TRUE}, uses doubly spike-and-slab prior
#'   with separate indicators for edge existence (delta) and cross-group
#'   similarity (xi). Default \code{FALSE}.
#' @param rho Numeric ADMM step-size parameter. Default 1.
#' @param a Numeric Beta prior shape1 for inclusion probabilities. Default 1.
#' @param b Numeric Beta prior shape2. Default 1. Setting \code{b = p} gives
#'   a sparse prior.
#' @param maxitr.em Integer max EM iterations per v0 step. Default 500.
#' @param tol.em Numeric EM convergence tolerance for the largest absolute
#'   change in any precision entry. Default 1e-4.
#' @param tol.pi Numeric EM convergence tolerance for the change in
#'   \code{log(pi_delta)} (and \code{log(pi_xi)} if \code{doubly}). A ladder
#'   step stops only when both tolerances are met; checking theta alone
#'   could stop the EM while pi was still collapsing. Default 1e-3 (a 0.1\%
#'   relative change).
#' @param maxitr.jgl Integer max ADMM iterations for M-step. Default 500.
#' @param tol.jgl Numeric ADMM convergence tolerance. Default 1e-5.
#' @param truncate Numeric threshold below which entries are zeroed. Default
#'   1e-5.
#' @param normalize Logical. If \code{TRUE}, mean-centres each variable
#'   within each group. It does \emph{not} scale. Default \code{FALSE}.
#'   Centring does not change the estimates, because the sample covariance is
#'   centred anyway. When missing values are imputed the data are always
#'   centred internally (the conditional-mean imputation assumes mean zero),
#'   and the imputed values are returned on the original scale. Kept for
#'   back-compatibility; see \code{standardize}.
#' @param c Numeric diagonal regularization for initial precision estimate.
#'   Default 0.1.
#' @param impute Logical. If \code{TRUE}, imputes \code{NA}s via conditional
#'   MVN at each EM iteration. The M-step then uses the expected sample
#'   covariance given the observed data, i.e. the covariance of the imputed
#'   data plus the conditional covariance of the missing cells (Li et al.,
#'   2019, supplement B). Default \code{TRUE}.
#' @param carry_pi Logical. If \code{TRUE}, pi_delta and pi_xi start each
#'   ladder step from their values at the end of the previous step. If
#'   \code{FALSE} (default, as in Li et al. 2019), they restart at the prior
#'   mean \code{a/(a+b)}.
#' @param verbose Logical. If \code{TRUE}, report progress (each EM
#'   iteration on the R path, and each ladder step) with \code{message()}.
#'   Default \code{FALSE}.
#' @param standardize Logical. If \code{TRUE}, centres \emph{and} scales each
#'   variable to unit variance within each group before fitting, so the
#'   precision matrices are estimated on the correlation scale and
#'   \code{lambda1}, \code{lambda2} and \code{v0s} have the same meaning
#'   whatever the variables' units. Recommended unless the variables are
#'   already on a common, meaningful scale. Default \code{FALSE} (for
#'   back-compatibility). Imputed values are returned on the original scale.
#'
#' @return An object of class \code{"ssjgl"}. With \code{standardize = TRUE}
#'   the precision matrices are those of the standardised variables. It is a
#'   list with elements:
#'   \describe{
#'     \item{thetalist}{List (length = length(v0s)) of lists of K precision
#'       matrices (p x p) at each v0 step.}
#'     \item{pi1list}{List of pi_delta values (edge inclusion probability)
#'       at each v0 step.}
#'     \item{pi2list}{List of pi_xi values (non-similarity probability)
#'       at each v0 step.}
#'     \item{fitlist}{List of raw JGL fit objects at each v0 step.}
#'     \item{itrlist}{Integer vector of EM iterations at each v0 step.}
#'     \item{problist1}{List of p x p edge inclusion probability matrices
#'       P(delta=1) at each v0 step.}
#'     \item{penlist1}{List of p x p adaptive penalty weight matrices for
#'       lambda1 at each v0 step.}
#'     \item{problist2}{List of p x p non-similarity probability matrices
#'       P(xi=1). NULL if \code{doubly = FALSE}.}
#'     \item{penlist2}{List of p x p adaptive penalty weights for lambda2.
#'       NULL if \code{doubly = FALSE}.}
#'     \item{timelist}{Numeric vector of wall-clock seconds per v0 step.}
#'     \item{imputed}{Numeric vector of imputed values on the original data
#'       scale (one per row of \code{missed}), or NULL.}
#'     \item{missed}{Matrix of (group, row, col) for missing values, or NULL.}
#'     \item{v0s, v1, lambda0, lambda1, lambda2, penalty, doubly, a, b}{The
#'       hyperparameters used.}
#'     \item{n, p, K}{Sample sizes (length K), number of variables and number
#'       of groups.}
#'     \item{varnames, groupnames}{Variable names (from \code{colnames(Y[[1]])},
#'       or \code{V1, ..., Vp}) used as dimnames of every p x p output, and
#'       group names (from \code{names(Y)}, possibly NULL).}
#'     \item{normalize, standardize, center, scale}{The preprocessing used:
#'       \code{center} is the K x p matrix of column means subtracted (NULL if
#'       the data were not centred) and \code{scale} the K x p matrix of
#'       standard deviations divided by (NULL unless \code{standardize = TRUE}).}
#'     \item{args}{Named list of every argument except \code{Y}, as used
#'       (with the default \code{v0s} filled in); used to refit with the same
#'       settings, e.g. by \code{\link{diff_edges}}.}
#'     \item{call}{The matched call.}
#'   }
#'
#' @section Posterior inclusion probabilities:
#' \code{problist1} holds the E-step probabilities \eqn{P(\delta_{jk} = 1)}
#' given the current estimates. They are conditional probabilities inside an
#' EM (posterior-mode) algorithm, not calibrated posterior probabilities, so
#' they should not be used for Bayesian FDR control. They do rank edges well,
#' and PIP > 0.5 is a reasonable selection rule with the default ladder.
#'
#' An edge reaches PIP 0.5 only if, roughly,
#' \eqn{\sum_g |\theta^{(g)}_{jk}| > (G \log r - \mathrm{logit}\,\pi_\delta)
#' / (r - 1)}, with \eqn{r = \lambda_1 / v_0}. If the ladder ends at a small
#' \eqn{r} (e.g. 16), this bar is above typical edge sizes; then
#' \eqn{\pi_\delta} shrinks, which raises the bar again, and the PIPs
#' collapse towards 0 even though they still rank edges. The default ladder
#' ends at \eqn{r = 100}. \code{ssjgl()} warns when no edge has PIP > 0.5 at
#' the last step. \eqn{\pi} is kept inside \eqn{[1/N, 1 - 1/N]}, where
#' \eqn{N = p(p-1)/2}. See \code{data-raw/pip-collapse-diagnostic.md} in the
#' package sources for the analysis.
#'
#' \code{problist2} (\eqn{P(\xi_{jk} = 1)}, doubly only) is the most fragile
#' output; treat it as a ranking.
#'
#' @references
#' Li, Z. R., McCormick, T. H., & Clark, S. J. (2019). Bayesian Joint
#' Spike-and-Slab Graphical Lasso. \emph{ICML 2019}.
#'
#' @seealso [make_v0_ladder()], [plot_path()], [plot_stability()],
#'   [SSJGL_select_v0_cv()], [compute_metrics()]
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
#' fit <- ssjgl(sim$data_list, penalty = "fused",
#'              lambda0 = 1, lambda1 = 1, lambda2 = 1,
#'              standardize = TRUE, maxitr.em = 50, impute = FALSE)
#' fit$v0s  # the default ladder, make_v0_ladder(lambda1 = 1)
#' print(fit)
#' adj <- extract_adjacency(fit, threshold = 0.5)
#' sum(adj[[1]][upper.tri(adj[[1]])])  # estimated edge count
ssjgl <- function(Y,penalty="fused",lambda0,lambda1,lambda2,
                  v1 = 1,
                  v0s = NULL,
                  doubly=FALSE,
                  rho=1, a=1, b =1,
                  maxitr.em=500, tol.em=1e-4, tol.pi=1e-3,
                  maxitr.jgl=500,tol.jgl=1e-5,
                  truncate=1e-5,
                  normalize=FALSE,
                  c=0.1,
                  impute=TRUE,
                  standardize=FALSE,
                  carry_pi=FALSE,
                  verbose=FALSE){
  cl <- match.call()

  # --- Input validation ---
  if (!is.list(Y) || length(Y) < 1)
    stop("Y must be a list of at least one data matrix")
  if (!all(vapply(Y, is.matrix, logical(1))))
    stop("Each element of Y must be a matrix")
  p_vals <- vapply(Y, ncol, integer(1))
  if (length(unique(p_vals)) > 1)
    stop("All matrices in Y must have the same number of columns (p)")
  penalty <- match.arg(penalty, c("fused", "group"))
  if (!is.numeric(lambda0) || length(lambda0) != 1 || lambda0 <= 0)
    stop("lambda0 must be a positive scalar")
  if (!is.numeric(lambda1) || any(lambda1 < 0))
    stop("lambda1 must be non-negative")
  if (!is.numeric(lambda2) || any(lambda2 < 0))
    stop("lambda2 must be non-negative")
  if (is.null(v0s)) {
    if (!is.numeric(lambda1) || length(lambda1) != 1 || lambda1 <= 0)
      stop("The default v0s ladder needs a positive scalar lambda1; supply v0s")
    v0s <- make_v0_ladder(lambda1)
  }
  if (!is.numeric(v0s) || any(v0s <= 0))
    stop("v0s must be a positive numeric vector")
  # every argument except the data, as used (for refits, e.g. diff_edges())
  args <- list(penalty = penalty, lambda0 = lambda0, lambda1 = lambda1,
               lambda2 = lambda2, v1 = v1, v0s = v0s, doubly = doubly,
               rho = rho, a = a, b = b, maxitr.em = maxitr.em, tol.em = tol.em,
               tol.pi = tol.pi, carry_pi = carry_pi,
               maxitr.jgl = maxitr.jgl, tol.jgl = tol.jgl, truncate = truncate,
               normalize = normalize, c = c, impute = impute,
               standardize = standardize, verbose = verbose)
  if (v1 <= 0)
    stop("v1 must be positive")
  if (a <= 0 || b <= 0)
    stop("Beta prior parameters a and b must be positive")
  if (!impute && any(vapply(Y, function(m) anyNA(m), logical(1))))
    warning("Y contains NA values but impute = FALSE. NAs will propagate.")

  # get dimensions
  p <- dim(Y[[1]])[2]
  K <- length(Y)
  n <- rep(0, K)
  for(k in 1:K) n[k] = dim(Y[[k]])[1]

  # variable and group names (restored on all outputs)
  varnames <- colnames(Y[[1]])
  if(is.null(varnames)) varnames <- paste0("V", 1:p)
  groupnames <- names(Y)

  # normalize Y? The conditional-mean imputation in getmissing() assumes
  # mean-zero data, so also centre whenever there are cells to impute.
  # Centring does not change S (cov() centres), so fits without missing
  # data are unaffected. standardize = TRUE also scales each column (within
  # each group) to unit variance.
  has_missing <- impute && any(vapply(Y, anyNA, logical(1)))
  meanj <- matrix(0, K, p)
  sdj <- matrix(1, K, p)
  if(normalize || standardize || has_missing){
    for(k in 1:K){
      for(j in 1:p){
        meanj[k, j] <- mean(Y[[k]][,j], na.rm = TRUE)
        Y[[k]][,j] = Y[[k]][,j] - meanj[k, j]
      }}
  }
  if(standardize){
    for(k in 1:K){
      sdj[k, ] <- apply(Y[[k]], 2, stats::sd, na.rm = TRUE)
      if(any(!is.finite(sdj[k, ]) | sdj[k, ] == 0))
        stop("standardize = TRUE requires every variable to have positive variance in every group")
      Y[[k]] <- sweep(Y[[k]], 2, sdj[k, ], "/")
    }
  }
  if(impute){
    klist <- ilist <- jlist <- NULL
    for(k in 1:K){
      for(i in 1:dim(Y[[k]])[1]){
        tmp <- which(is.na(Y[[k]][i, ]))
        if(length(tmp) > 0){
          klist <- c(klist, rep(k, length(tmp)))
          ilist <- c(ilist, rep(i, length(tmp)))
          jlist <- c(jlist, tmp)
        }
      }}
    if(length(klist) == 0) impute <- FALSE
    missed <- cbind(klist, ilist, jlist)
    imputed <- rep(NA, length(klist))
  }else{
    missed <- NULL
    imputed <- NULL
  }

  trace_theta <- trace_d <- NULL
  trace_prob_si <- trace_d_si <- NULL
  trace_pi1 <- trace_pi2 <- NULL
  trace_fit <- trace_prob <- NULL
  trace_itr <- trace_diff <- rep(NA, length(v0s))
  theta <- theta_last <- NULL
  d1 <- d2 <- matrix(1, p, p)
  prob1 <- matrix(a/(a+b), p, p)
  prob2 <- matrix(a/(a+b), p, p)
  diag(d1) <- diag(prob1) <- diag(d2) <- diag(prob2) <- 0
  for(k in 1:K){
    theta[[k]] <- theta_last[[k]] <- solve(cov(Y[[k]], use='complete.obs') + diag(c, p))
  }

  # Precompute base covariance matrices (constant when impute=FALSE)
  S_base <- vector("list", K)
  for(k in 1:K){
    ns_k <- dim(Y[[k]])[1]
    S_base[[k]] <- cov(Y[[k]], use='complete.obs') * (ns_k - 1) / ns_k
  }

  # Determine if we can use the C++ EM inner loop
  # C++ path handles every penalty and K; the R loop is needed only to impute
  use_cpp_em <- !impute

  # pi is kept inside [1/N, 1 - 1/N] (N = number of variable pairs), so that
  # log(pi) stays finite in the E-step; otherwise pi_xi can reach exactly 0
  # and stay there.
  n_edges <- p * (p - 1) / 2
  pi_floor <- min(1 / n_edges, 0.5)
  clamp_pi <- function(x) min(max(x, pi_floor), 1 - pi_floor)

  time <- rep(0, length(v0s))
  for(i in 1:length(v0s)){
    start_time <- Sys.time()

    # initialize parameters (pi restarts at the prior mean at every step
    # unless carry_pi = TRUE)
    if(!carry_pi || i == 1){
      pi_delta <- clamp_pi(a/(a+b))
      pi_xi <- clamp_pi(a/(a+b))
    }
    pi_delta_last <- NULL
    itr <- 1
    diff <- diff_pi <- 1
    v0 <- v0s[i]
    # re-initiate if all 0 in previous case
    tmp <- theta
    klist <- NULL
    for(k in 1:K) diag(tmp[[k]]) <- 0
    for(k in 1:K){
      theta[[k]] <- solve(cov(Y[[k]], use='complete.obs') + diag(c, p))
    }

    for(k in 1:K){
      if(sum(unlist(tmp[[k]])) != 0 && sum(diag(theta_last[[k]])) > 1){
        # bring back slab elements over median model?
        theta_last[[k]][prob1 > 0.5] <- 1
        theta[[k]][theta_last[[k]] == 0] <- 0
      }else{
        klist <- c(klist, k)
      }
    }
    theta_last <- NULL
    if(verbose && length(klist) > 0) message(paste("Re-initiated to full precision matrices for group", paste(klist, collapse=",")))

    if (use_cpp_em) {
      # === C++ EM inner loop ===
      em_result <- ssjgl_em_inner_cpp(
        S_list = S_base,
        addvar_list = NULL,
        n_vec = n,
        theta_init = theta,
        penalty = penalty,
        lambda0 = lambda0,
        lambda1 = lambda1,
        lambda2 = lambda2,
        v0 = v0, v1 = v1,
        doubly = doubly,
        rho = rho, a = a, b = b,
        maxitr_em = maxitr.em, tol_em = tol.em,
        maxitr_jgl = maxitr.jgl, tol_jgl = tol.jgl,
        truncate = truncate,
        pi_delta_init = pi_delta, pi_xi_init = pi_xi,
        pi_floor = pi_floor, tol_pi = tol.pi
      )
      theta <- em_result$theta
      prob1 <- em_result$prob1
      prob2 <- em_result$prob2
      d1 <- em_result$d1
      d2 <- em_result$d2
      pi_delta <- em_result$pi_delta
      pi_xi <- em_result$pi_xi
      itr <- em_result$itr
      if (!doubly) { d2 <- prob2 <- NULL }
      # Set theta_last so the next v0 step warm-start works correctly
      theta_last <- theta
      # Build a minimal mstep-like object for trace_fit (connected info)
      mstep <- list(theta = theta, connected = rep(TRUE, p))
      class(mstep) <- "jgl"
    } else {
      # === R EM loop (used for imputation or K>2 fused) ===
      for(itr in 1:maxitr.em){
        if(diff < tol.em && diff_pi < tol.pi) break
        # missing impute step
        if(impute){
          tmp <- getmissing(Y, theta, missed)
          YY <- tmp$YY
          addvar <- tmp$addvar
        }else{
          YY <- Y
          addvar <- NULL
        }
        # E-step
        estep <- gete(p, theta, lambda1, lambda2, v0, v1, pi_delta, pi_xi, penalty, doubly)
        d1 <- estep$d1
        d2 <- estep$d2
        prob1 <- estep$prob1
        prob2 <- estep$prob2
        diag(prob1) <- 0
        if(doubly) diag(prob2) <- 0
        # Pi update
        pi_delta <- clamp_pi((a + sum(prob1) / 2 - 1) / (a + b + n_edges - 2))
        if(doubly) pi_xi <- clamp_pi((a + sum(prob2) / 2 - 1) / (a + b + n_edges - 2))

        # M-step
        lambda1_current <- lambda1 * d1
        if(doubly){
          lambda2_current <- lambda2 * d2
        }else{
          lambda2_current <- lambda2 * d1
          d2 <- prob2 <- NULL
        }
        mstep <- JGL.adaptive(YY, addvar = addvar, penalty=penalty,lambda0=lambda0, lambda1=lambda1_current,lambda2=lambda2_current,rho=rho, maxiter=maxitr.jgl,tol=tol.jgl,warm=NULL, return.whole.theta=TRUE, truncate=truncate, normalize=FALSE)
        theta <- mstep$theta

        # compare difference
        if(!is.null(pi_delta_last)){
          diff <- 0
          for(k in 1:length(theta_last)) diff <- max(diff, max(abs(theta_last[[k]] - theta[[k]])))
          diff_pi <- abs(log(pi_delta) - log(pi_delta_last))
          if(doubly) diff_pi <- max(diff_pi, abs(log(pi_xi) - log(pi_xi_last)))
          if(verbose){
            if(doubly){
              message("Itr ", itr, "  Difference: ", round(diff,6), "  p.slab1: ", round(pi_delta, 4), "  p.slab2: ", round(pi_xi, 10))
            }else{
              message("Itr ", itr, "  Difference: ", round(diff,6), "  p.slab: ", round(pi_delta, 4))
            }
          }
        }
        pi_delta_last <- pi_delta
        pi_xi_last <- pi_xi
        theta_last <- theta
      }
    }

    trace_fit[[i]] <- mstep
    trace_prob[[i]] <- prob1
    trace_d[[i]] <- d1
    trace_prob_si[[i]] <- prob2
    trace_d_si[[i]] <- d2
    trace_theta[[i]] <- theta
    trace_pi1[[i]] <- pi_delta
    trace_pi2[[i]] <- pi_xi
    trace_itr[i] <- itr
    trace_diff <- diff
    time[i] <- as.numeric(Sys.time() - start_time, units="secs")
    if(verbose) message("Ladder step ", i, ": v0 = ", signif(v0, 5), " done (", itr, " EM iterations, ", round(time[i], 1), " s)")
  }

  # Warn when the last step selected nothing: usually the spike penalty is too
  # strong for the scale of the precision entries (see ?ssjgl, v0s).
  last_prob <- trace_prob[[length(v0s)]]
  n_sel <- sum(last_prob[upper.tri(last_prob)] > 0.5)
  if(n_sel == 0 || pi_delta <= pi_floor){
    warning(sprintf(paste0(
      "At the last ladder step %d edges have PIP > 0.5 and pi_delta = %.3g ",
      "(floor %.3g). The PIPs may have collapsed: an edge needs roughly ",
      "sum_g |theta_g| > (G log r - logit(pi)) / (r - 1), r = lambda1/v0, to ",
      "reach PIP 0.5. Consider the default ladder, standardize = TRUE, or a ",
      "ladder ending at a larger lambda1/v0. See ?ssjgl."),
      n_sel, pi_delta, pi_floor), call. = FALSE)
  }

  if(impute){
    imputed <- rep(NA, dim(missed)[1])
    for(i in 1:dim(missed)[1]){
      imputed[i] <- YY[[missed[i, 1]]][missed[i, 2], missed[i, 3]]
    }
    idx <- cbind(missed[, 1], missed[, 3])
    imputed <- imputed * sdj[idx] + meanj[idx]
  }

  # restore variable (and group) names on all p x p outputs
  name_mat <- function(m) {
    if(is.matrix(m)) dimnames(m) <- list(varnames, varnames)
    m
  }
  name_groups <- function(th) {
    th <- lapply(th, name_mat)
    names(th) <- groupnames
    th
  }
  trace_theta <- lapply(trace_theta, name_groups)
  trace_prob <- lapply(trace_prob, name_mat)
  trace_d <- lapply(trace_d, name_mat)
  if(!is.null(trace_prob_si)) trace_prob_si <- lapply(trace_prob_si, name_mat)
  if(!is.null(trace_d_si)) trace_d_si <- lapply(trace_d_si, name_mat)
  if(!is.null(missed)) colnames(missed) <- c("group", "row", "col")

  out <- list(thetalist = trace_theta, pi1list = trace_pi1, pi2list = trace_pi2, fitlist = trace_fit, itrlist = trace_itr, problist1 = trace_prob, penlist1 = trace_d, problist2 = trace_prob_si, penlist2 = trace_d_si, timelist = time,
              imputed = imputed, missed = missed,
              v0s = v0s, v1 = v1, lambda0 = lambda0, lambda1 = lambda1,
              lambda2 = lambda2, penalty = penalty, doubly = doubly,
              a = a, b = b, n = n, p = p, K = K,
              varnames = varnames, groupnames = groupnames,
              normalize = normalize, standardize = standardize,
              center = if(normalize || standardize || has_missing) meanj else NULL,
              scale = if(standardize) sdj else NULL,
              args = args, call = cl)
  class(out) = "ssjgl"
  return(out)
}
