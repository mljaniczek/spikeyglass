#' Bayesian Spike-and-Slab Joint Graphical Lasso
#'
#' Estimates multiple related precision matrices (inverse covariance matrices)
#' across K groups using an EM algorithm with spike-and-slab priors. The method
#' encourages shared sparsity across groups via either fused or group penalties,
#' while allowing group-specific differences through adaptive, edge-specific
#' penalization.
#'
#' The algorithm follows the dynamic posterior exploration strategy of Li et al.
#' (2019), iterating over a decreasing ladder of spike variance parameters
#' \code{v0s} with warm-starting between steps.
#'
#' @param Y List of K data matrices, each n_k x p. Missing values (\code{NA})
#'   are supported when \code{impute = TRUE}.
#' @param penalty Character: \code{"fused"} (penalizes pairwise differences
#'   between groups) or \code{"group"} (penalizes L2 norm across groups).
#' @param lambda0 Scalar penalty on diagonal entries of precision matrices.
#'   The method is relatively insensitive to this value; \code{lambda0 = 1} is
#'   recommended in most cases.
#' @param lambda1 Scalar (or matrix) base penalty on off-diagonal entries
#'   (edge-wise sparsity). The E-step produces adaptive weights that multiply
#'   this value, so the effective penalty is edge-specific. The method is
#'   relatively insensitive to the absolute value of \code{lambda1} (Li et al.,
#'   2019); what matters most is the ratio \code{lambda1/v0}. Guidance:
#'   \itemize{
#'     \item Normalized data (\code{normalize = TRUE}): use 0.01--0.1
#'     \item Raw data with moderate variance: use 0.5--1
#'     \item p >> n settings: use smaller values (0.01)
#'   }
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
#' @param v0s Numeric vector of spike variance parameters, typically
#'   \strong{decreasing}. Smaller v0 = stronger shrinkage for unlikely edges.
#'   The spike standard deviation \code{sqrt(v0)} sets the scale below which
#'   partial correlations are considered noise. For normalized data where
#'   partial correlations live in [-1, 1], \code{v0 = 0.01} (spike SD = 0.1)
#'   provides a natural noise/signal boundary. The default
#'   \code{c(0.1, 0.03, 0.01)} provides a short warm-start ladder ending at
#'   this target. See \code{vignette("parameter-exploration")} for guidance
#'   on choosing v0 and the \code{\link{make_v0_ladder}} helper for generating
#'   longer exploration ladders.
#' @param doubly Logical. If \code{TRUE}, uses doubly spike-and-slab prior
#'   with separate indicators for edge existence (delta) and cross-group
#'   similarity (xi). Default \code{FALSE}.
#' @param rho Numeric ADMM step-size parameter. Default 1.
#' @param a Numeric Beta prior shape1 for inclusion probabilities. Default 1.
#' @param b Numeric Beta prior shape2. Default 1. Setting \code{b = p} gives
#'   a sparse prior.
#' @param maxitr.em Integer max EM iterations per v0 step. Default 500.
#' @param tol.em Numeric EM convergence tolerance. Default 1e-4.
#' @param maxitr.jgl Integer max ADMM iterations for M-step. Default 500.
#' @param tol.jgl Numeric ADMM convergence tolerance. Default 1e-5.
#' @param warm List of K warm-start precision matrices. Default \code{NULL}.
#' @param warm.connected Logical vector for warm-starting block structure.
#'   Default \code{NULL}.
#' @param truncate Numeric threshold below which entries are zeroed. Default
#'   1e-5.
#' @param normalize Logical. If \code{TRUE}, mean-centers each variable.
#'   Default \code{FALSE}.
#' @param c Numeric diagonal regularization for initial precision estimate.
#'   Default 0.1.
#' @param impute Logical. If \code{TRUE}, imputes \code{NA}s via conditional
#'   MVN at each EM iteration. Default \code{TRUE}.
#'
#' @return An object of class \code{"ssjgl"}, a list with elements:
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
#'     \item{imputed}{Numeric vector of imputed values, or NULL.}
#'     \item{missed}{Matrix of (group, row, col) for missing values, or NULL.}
#'   }
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
#'              lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
#'              v0s = c(0.1, 0.03, 0.01),
#'              maxitr.em = 50, impute = FALSE)
#' print(fit)
#' adj <- extract_adjacency(fit, threshold = 0.5)
#' sum(adj[[1]][upper.tri(adj[[1]])])  # estimated edge count
ssjgl <- function(Y,penalty="fused",lambda0,lambda1,lambda2,
                  v1 = 1,
                  v0s = c(0.1, 0.03, 0.01),
                  doubly=FALSE,
                  rho=1, a=1, b =1,
                  maxitr.em=500, tol.em=1e-4,
                  maxitr.jgl=500,tol.jgl=1e-5,
                  warm=NULL, warm.connected=NULL,
                  truncate=1e-5,
                  normalize=FALSE,
                  c=0.1,
                  impute=TRUE){

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
  if (!is.numeric(v0s) || any(v0s <= 0))
    stop("v0s must be a positive numeric vector")
  if (v1 <= 0)
    stop("v1 must be positive")
  if (a <= 0 || b <= 0)
    stop("Beta prior parameters a and b must be positive")
  if (!impute && any(vapply(Y, function(m) anyNA(m), logical(1))))
    warning("Y contains NA values but impute = FALSE. NAs will propagate.")

  # decreasing v0, i.e., increasing penalty, warm start the 0 elements
  if(length(v0s) > 1){
    if(v0s[1] > v0s[2]){
      use.warm.connected <- TRUE
    }else{
      use.warm.connected <- FALSE
    }
  }else{
    use.warm.connected <- FALSE
  }

  # get dimensions
  p <- dim(Y[[1]])[2]
  K <- length(Y)
  n <- rep(0, K)
  for(k in 1:K) n[k] = dim(Y[[k]])[1]

  # normalize Y?
  meanj <- matrix(NA, K, p)
  if(normalize){
    for(k in 1:K){
      for(j in 1:p){
        meanj[k, j] <- mean(Y[[k]][,j], na.rm = TRUE)
        Y[[k]][,j] = Y[[k]][,j] - meanj[k, j]
      }}
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

  warm.connected <- NULL
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
  # C++ path handles: non-imputation case with K<=2 fused or any group penalty
  use_cpp_em <- !impute && !(penalty == "fused" && K > 2)

  time <- rep(0, length(v0s))
  for(i in 1:length(v0s)){
    start_time <- Sys.time()

    # initialize parameters
    pi_delta <- a/(a+b)
    pi_xi <- a/(a+b)
    pi_delta_last <- NULL
    itr <- 1
    diff <- 1
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
    if(length(klist) > 0) message(paste("Re-initiated to full precision matrices for group", paste(klist, collapse=",")))

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
        truncate = truncate
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
        if(diff < tol.em) break
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
        n_edges <- p * (p - 1) / 2
        pi_delta <- (a + sum(prob1) / 2 - 1) / (a + b + n_edges - 2)
        pi_xi <- (a + sum(prob2) / 2 - 1) / (a + b + n_edges - 2)

        # M-step
        lambda1_current <- lambda1 * d1
        if(doubly){
          lambda2_current <- lambda2 * d2
        }else{
          lambda2_current <- lambda2 * d1
          d2 <- prob2 <- NULL
        }
        mstep <- JGL.adaptive(YY, addvar = addvar, penalty=penalty,lambda0=lambda0, lambda1=lambda1_current,lambda2=lambda2_current,rho=rho, maxiter=maxitr.jgl,tol=tol.jgl,warm=NULL, warm.connected=NULL, return.whole.theta=TRUE, truncate=truncate, normalize=FALSE)
        theta <- mstep$theta

        # compare difference
        if(!is.null(pi_delta_last)){
          diff <- 0
          for(k in 1:length(theta_last)) diff <- max(diff, max(abs(theta_last[[k]] - theta[[k]])))
          if(doubly){
            cat(paste0("Itr ", itr, "  Difference: ", round(diff,6), "  p.slab1: ", round(pi_delta, 4), "  p.slab2: ", round(pi_xi, 10), "\n"))
          }else{
            cat(paste0("Itr ", itr, "  Difference: ", round(diff,6), "  p.slab: ", round(pi_delta, 4), "\n"))
          }
        }
        pi_delta_last <- pi_delta
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
    if(use.warm.connected) warm.connected <- mstep$connected
    time[i] <- as.numeric(Sys.time() - start_time, units="secs")
    cat(paste0("Ladder= ", i, " v0 = ", round(v0,5), " done. Time: ", round(time[i]), "\n"))
  }

  if(impute){
    imputed <- rep(NA, dim(missed)[1])
    for(i in 1:dim(missed)[1]){
      imputed[i] <- YY[[missed[i, 1]]][missed[i, 2], missed[i, 3]]
    }
    imputed <- imputed + meanj[cbind(missed[, 1], missed[, 3])]
  }
  out <- list(thetalist = trace_theta, pi1list = trace_pi1, pi2list = trace_pi2, fitlist = trace_fit, itrlist = trace_itr, problist1 = trace_prob, penlist1 = trace_d, problist2 = trace_prob_si, penlist2 = trace_d_si, timelist = time,
              imputed = imputed, missed = missed)
  class(out) = "ssjgl"
  return(out)
}
