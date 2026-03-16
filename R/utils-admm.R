#' ADMM iterations with adaptive (matrix-valued) penalties (internal)
#'
#' Implements ADMM for the fused or group graphical lasso M-step with
#' matrix-valued lambda1 and lambda2 penalties. Uses a C++ implementation
#' via RcppArmadillo for K=2 fused penalty (the most common case), with
#' an R fallback for K>2 fused penalty (which requires JGL:::flsa.general).
#'
#' @keywords internal
#' @noRd
admm.iters.adaptive = function(Y, lam1, lam2, penalty = "fused", rho = 1,
                                rho.increment = 1, weights, maxiter = 1000,
                                tol = 1e-5, warm = NULL) {
  K <- length(Y)
  p <- dim(Y[[1]])[2]

  # Use C++ for K=2 fused or any group penalty (most common cases)
  # Fall back to R for K>2 fused (needs JGL:::flsa.general)
  use_cpp <- !(penalty == "fused" && K > 2)

  if (use_cpp) {
    result <- admm_iters_adaptive_cpp(
      Y_list = Y,
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
    return(result)
  }

  # R fallback for K>2 fused penalty
  admm.iters.adaptive.R(Y, lam1, lam2, penalty, rho, rho.increment,
                         weights, maxiter, tol, warm)
}

#' R fallback ADMM for K>2 fused penalty (internal)
#' @keywords internal
#' @noRd
admm.iters.adaptive.R = function(Y, lam1, lam2, penalty = "fused", rho = 1,
                                  rho.increment = 1, weights, maxiter = 1000,
                                  tol = 1e-5, warm = NULL) {
  K = length(Y)
  p = dim(Y[[1]])[2]
  n = weights

  ns = c(); for(k in 1:K){ns[k] = dim(Y[[k]])[1]}
  S = list(); for(k in 1:K){S[[k]] = cov(Y[[k]])*(ns[k]-1)/ns[k]}

  theta = list()
  if(is.null(warm)){
    for(k in 1:K){
      tmp <- diag(S[[k]])
      tmp[tmp == 0] <- min(tmp[tmp > 0]) / 2
      theta[[k]] = diag(1 / tmp)
    }
  }else{
    for(k in 1:K){theta[[k]] = warm[[k]]}
  }
  Z = list(); for(k in 1:K){Z[[k]]=matrix(0,p,p)}
  W = list(); for(k in 1:K) {W[[k]] = matrix(0,p,p) }

  iter=0
  diff_value = 10
  while((iter==0) || (iter<maxiter && diff_value > tol))
  {
    theta.prev = theta
    for(k in 1:K){
      edecomp = eigen(S[[k]] - rho*Z[[k]]/n[k] + rho*W[[k]]/n[k])
      D = edecomp$values
      V = edecomp$vectors
      D2 = n[k]/(2*rho) * ( -D + sqrt(D^2 + 4*rho/n[k]) )
      theta[[k]] = V %*% diag(D2) %*% t(V)
    }

    A = list()
    for(k in 1:K){ A[[k]] = theta[[k]] + W[[k]] }
    if(penalty=="fused")
    {
      if(K==2){Z = JGL:::flsa2(A,rho,lam1,lam2,penalize.diagonal=TRUE)}
      if(K>2){Z = JGL:::flsa.general(A,rho,lam1,lam2,penalize.diagonal=TRUE)}
    }
    if(penalty=="group")
    {
      Z = JGL:::dsgl(A,rho,lam1,lam2,penalize.diagonal=TRUE)
    }

    for(k in 1:K){W[[k]] = W[[k]] + (theta[[k]]-Z[[k]])}

    iter = iter+1
    diff_value = 0
    for(k in 1:K) {diff_value = diff_value + sum(abs(theta[[k]] - theta.prev[[k]])) / sum(abs(theta.prev[[k]]))}
    rho = rho*rho.increment
  }
  diff = 0; for(k in 1:K){diff = diff + sum(abs(theta[[k]]-Z[[k]]))}
  out = list(theta=theta,Z=Z,diff=diff,iters=iter)
  return(out)
}
