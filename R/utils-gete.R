#' E-step for spike-and-slab prior (internal)
#'
#' Computes posterior probabilities of edge inclusion (delta) and the
#' adaptive penalty weights for the M-step. Implements the E-step from
#' Li et al. (2019), equations 9-10.
#'
#' For the single spike-and-slab case, computes P(delta=1|Omega, pi_delta)
#' for each edge (j,k). For the doubly spike-and-slab case, dispatches to
#' \code{gete.doubly} which computes three-way probabilities.
#'
#' A penalty with \code{lambda = 0} contributes nothing to the prior, so its
#' terms are omitted (otherwise \code{log(0)} makes every PIP 0).
#' @keywords internal
#' @noRd

gete <- function(p, theta, lambda1, lambda2, v0, v1, pi_delta, pi_xi, penalty, doubly){
  if(doubly) return(gete.doubly(p, theta, lambda1, lambda2, v0, v1, pi_delta, pi_xi, penalty))
  # this function assumes sum_{g1 < g2}
  G <- length(theta)
  pen <- matrix(0, p, p)
  if(penalty == "fused"){
    for(i in 1:(G-1)){
      for(j in (i+1):G){
        pen <- pen + abs(theta[[i]] - theta[[j]])
      }
    }
    const <- G *(G-1) / 2
  }else if(penalty == "group"){
    for(i in 1:length(theta)){
      pen <- pen + theta[[i]]^2
    }
    pen <- sqrt(pen)
    const <- 1
  }else{
    stop("Penalty needs to be fused or group.")
  }

  abssum <- matrix(0, p, p)
  for(i in 1:G){
    abssum <- abssum + abs(theta[[i]])
  }

  logp0 <- laplace_logprior(lambda1, v0, abssum, G) + laplace_logprior(lambda2, v0, pen, const) + log(1-pi_delta)
  logp1 <- laplace_logprior(lambda1, v1, abssum, G) + laplace_logprior(lambda2, v1, pen, const) + log(pi_delta)

  # log-sum-exp trick
  mx <- pmax(logp0, logp1)
  logz <- mx + log(exp(logp0 - mx) + exp(logp1 - mx))
  prob <- exp(logp1 - logz)
  prob[is.nan(prob)] <- 0
  d <- (1-prob)/v0 + prob/v1
  d[is.nan(d)] <- 1/v0
  diag(prob) <- 0
  diag(d) <- 0

  d <- (d + t(d)) / 2
  prob <- (prob + t(prob)) / 2
  out <- list(d1 = d, prob1 = prob, d2=0, prob2 = 0)
  return(out)
}

gete.doubly <- function(p, theta, lambda1, lambda2, v0, v1, pi_delta, pi_xi, penalty){
  # this function assumes sum_{g1 < g2}
  G <- length(theta)
  pen <- matrix(0, p, p)
  if(penalty == "fused"){
    for(i in 1:(G-1)){
      for(j in (i+1):G){
        pen <- pen + abs(theta[[i]] - theta[[j]])
      }
    }
    const <- G *(G-1) / 2
  }else if(penalty == "group"){
    for(i in 1:length(theta)){
      pen <- pen + theta[[i]]^2
    }
    pen <- sqrt(pen)
    const <- 1
  }else{
    stop("Penalty needs to be fused or group.")
  }

  abssum <- matrix(0, p, p)
  for(i in 1:G){
    abssum <- abssum + abs(theta[[i]])
  }

  prob <- array(NA, dim=c(p,p,3))

  # 00
  prob[,,1] <- laplace_logprior(lambda1, v0, abssum, G) + laplace_logprior(lambda2, v0, pen, const) + log(1-pi_delta) + log(1-pi_xi)
  # 10
  prob[,,2] <- laplace_logprior(lambda1, v1, abssum, G) + laplace_logprior(lambda2, v0, pen, const) + log(pi_delta) + log(1-pi_xi)
  # 11
  prob[,,3] <- laplace_logprior(lambda1, v1, abssum, G) + laplace_logprior(lambda2, v1, pen, const) + log(pi_delta) + log(pi_xi)

  # z <- apply(prob, c(1,2), function(x) sum(exp(x)))
  # for(i in 1:3) prob[,,i] <- exp(prob[,,i]) / z

  # log-sum-exp trick
  mx <- apply(prob, c(1, 2), max)
  prob2 <- prob
  prob2[,,1] <- prob[,,1] - mx
  prob2[,,2] <- prob[,,2] - mx
  prob2[,,3] <- prob[,,3] - mx
  logz <- log(apply(prob2, c(1, 2), function(x){sum(exp(x))})) + mx
  for(i in 1:3) prob[,,i] <- exp(prob[,,i] - logz)


  prob1 <- prob[,,2] + prob[,,3]
  prob2 <- prob[,,3]
  # Replace NaN with 0 (can arise from extreme log-probabilities)
  prob1[is.nan(prob1)] <- 0
  prob2[is.nan(prob2)] <- 0
  d1 <- (1-prob1)/v0 + prob1/v1
  d2 <- (1-prob2)/v0 + prob2/v1
  d1[is.nan(d1)] <- 1/v0
  d2[is.nan(d2)] <- 1/v0
  diag(prob1) <- diag(prob2) <- 0
  diag(d1) <- diag(d2) <- 0

  d1 <- (d1 + t(d1)) / 2
  prob1 <- (prob1 + t(prob1)) / 2
  d2 <- (d2 + t(d2)) / 2
  prob2 <- (prob2 + t(prob2)) / 2
  out <- list(d1 = d1, prob1 = prob1, d2=d2, prob2 = prob2)

  return(out)
}

# Log of the Laplace prior kernel (lambda/v)^cnt * exp(-lambda/v * x). With
# lambda = 0 the penalty is absent, so the term is dropped rather than giving
# log(0) = -Inf in every mixture component.
laplace_logprior <- function(lambda, v, x, cnt){
  if(all(lambda == 0)) return(0)
  -lambda/v * x + log(lambda/v) * cnt
}

# missed has three cols: k, i, j
getmissing <- function(Y, theta, missed){
  YY <- Y
  addvar <- NULL
  for(i in 1:length(Y)){
    addvar[[i]] <- matrix(0, dim(Y[[i]])[2], dim(Y[[i]])[2])
  }
  for(k in unique(missed[,1])){
    tmp <- missed[missed[,1] == k, , drop = FALSE]
    cov <- solve(theta[[k]])

    for(i in unique(tmp[, 2])){
      jj <- tmp[tmp[,2] == i, 3]
      oldy <- Y[[k]][i, -jj]
      tmpcov = solve(cov[-jj, -jj])
      newy = as.numeric(t(oldy) %*% t(cov[jj, -jj] %*% tmpcov))
      YY[[k]][i, jj] <- newy
      addvar[[k]][jj, jj] <- addvar[[k]][jj, jj] + cov[jj, jj] - cov[jj, -jj] %*% tmpcov %*% cov[-jj,jj]
    }
  }
  return(list(YY=YY, addvar = addvar))
}
