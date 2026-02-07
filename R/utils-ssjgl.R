#'
#' This function is adapted from the JGL package (version 2.3, 2013-04-16)
#' It takes lambda1 and lambda2 as matrices
#' It also takes a lambda0 variable (scalar) for penalization of the diagonals
#' @noRd
#' @noMd

JGL.adaptive <-
  function(Y, addvar = NULL, penalty="fused",lambda0,lambda1,lambda2,rho=1,weights="sample.size", maxiter=500,tol=1e-5,warm=NULL, warm.connected=NULL, return.whole.theta=FALSE, truncate=0, normalize=FALSE)
  {
    ## initialize:
    p = dim(Y[[1]])[2]
    K = length(Y)
    n = rep(0,K)
    for(k in 1:K) {n[k] = dim(Y[[k]])[1]}

    # assign feature names if none exist:
    if(length(dimnames(Y[[1]])[[2]])==0)
    {
      for(k in 1:K)
      {
        dimnames(Y[[k]])[[2]]=paste("V",1:p,sep="")
      }
    }

    # mean-normalize Y:
    if(normalize){
      for(k in 1:K){
        for(j in 1:p){
          Y[[k]][,j] = Y[[k]][,j]-mean(Y[[k]][,j])
        }}
    }

    # set weights:
    if(length(weights)==1){if(weights == "equal"){
      weights = rep(1,K)
    }}
    if(length(weights)==1){if(weights == "sample.size"){
      weights = n #/sum(n)
    }}


    connected = rep(TRUE,p)

    ### now get criterion over connected S:
    ## define S
    S = vector("list",length=K)
    for(k in 1:K)
    {
      ntemp = dim(Y[[k]])[1]
      S[[k]] = cov(Y[[k]][,connected])*(ntemp-1)/ntemp
      if(!is.null(addvar)) S[[k]] <- S[[k]] + addvar[[k]]/ntemp
      # if warm.connected specified, make those variance to be 0.
      if(!is.null(warm.connected)){
        dd <- diag(S[[k]])
        S[[k]][, !warm.connected] <- 0
        S[[k]][!warm.connected, ] <- 0
        diag(S[[k]]) <- dd
      }
    }

    # if a penalty matrix is entered, only take its appropriate rows:
    lam1 = lambda1
    lam2 = lambda2
    if(length(lam1)>1) {lam1 = lam1[connected,connected]}
    if(length(lam2)>1) {lam2 = lam2[connected,connected]}

    # ## examine criteria:  (value 0 where S allows theta=0 to satisfy KKT; value 1 where theta must be connected)
    if(penalty=="fused"){
      if(K==2){  #use bi-conditional screening rule to identify block structure exactly
        crit1 = list()
        for(k in 1:K) { crit1[[k]] =  abs(S[[k]])*weights[k] > lam1 + lam2 }
        S.sum = matrix(0,sum(connected),sum(connected))
        for(k in 1:K) {S.sum = S.sum + weights[k]*S[[k]]}
        S.sum = abs(S.sum)
        crit2 = S.sum > 2*lam1
      }

      if(K>2){  #use sufficient screening rule to identify larger-grained block structure
        crit1 = list()
        for(k in 1:K) { crit1[[k]] =  abs(S[[k]])*weights[k] > lam1 }
        crit2 = matrix(0,sum(connected),sum(connected))
      }

      # are both criteria met?
      critboth = crit2
      for(k in 1:K) {critboth = critboth + crit1[[k]]}
      critboth = (critboth!=0)
      diag(critboth) = 1
    }

    if(penalty=="group")
    {
      ## examine criteria:  (value 0 where S allows theta=0 to satisfy KKT; value 1 where theta must be connected)
      tempsum = matrix(0,sum(connected),sum(connected))
      for(k in 1:K) {tempsum = tempsum + (pmax(weights[k]*abs(S[[k]]) - lam1,0))^2 }
      critboth = tempsum > lam2^2
      diag(critboth) = 1
    }

    ## now identify block structure using igraph:
    g1 <- igraph::graph.adjacency(critboth)
    cout = igraph::clusters(g1)
    blocklist = list()
    # identify unconnected elements, and get blocks:
    unconnected = c()

    # adapt cout$membership to start with index 1:
    if(min(cout$membership)==0){cout$membership=cout$membership+1}
    for(i in 1:(cout$no))
    {
      if(sum(cout$membership==i)==1) { unconnected <- c(unconnected,which(cout$membership==i)) }
      if(sum(cout$membership==i)>1) { blocklist[[length(blocklist)+1]] <- which(cout$membership==i) }
    }

    # final set of connected nodes
    connected[unconnected] = FALSE

    # connected indices of connected nodes:  0 for unconnected nodes, and 1:length(connected) for the rest.
    # maps features 1:p to their order in the connected features
    connected.index = rep(0,p)
    connected.index[connected] = 1:sum(connected)
    # regular indices of connected nodes: map connected nodes onto 1:p indexing:

    # redefine unconnected as !connected (up until now it's been extra nodes caught as unconnected)
    unconnected=!connected

    # unconnected <- rep(FALSE, p)
    # connected <- rep(TRUE, p)

    ## define theta on all connected:   (so theta is really theta.connected).
    theta = list()
    for(k in 1:K)
    {
      theta[[k]] = matrix(0,sum(connected),sum(connected))
      if(sum(connected)>0)
      {
        dimnames(theta[[k]])[[1]]=dimnames(theta[[k]])[[2]]=dimnames(Y[[k]])[[2]][connected]
      }
    }

    ## get solution on unconnected nodes
    # data:
    Yu = list()
    for(k in 1:K){Yu[[k]] = Y[[k]][,unconnected]}
    # penalty vectors:
    # note: for admm.iters.unconnected, we use the penalize.diagonal argument before calling the function.  for admm.iters, we use it IN the function.

    #### This works fine for scalar, vector and matrix lambda1!
    #### if(length(lambda1)==1) { lam1.unconnected = lambda1 }
    #### if(length(lambda1)>1) { lam1.unconnected = diag(lambda1)[unconnected] }
    #### if(length(lambda2)==1) { lam2.unconnected = lambda2 }
    #### if(length(lambda2)>1) { lam2.unconnected = diag(lambda2)[unconnected] }
    # if penalize.diagonal==FALSE, then set the appropriate penalty vectors to zero:
    #### if(!penalize.diagonal){
    #### 	lam1.unconnected = lam1.unconnected * 0
    #### 	if(penalty=="group") {lam2.unconnected = lam2.unconnected * 0}
    #### }
    # get the unconnected portion of theta:
    lam1.unconnected <- lambda0
    lam2.unconnected <- 0

    if(sum(unconnected)>0)
    {
      theta.unconnected = JGL:::admm.iters.unconnected(Yu,lambda1=lam1.unconnected,lambda2=lam2.unconnected,penalty=penalty,rho=rho,weights=weights,maxiter=maxiter,tol=tol)$Z
      for(k in 1:K) { names(theta.unconnected[[k]])=dimnames(Y[[k]])[[2]][!connected] }
    }
    if(sum(unconnected)==0) {theta.unconnected = NULL}

    ## now run JGL on each block of the connected nodes to fill in theta:
    if(length(blocklist)>0){
      for(i in 1:length(blocklist)){
        # the variables in the block
        bl <- blocklist[[i]]
        Ybl = list()
        # get the data on only those variables
        for(k in 1:K)
        {
          Ybl[[k]] = Y[[k]][,bl]
        }
        # penalty matrices:
        if(length(lambda1)==1) { lam1.bl = lambda1 }
        if(length(lambda1)>1) { lam1.bl = lambda1[bl,bl] }
        if(length(lambda2)==1) { lam2.bl = lambda2 }
        if(length(lambda2)>1) { lam2.bl = lambda2[bl,bl] }
        # initialize lambdas:
        lam1.bl = JGL:::penalty.as.matrix(lam1.bl,dim(Ybl[[1]])[2],penalize.diagonal=TRUE)
        diag(lam1.bl) <- lambda0

        if(penalty=="fused") {lam2.bl = JGL:::penalty.as.matrix(lam2.bl,dim(Ybl[[1]])[2],penalize.diagonal=FALSE)}
        if(penalty=="group") {lam2.bl = JGL:::penalty.as.matrix(lam2.bl,dim(Ybl[[1]])[2],penalize.diagonal=FALSE)}

        # implement warm start if desired
        if(length(warm)==0) {warm.bl = NULL}
        if(length(warm)>0)
        {
          warm.bl = list()
          for(k in 1:K) { warm.bl[[k]] = warm[[k]][bl,bl] }
        }
        # run JGL on the block:
        Thetabl = admm.iters.adaptive(Ybl,lam1.bl,lam2.bl,penalty=penalty,rho=rho,weights=weights,maxiter=maxiter,tol=tol,warm=warm.bl)
        # update Theta with Thetabl's results:
        for(k in 1:K) {theta[[k]][connected.index[bl],connected.index[bl]] = Thetabl$Z[[k]]}
      }}

    # round very small theta entries down to zero:
    if(dim(theta[[1]])[1]>0)
    {
      for(k in 1:K)
      {
        rounddown = abs(theta[[k]])<truncate; diag(rounddown)=FALSE
        theta[[k]]=theta[[k]]*(1-rounddown)
      }}

    # return output: theta on connected nodes, diagonal theta on unconnected nodes, and the identities of the connected nodes
    if(!return.whole.theta)
    {
      out = list(theta=theta,theta.unconnected=theta.unconnected,connected=connected)
    }
    if(return.whole.theta)
    {
      whole.theta = list()
      for(k in 1:K)
      {
        whole.theta[[k]] = matrix(0,p,p)
        if(sum(unconnected) > 0) diag(whole.theta[[k]])[unconnected] = theta.unconnected[[k]]
        whole.theta[[k]][connected,connected] = theta[[k]]
        dimnames(whole.theta[[k]])[[1]] = dimnames(whole.theta[[k]])[[2]] = dimnames(Y[[k]])[[2]]
      }
      out = list(theta=whole.theta,connected=connected)
    }
    class(out)="jgl"
    return(out)
  }

# MJ ADDED FUNCTIONS BELOW
# goal is to be able to add some functionality to make inference

# negative log likelihood validation
negloglik_Gaussian <- function(S, Theta) {
  Theta <- as.matrix(Theta)
  # guard: determinant can fail if not PD
  detinfo <- determinant(Theta, logarithm = TRUE)
  if (detinfo$sign <= 0) return(Inf)
  -as.numeric(detinfo$modulus) + sum(diag(S %*% Theta))
}

# precision to partial correlation
precision_to_pcor <- function(Theta) {
  Theta <- as.matrix(Theta)
  d <- diag(Theta)
  # avoid division by 0 / negative diag issues
  if (any(!is.finite(d)) || any(d <= 0)) {
    p <- nrow(Theta)
    out <- matrix(NA_real_, p, p)
    return(out)
  }
  denom <- sqrt(outer(d, d))
  P <- -Theta / denom
  diag(P) <- 1
  # symmetry cleanup
  P <- (P + t(P)) / 2
  P
}



# CV ladder selection to select "best" v0
SSJGL_select_v0_cv <- function(
    Y, v0s,
    folds = 5,
    seed = 1,
    # SSJGL args (pass-through)
    penalty,
    lambda0, lambda1, lambda2,
    v1 = 1,
    doubly=FALSE,
    rho=1, a=1, b=1,
    maxitr.em=200, tol.em=1e-4,     # reduce for CV speed by default
    maxitr.jgl=200, tol.jgl=1e-5,
    truncate=1e-5,
    normalize=FALSE,
    c=0.1,
    impute=TRUE,
    verbose = TRUE
) {
  set.seed(seed)

  K <- length(Y)
  p <- ncol(Y[[1]])

  # Build fold assignments per group (handles different n_k)
  fold_id <- vector("list", K)
  for (k in seq_len(K)) {
    nk <- nrow(Y[[k]])
    fold_id[[k]] <- sample(rep(seq_len(folds), length.out = nk))
  }

  cv_score <- rep(0, length(v0s))
  names(cv_score) <- paste0("v0=", signif(v0s, 4))

  for (f in seq_len(folds)) {

    # Split each group into train/val
    Y_train <- vector("list", K)
    Y_val   <- vector("list", K)
    S_val   <- vector("list", K)

    for (k in seq_len(K)) {
      idx_val <- which(fold_id[[k]] == f)
      idx_tr  <- which(fold_id[[k]] != f)

      Y_train[[k]] <- Y[[k]][idx_tr, , drop=FALSE]
      Y_val[[k]]   <- Y[[k]][idx_val, , drop=FALSE]

      # Validation covariance; if you used normalize=TRUE in fit,
      # it's best to center consistently here as well.
      Yv <- Y_val[[k]]
      if (normalize) {
        Yv <- scale(Yv, center = TRUE, scale = FALSE)
      }
      S_val[[k]] <- cov(Yv, use="complete.obs")  # (n-1) denom; constants don't affect selection much
    }

    # Evaluate each v0 on this fold
    for (i in seq_along(v0s)) {
      v0 <- v0s[i]

      fit_i <- tryCatch(
        ssjgl(
          Y = Y_train,
          penalty = penalty,
          lambda0 = lambda0, lambda1 = lambda1, lambda2 = lambda2,
          v1 = v1, v0s = c(v0),
          doubly = doubly,
          rho = rho, a = a, b = b,
          maxitr.em = maxitr.em, tol.em = tol.em,
          maxitr.jgl = maxitr.jgl, tol.jgl = tol.jgl,
          warm = NULL, warm.connected = NULL,
          truncate = truncate,
          normalize = normalize,
          c = c,
          impute = impute
        ),
        error = function(e) NULL
      )

      if (is.null(fit_i)) {
        cv_score[i] <- cv_score[i] + Inf
        next
      }

      thetas <- fit_i$thetalist[[1]]  # list of K precision matrices

      # Fold loss: sum across groups
      loss_fi <- 0
      for (k in seq_len(K)) {
        loss_fi <- loss_fi + negloglik_Gaussian(S_val[[k]], thetas[[k]])
      }

      cv_score[i] <- cv_score[i] + loss_fi
    }

    if (verbose) cat(sprintf("CV fold %d / %d complete\n", f, folds))
  }

  # Select best v0
  i_best <- which.min(cv_score)
  list(
    v0_best = v0s[i_best],
    i_best = i_best,
    cv_score = cv_score,
    folds = folds,
    seed = seed
  )
}


## Final function which uses best v0, and add bootstrap CIs
SSJGL_final_with_pcor_CI <- function(
    Y,
    v0_best,
    B = 200,
    ci_level = 0.95,
    seed = 1,
    # SSJGL args (pass-through)
    penalty="fused", lambda0, lambda1, lambda2,
    v1 = 1,
    doubly=FALSE,
    rho=1, a=1, b=1,
    maxitr.em=500, tol.em=1e-4,
    maxitr.jgl=500, tol.jgl=1e-5,
    truncate=1e-5,
    normalize=FALSE,
    c=0.1,
    impute=TRUE,
    verbose = TRUE
) {
  set.seed(seed)

  K <- length(Y)
  p <- ncol(Y[[1]])

  # Fit on full data at the selected v0
  fit0 <- ssjgl(
    Y = Y,
    penalty = penalty,
    lambda0 = lambda0, lambda1 = lambda1, lambda2 = lambda2,
    v1 = v1, v0s = c(v0_best),
    doubly = doubly,
    rho = rho, a = a, b = b,
    maxitr.em = maxitr.em, tol.em = tol.em,
    maxitr.jgl = maxitr.jgl, tol.jgl = tol.jgl,
    warm = NULL, warm.connected = NULL,
    truncate = truncate,
    normalize = normalize,
    c = c,
    impute = impute
  )

  theta_hat <- fit0$thetalist[[1]]  # list length K
  pcor_hat  <- lapply(theta_hat, precision_to_pcor)

  # Bootstrap storage: for each group k, store p x p x B partial correlations
  boot_pcor <- vector("list", K)
  for (k in seq_len(K)) boot_pcor[[k]] <- array(NA_real_, dim = c(p, p, B))

  bootstrap_sample_Y <- function(Y) {
    K <- length(Y)
    Yb <- vector("list", K)
    for (k in seq_len(K)) {
      nk <- nrow(Y[[k]])
      idx <- sample.int(nk, size = nk, replace = TRUE)
      Yb[[k]] <- Y[[k]][idx, , drop = FALSE]
    }
    Yb
  }

  for (b_iter in seq_len(B)) {
    Yb <- bootstrap_sample_Y(Y)

    fitb <- tryCatch(
      ssjgl(
        Y = Yb,
        penalty = penalty,
        lambda0 = lambda0, lambda1 = lambda1, lambda2 = lambda2,
        v1 = v1, v0s = c(v0_best),
        doubly = doubly,
        rho = rho, a = a, b = b,
        maxitr.em = maxitr.em, tol.em = tol.em,
        maxitr.jgl = maxitr.jgl, tol.jgl = tol.jgl,
        warm = NULL, warm.connected = NULL,
        truncate = truncate,
        normalize = normalize,
        c = c,
        impute = impute
      ),
      error = function(e) NULL
    )

    if (!is.null(fitb)) {
      thetab <- fitb$thetalist[[1]]
      for (k in seq_len(K)) {
        boot_pcor[[k]][,,b_iter] <- precision_to_pcor(thetab[[k]])
      }
    }

    if (verbose) cat(sprintf("Bootstrap %d / %d complete\n", b_iter, B))
  }

  # Entrywise percentile CI
  alpha <- 1 - ci_level
  lo_q <- alpha / 2
  hi_q <- 1 - alpha / 2

  CI_lower <- vector("list", K)
  CI_upper <- vector("list", K)

  for (k in seq_len(K)) {
    arr <- boot_pcor[[k]]  # p x p x B
    lo <- matrix(NA_real_, p, p)
    hi <- matrix(NA_real_, p, p)

    for (r in seq_len(p)) {
      for (c2 in seq_len(p)) {
        vals <- arr[r, c2, ]
        vals <- vals[is.finite(vals)]
        if (length(vals) < 10) {  # too few successful bootstraps
          lo[r, c2] <- NA_real_
          hi[r, c2] <- NA_real_
        } else {
          lo[r, c2] <- as.numeric(quantile(vals, probs = lo_q, names = FALSE, type = 8))
          hi[r, c2] <- as.numeric(quantile(vals, probs = hi_q, names = FALSE, type = 8))
        }
      }
    }

    # symmetry cleanup, diagonal fixed at 1
    lo <- (lo + t(lo)) / 2; diag(lo) <- 1
    hi <- (hi + t(hi)) / 2; diag(hi) <- 1

    CI_lower[[k]] <- lo
    CI_upper[[k]] <- hi
  }

  list(
    v0_best = v0_best,
    fit = fit0,                 # single-ladder-step SSJGL fit
    theta_hat = theta_hat,      # list of K precision matrices
    pcor_hat = pcor_hat,        # list of K partial-corr matrices
    CI_lower = CI_lower,        # list of K lower CI matrices (pcor)
    CI_upper = CI_upper,        # list of K upper CI matrices (pcor)
    boot_pcor = boot_pcor,      # optional: store bootstrap distribution
    B = B,
    ci_level = ci_level,
    seed = seed
  )
}

# final function putting it all together
SSJGL_CV_final_pcorCI <- function(
    Y,
    v0s,
    folds = 5,
    B = 200,
    ci_level = 0.95,
    seed = 1,
    # SSJGL args
    penalty="fused", lambda0, lambda1, lambda2,
    v1 = 1,
    doubly=FALSE,
    rho=1, a=1, b=1,
    # CV iteration controls (usually smaller)
    maxitr.em.cv=200, tol.em=1e-4,
    maxitr.jgl.cv=200, tol.jgl=1e-5,
    # final fit iteration controls
    maxitr.em=500, maxitr.jgl=500,
    truncate=1e-5,
    normalize=FALSE,
    c=0.1,
    impute=TRUE,
    verbose = TRUE
) {
  cv <- SSJGL_select_v0_cv(
    Y = Y, v0s = v0s, folds = folds, seed = seed,
    penalty = penalty, lambda0 = lambda0, lambda1 = lambda1, lambda2 = lambda2,
    v1 = v1, doubly = doubly, rho = rho, a = a, b = b,
    maxitr.em = maxitr.em.cv, tol.em = tol.em,
    maxitr.jgl = maxitr.jgl.cv, tol.jgl = tol.jgl,
    truncate = truncate, normalize = normalize, c = c, impute = impute,
    verbose = verbose
  )

  if (verbose) {
    cat(sprintf("Selected v0 = %g (index %d)\n", cv$v0_best, cv$i_best))
  }

  final <- SSJGL_final_with_pcor_CI(
    Y = Y, v0_best = cv$v0_best,
    B = B, ci_level = ci_level, seed = seed,
    penalty = penalty, lambda0 = lambda0, lambda1 = lambda1, lambda2 = lambda2,
    v1 = v1, doubly = doubly, rho = rho, a = a, b = b,
    maxitr.em = maxitr.em, tol.em = tol.em,
    maxitr.jgl = maxitr.jgl, tol.jgl = tol.jgl,
    truncate = truncate, normalize = normalize, c = c, impute = impute,
    verbose = verbose
  )

  list(
    cv = cv,
    final = final
  )
}

# example:
# res <- SSJGL_CV_final_pcorCI(
#   Y = Y,
#   v0s = seq(0.01, 0.0005, length.out = 10),
#   folds = 5,
#   B = 200,
#   ci_level = 0.95,
#   seed = 123,
#   penalty = "fused",
#   lambda0 = 0.1, lambda1 = 0.2, lambda2 = 0.2,
#   normalize = TRUE,
#   impute = TRUE
# )
#
# # Selected ladder step
# res$cv$v0_best
# res$cv$cv_score
#
# # Final partial correlation estimate for group k=1
# pcor_hat_g1 <- res$final$pcor_hat[[1]]
#
# # 95% CI matrices for partial correlations (group 1)
# pcor_lo_g1 <- res$final$CI_lower[[1]]
# pcor_hi_g1 <- res$final$CI_upper[[1]]




