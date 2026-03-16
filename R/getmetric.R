#' Compute graph recovery metrics
#'
#' Compares an estimated precision matrix to the truth, computing edge-level
#' metrics (TP, FP, TN, FN), sum of squared errors, L1 norm, and
#' Kullback-Leibler divergence.
#'
#' @param est Estimated precision matrix (p x p).
#' @param truth True precision matrix (p x p).
#' @param graph Binary adjacency matrix of the true graph (p x p, 0 diagonal).
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{SSE}{Sum of squared errors between est and truth (off-diagonal).}
#'     \item{L1}{L1 norm of the estimated precision (off-diagonal).}
#'     \item{nedges}{Number of non-zero edges in the estimate.}
#'     \item{tp}{True positives (edges correctly identified).}
#'     \item{fp}{False positives (spurious edges).}
#'     \item{fn}{False negatives (missed edges).}
#'     \item{tn}{True negatives (correctly absent edges).}
#'     \item{dKL}{Kullback-Leibler divergence KL(est || truth).}
#'   }
#' @export

getmetric <- function(est, truth, graph){
  out <- NULL
  gtmp <- graph
  tmp <- est
  tmp0 <- truth
  diag(tmp) <- diag(tmp0) <- diag(gtmp) <- NA
  out$SSE<- sum((tmp-tmp0)^2, na.rm=T)
  out$L1 <- sum(abs(tmp), na.rm=T)

  tmp[tmp!=0] <- 1
  tmp0[tmp0!=0] <- 1
  diag(tmp) <- diag(tmp0) <- diag(gtmp) <- NA
  out$nedges <- sum(tmp != 0, na.rm=T)/2
  out$tp <- sum(tmp * gtmp > 0, na.rm=T)/2
  out$fp <- sum(tmp * (1-gtmp) > 0, na.rm=T)/2
  out$fn <- sum((1-tmp) * gtmp > 0, na.rm=T)/2
  out$tn <- sum((1-tmp) * (1-gtmp) > 0, na.rm=T)/2
  out$dKL <- 0.5 * (-log(det(est %*% solve(truth))) + sum(diag(est %*% solve(truth))))
  return(out)
}
