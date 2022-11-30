#' getmetric
#'
#' @param est ?
#' @param truth ?
#' @param graph ?
#'
#' @return Summary of true positive, false positives, true negatives etc
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
