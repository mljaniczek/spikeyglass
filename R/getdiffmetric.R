#' getdiffmetric
#'
#' @param est ?
#' @param truth ?
#' @param graph ?
#' @param tol ?
#'
#' @return Summary of true positive, false positives, true negatives etc
#' @export

getdiffmetric <- function(est, truth, graph, tol){
  out <- NULL
  out$tp.diff=out$fp.diff=out$fn.diff=0
  out$tp.gdiff=out$fp.gdiff=out$fn.gdiff=0
  G <- length(est)
  for(i in 1:(G-1)){
    for(j in (i+1) : G){
      tmp.a <- est[[i]]
      tmp.a[tmp.a!=0] <- 1
      gtmp.a <- graph[[i]]
      tmp.b <- est[[j]]
      tmp.b[tmp.b!=0] <- 1
      gtmp.b <- graph[[j]]
      diag(tmp.a) <- diag(tmp.b) <- diag(gtmp.a) <- diag(gtmp.b) <- NA

      out$tp.gdiff <- out$tp.gdiff+sum((tmp.a != tmp.b) * (gtmp.a != gtmp.b), na.rm=T)/2
      out$fp.gdiff <- out$fp.gdiff+sum((tmp.a != tmp.b) * (gtmp.a == gtmp.b), na.rm=T)/2
      out$fn.gdiff <- out$fn.gdiff+sum((tmp.a == tmp.b) * (gtmp.a != gtmp.b), na.rm=T)/2
    }
  }
  for(i in 1:(G-1)){
    for(j in (i+1) : G){
      tmp.a <- est[[i]]
      gtmp.a <- truth[[i]]
      tmp.b <- est[[j]]
      gtmp.b <- truth[[j]]
      diag(tmp.a) <- diag(tmp.b) <- diag(gtmp.a) <- diag(gtmp.b) <- NA
      diff <- abs(tmp.a - tmp.b) > tol
      tdiff <- abs(gtmp.a - gtmp.b) > tol

      out$tp.diff <- out$tp.diff+sum(diff * tdiff, na.rm=T)/2
      out$fp.diff <- out$fp.diff+sum(diff * (1-tdiff), na.rm=T)/2
      out$fn.diff <- out$fn.diff+sum((1-diff) * tdiff, na.rm=T)/2
    }
  }
  return(out)
}
