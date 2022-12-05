#' Plot the solution path
#'
#' Plots the solution paths from ssjgl().
#'
#'
#'
#' @param v0s list of v0 input
#' @param obj object created using ssjgl
#' @param thres default threshold for probability of edge inclusion
#' @param normalize True or False (default False) on if normalized covariance should be returned
#' @param xlab String for xlab. Default empty.
#' @param ylab string for y-axis label. Default empty.
#' @param ylim default NULL and will calculate from range
#' @param main string for main title. Default empty.
#' @param par default c(1,2) to list layout of output plots
#' @param reverse Default FALSE. If want to reverse direction of plot.
#' @param position Default "bottomright". For position of legend.
#' @param ... other parameters passed in to base plot
#' @return plot
#' @export

plot_path <- function(v0s, obj, thres = 0.5, normalize = FALSE,
                      xlab="", ylab = "", main = "",
                     par = c(1, 2),
                      ylim = NULL, reverse=FALSE, position="bottomright",
                       ...){
  only = NULL
  color0 = "gray70"
  color1 = "darkblue"
  which.top = c(0,1)[2]
  cex=.5
  cex.xlab=1
  cex.ylab=1
  vline = NULL
  NV <- length(obj$thetalist)
  K <- length(obj$thetalist[[1]])
  P <- dim(obj$thetalist[[1]][[1]])[1]
  if(is.null(obj$problist1)) obj$problist1 <- obj$problist
  if(is.null(only)) only <- c(0, 1)
  if(!is.null(par)) par(mfrow = par)

  if(is.null(ylim)){
    for(k in 1:K){
      prob <- omega <- array(NA, dim = c(P, P, NV))
      for(i in 1:NV){
        prob[, , i] <- obj$problist1[[i]]
        if(normalize){
          omega[, , i] <- -cov2cor(obj$thetalist[[i]][[k]])
        }else{
          omega[, , i] <- (obj$thetalist[[i]][[k]])
        }
      }
      if(!is.null(prob)){
        for(i in 1:dim(omega)[3]){
          omega[, , i] <- omega[, , i] * (prob[, , i] >= thres)
        }
      }
      for(i in 1:dim(omega)[3]){
        diag(omega[, , i]) <- NA
      }
      ylim <- range(c(ylim, omega), na.rm=TRUE)
    }
  }

  for(k in 1:K){
    prob <- omega <- array(NA, dim = c(P, P, NV))
    for(i in 1:NV){
      prob[, , i] <- obj$problist1[[i]]
      if(normalize){
        omega[, , i] <- -cov2cor(obj$thetalist[[i]][[k]])
      }else{
        omega[, , i] <- (obj$thetalist[[i]][[k]])
      }
    }
    if(!is.null(prob)){
      for(i in 1:dim(omega)[3]){
        omega[, , i] <- omega[, , i] * (prob[, , i] >= thres)
      }
    }
    for(i in 1:dim(omega)[3]){
      diag(omega[, , i]) <- NA
    }
    xlim <- range(v0s)
    if(reverse) xlim = rev(range(v0s))
    if(is.null(ylim)) ylim <- range(omega, na.rm=T)

    plot(v0s, rep(0, length(v0s)), ylim = ylim, col = "white", xlab = xlab,
         main = main[k], ylab = ylab, xlim = xlim, ...)
    if(!is.null(vline)) abline(v = v0s[vline], col = "darkblue", lty = 2)
#browser()
   # M <- dim(G[[k]])[1]
    M <- dim(obj$thetalist[[1]][[1]])[1]


    # if(1 %in% only && which.top == 0){
    #   for(i in 2:M){
    #     for(j in 1:(i-1)){
    #       if(G[[k]][i, j] != 0){
    #         lines(v0s, omega[i, j, ], type = "b", col = color1,
    #               cex=cex, pch = 20)
    #       }
    #     }
    #   }
    # }
    #if(0 %in% only){
      for(i in 2:M){
        for(j in 1:(i-1)){
         # if(G[[k]][i, j] == 0 && sum(abs(omega[i,j,])) > 0){
            # if(G[[k]][i, j] == 0 ){
            lines(v0s, omega[i, j, ], type = "b", col = color1, lty = 4,
                  cex=cex, pch = 20)
          #}
        }
      }
    #}
    # if(1 %in% only && which.top == 1){
    #   for(i in 2:M){
    #     for(j in 1:(i-1)){
    #       if(G[[k]][i, j] != 0){
    #         lines(v0s, omega[i, j, ], type = "b", col = color1, cex=cex, pch = 20)
    #       }
    #     }
    #   }
    # }
    if(0 %in% only && 1 %in% only){
      if(position != "none") legend(position, lty = c(1, 4), pch= c(20, 20),
                                    col = c(color1, color0), c("Edge", "Non-edge"))
    }
  }
}
