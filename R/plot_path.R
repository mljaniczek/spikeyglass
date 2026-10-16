#' Plot the solution path
#'
#' Plots each off-diagonal precision entry (or, with \code{normalize = TRUE},
#' partial correlation) against v0 along the ladder, one panel per group.
#' Entries whose PIP is below \code{thres} at a step are drawn as 0.
#'
#' @param v0s The v0 values of the fit (\code{obj$v0s}).
#' @param obj An \code{ssjgl} object.
#' @param thres PIP threshold. Default 0.5.
#' @param normalize Logical. If \code{TRUE}, plot partial correlations
#'   instead of precision entries. Default \code{FALSE}.
#' @param xlab String for xlab. Default empty.
#' @param ylab string for y-axis label. Default empty.
#' @param ylim default NULL and will calculate from range
#' @param main string for main title. Default empty.
#' @param par Layout \code{c(rows, cols)} for the K panels, passed to
#'   \code{par(mfrow = )} and restored on exit. Default \code{c(1, 2)};
#'   \code{NULL} leaves the layout alone.
#' @param reverse Default FALSE. If want to reverse direction of plot.
#' @param position Legend position, or \code{"none"}. Default
#'   \code{"bottomright"}.
#' @param ... other parameters passed in to base plot
#' @return Invisible NULL. Called for side effect (plot).
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 8, n = 100, seed = 42)
#' fit <- ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
#'              v0s = make_v0_ladder(1, n_steps = 5, min_ratio = 5, max_ratio = 200),
#'              standardize = TRUE, maxitr.em = 50, impute = FALSE)
#' plot_path(fit$v0s, fit, thres = 0.5,
#'           xlab = expression(v[0]), ylab = "Precision entries",
#'           main = c("Group 1", "Group 2"))

plot_path <- function(v0s, obj, thres = 0.5, normalize = FALSE,
                      xlab="", ylab = "", main = "",
                      par = c(1, 2),
                      ylim = NULL, reverse=FALSE, position="bottomright",
                      ...){
  col_edge <- "darkblue"
  NV <- length(obj$thetalist)
  K <- length(obj$thetalist[[1]])
  P <- nrow(obj$thetalist[[1]][[1]])
  if (is.null(obj$problist1)) obj$problist1 <- obj$problist
  if (length(main) < K) main <- rep(main, length.out = K)

  # entries of group k along the ladder, set to 0 where PIP < thres
  path_k <- function(k) {
    omega <- array(NA_real_, dim = c(P, P, NV))
    for (i in seq_len(NV)) {
      th <- obj$thetalist[[i]][[k]]
      omega[, , i] <- if (normalize) -stats::cov2cor(th) else th
      omega[, , i] <- omega[, , i] * (obj$problist1[[i]] >= thres)
      diag(omega[, , i]) <- NA
    }
    omega
  }
  paths <- lapply(seq_len(K), path_k)
  if (is.null(ylim)) ylim <- range(unlist(paths), na.rm = TRUE)
  xlim <- if (reverse) rev(range(v0s)) else range(v0s)

  if (!is.null(par)) {
    old <- graphics::par(mfrow = par)
    on.exit(graphics::par(old))
  }
  for (k in seq_len(K)) {
    omega <- paths[[k]]
    graphics::plot(v0s, rep(0, length(v0s)), ylim = ylim, col = "white",
                   xlab = xlab, main = main[k], ylab = ylab, xlim = xlim, ...)
    for (i in 2:P) {
      for (j in 1:(i - 1)) {
        graphics::lines(v0s, omega[i, j, ], type = "b", col = col_edge,
                        lty = 4, cex = 0.5, pch = 20)
      }
    }
    if (position != "none") {
      graphics::legend(position, lty = 4, pch = 20, col = col_edge, bty = "n",
                       legend = sprintf("entry (zero if PIP < %g)", thres))
    }
  }
  invisible(NULL)
}
