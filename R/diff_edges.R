# Differential-edge inference between two groups of an ssjgl fit

#' Differential edges between two groups
#'
#' Tests which edges differ between two groups of an \code{\link{ssjgl}} fit,
#' by refitting \code{ssjgl()} with the same settings on permuted or
#' bootstrapped data.
#'
#' @param fit An \code{ssjgl} object.
#' @param groups Two distinct group indices (or group names) to compare. The
#'   difference is group 1 minus group 2.
#' @param score Scale of the comparison: \code{"pcor"} (partial correlations,
#'   default) or \code{"precision"} (precision-matrix entries).
#' @param method \describe{
#'   \item{\code{"permutation"}}{(default) Rows of the two groups are pooled
#'     and the group labels shuffled (group sizes kept; any other groups are
#'     left as they are), and \code{ssjgl()} is refitted \code{B} times. The
#'     null scores \eqn{|\Delta|} are pooled across all edges and permutations.
#'     Each edge's p-value is \eqn{(1 + \#\{null \ge |\Delta_{obs}|\}) /
#'     (1 + \#null)}, and the p-values are adjusted by Benjamini-Hochberg.
#'     \code{selected} is \code{p_adj < alpha}.}
#'   \item{\code{"bootstrap"}}{Rows are resampled with replacement within
#'     every group, \code{ssjgl()} is refitted \code{B} times, and
#'     \code{lower}/\code{upper} are percentile \eqn{1 - \alpha} intervals for
#'     \eqn{\Delta}. \code{selected} means the interval excludes 0. This gives
#'     no multiplicity control.}
#' }
#' @param B Number of permutations or bootstrap resamples. Default 100.
#' @param alpha BH level (permutation) or 1 - interval coverage (bootstrap).
#'   Default 0.05.
#' @param Y The list of data matrices \code{fit} was estimated from. If
#'   \code{NULL} (default), the \code{Y} argument of \code{fit$call} is
#'   evaluated in the calling environment; supply it explicitly if that object
#'   has changed or is not available.
#' @param v0_index Ladder step whose estimates are compared. Default: the last
#'   step.
#' @param ... Arguments passed to the \code{ssjgl()} refits, overriding the
#'   settings stored in \code{fit} (e.g. \code{maxitr.em} to make the refits
#'   faster).
#'
#' @details
#' Refits use the arguments stored in \code{fit$args}, so they repeat the
#' whole v0 ladder of the original fit. The resampling indices are drawn in
#' the calling R session, so results are reproducible with
#' \code{set.seed()} and do not depend on how the refits are run.
#'
#' \strong{Parallel refits.} If the \pkg{future.apply} package is installed,
#' the refits are run with \code{future.apply::future_lapply()}, so they are
#' parallel under any \code{future::plan()} other than sequential, e.g.
#' \code{future::plan(future::multisession, workers = 4)}.
#'
#' \strong{\code{prob_dissimilar}.} With \code{doubly = TRUE} the table also
#' has \code{prob_dissimilar}, the EM's posterior probability
#' \eqn{P(\xi_{jk} = 1)} that edge \eqn{(j, k)} is in the slab of the
#' cross-group similarity penalty, i.e. is only weakly shrunk towards equality
#' across groups. It is shared by all groups, comes from the point-estimating
#' EM, and is \emph{not} a calibrated probability that the edge differs
#' between \code{groups}. In practice it can be uninformative: in one real
#' fit (TCGA PI3K pathway, p = 28, group penalty), every edge with
#' \eqn{P(\delta = 1) > 0.5} also had \eqn{P(\xi = 1) > 0.5}. Use the
#' permutation p-values for inference.
#'
#' \strong{Column names.} Where the concepts match, the columns follow
#' multiGGMr's \code{diff_summary()} and \code{select_diff_edges()}
#' (\code{i}, \code{j}, \code{lower}, \code{upper}), so the shared columns of
#' the two packages' tables can be bound together.
#'
#' @return A data frame of class \code{"ssjgl_diff_edges"}, one row per edge
#'   \eqn{i < j} in \code{upper.tri()} order, with columns
#'   \describe{
#'     \item{i, j}{Variable indices.}
#'     \item{node1, node2}{Variable names.}
#'     \item{est_g1, est_g2}{Estimates in the two groups on the \code{score}
#'       scale.}
#'     \item{diff}{\code{est_g1 - est_g2}.}
#'     \item{pvalue, p_adj}{Permutation p-value and its BH adjustment
#'       (\code{method = "permutation"}).}
#'     \item{lower, upper}{Bootstrap percentile interval for \code{diff}
#'       (\code{method = "bootstrap"}).}
#'     \item{selected}{Logical: called differential.}
#'     \item{prob_dissimilar}{\eqn{P(\xi = 1)} (only if the fit is doubly).}
#'   }
#'   Attributes: \code{method}, \code{score}, \code{groups},
#'   \code{group_names}, \code{B} (successful refits), \code{alpha},
#'   \code{v0_index}, and \code{null_scores} (the pooled null, permutation)
#'   or \code{boot_diffs} (a \code{B} x edges matrix, bootstrap).
#'
#' @seealso [ssjgl()], [plot.ssjgl_diff_edges()]
#' @export
#'
#' @examples
#' \donttest{
#' sim <- simulate_ssjgl_data(K = 2, p = 8, n = 60, seed = 1)
#' fit <- ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
#'              standardize = TRUE, impute = FALSE)
#' set.seed(1)
#' de <- diff_edges(fit, Y = sim$data_list, B = 20)
#' de
#' plot(de)
#' }
diff_edges <- function(fit, groups = c(1, 2), score = c("pcor", "precision"),
                       method = c("permutation", "bootstrap"), B = 100,
                       alpha = 0.05, Y = NULL, v0_index = NULL, ...) {
  if (!inherits(fit, "ssjgl")) stop("fit must be an ssjgl object")
  if (is.null(fit$args))
    stop("fit has no stored arguments (fit$args); refit it with this version of spikeyglass")
  score <- match.arg(score)
  method <- match.arg(method)
  B <- as.integer(B)
  if (length(B) != 1 || is.na(B) || B < 1) stop("B must be a positive integer")
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("alpha must be a single number in (0, 1)")
  groups <- .check_diff_groups(groups, fit)
  n_v0 <- length(fit$thetalist)
  if (is.null(v0_index)) v0_index <- n_v0
  if (!(v0_index %in% seq_len(n_v0))) stop("v0_index must be in 1..", n_v0)

  if (is.null(Y)) {
    Y <- tryCatch(eval(fit$call$Y, parent.frame()), error = function(e) NULL)
    if (is.null(Y)) stop("Could not recover the data from fit$call; supply Y")
  }
  if (!is.list(Y) || length(Y) != fit$K ||
      !identical(as.numeric(vapply(Y, nrow, integer(1))), as.numeric(fit$n)) ||
      ncol(Y[[1]]) != fit$p)
    stop("Y does not match the dimensions of fit (K, n, p)")

  # Observed table
  out <- .diff_table(fit$thetalist[[v0_index]], groups, score, fit$varnames)
  if (isTRUE(fit$doubly) && !is.null(fit$problist2)) {
    pr2 <- fit$problist2[[v0_index]]
    out$prob_dissimilar <- unname(pr2[upper.tri(pr2)])
  }

  refit_args <- utils::modifyList(fit$args, list(...))
  g1 <- groups[1]; g2 <- groups[2]
  n1 <- nrow(Y[[g1]]); n2 <- nrow(Y[[g2]])

  # Draw all resampling indices here, so results depend only on the RNG
  # state of the calling session (not on how the refits are scheduled).
  if (method == "permutation") {
    idx <- lapply(seq_len(B), function(b) sample.int(n1 + n2))
    make_Y <- function(ix) {         # ix: a permutation of the pooled rows
      pooled <- rbind(Y[[g1]], Y[[g2]])
      Yb <- Y
      Yb[[g1]] <- pooled[ix[seq_len(n1)], , drop = FALSE]
      Yb[[g2]] <- pooled[ix[n1 + seq_len(n2)], , drop = FALSE]
      Yb
    }
  } else {
    idx <- lapply(seq_len(B), function(b)
      lapply(Y, function(m) sample.int(nrow(m), replace = TRUE)))
    make_Y <- function(ix) Map(function(m, r) m[r, , drop = FALSE], Y, ix)  # ix: rows per group
  }

  one_refit <- function(ix) {
    f <- NULL
    res <- tryCatch({
      utils::capture.output(f <- suppressMessages(
        do.call(ssjgl, c(list(Y = make_Y(ix)), refit_args))))
      th <- f$thetalist[[min(v0_index, length(f$thetalist))]]
      .diff_table(th, groups, score, NULL)$diff
    }, error = function(e) NULL)
    res
  }

  if (requireNamespace("future.apply", quietly = TRUE)) {
    diffs <- future.apply::future_lapply(idx, one_refit, future.seed = NULL,
                                         future.packages = "spikeyglass")
  } else {
    diffs <- lapply(idx, one_refit)
  }
  ok <- !vapply(diffs, is.null, logical(1))
  if (!any(ok)) stop("All refits failed")
  if (!all(ok)) warning(sum(!ok), " of ", B, " refits failed and were dropped")
  D <- do.call(rbind, diffs[ok])

  if (method == "permutation") {
    pp <- .pooled_perm_pvalues(abs(out$diff), abs(D))
    out$pvalue <- pp$pvalue
    out$p_adj <- pp$p_adj
    out$selected <- out$p_adj < alpha
    attr(out, "null_scores") <- abs(as.vector(D))
  } else {
    q <- apply(D, 2, stats::quantile, probs = c(alpha / 2, 1 - alpha / 2),
               names = FALSE, type = 7)
    out$lower <- q[1, ]
    out$upper <- q[2, ]
    out$selected <- out$lower > 0 | out$upper < 0
    attr(out, "boot_diffs") <- D
  }

  cols <- c("i", "j", "node1", "node2", "est_g1", "est_g2", "diff",
            if (method == "permutation") c("pvalue", "p_adj") else c("lower", "upper"),
            "selected", if (!is.null(out$prob_dissimilar)) "prob_dissimilar")
  attrs <- attributes(out)[c("null_scores", "boot_diffs")]
  out <- out[, cols]
  for (a in names(attrs)) if (!is.null(attrs[[a]])) attr(out, a) <- attrs[[a]]
  attr(out, "method") <- method
  attr(out, "score") <- score
  attr(out, "groups") <- groups
  attr(out, "group_names") <- if (!is.null(fit$groupnames)) fit$groupnames[groups] else
    paste("group", groups)
  attr(out, "B") <- sum(ok)
  attr(out, "alpha") <- alpha
  attr(out, "v0_index") <- v0_index
  class(out) <- c("ssjgl_diff_edges", "data.frame")
  out
}

# Groups may be given as indices or names
.check_diff_groups <- function(groups, fit) {
  if (is.character(groups)) {
    if (is.null(fit$groupnames)) stop("fit has no group names; give groups as indices")
    groups <- match(groups, fit$groupnames)
  }
  groups <- as.integer(groups)
  if (length(groups) != 2L || anyNA(groups) || groups[1] == groups[2] ||
      any(groups < 1L | groups > fit$K))
    stop(sprintf("groups must be two distinct group indices in 1..%d", fit$K))
  groups
}

# Upper-triangle edge table of the two groups' estimates
.diff_table <- function(theta, groups, score, varnames) {
  est <- lapply(theta[groups], function(m) {
    m <- unname(as.matrix(m))
    if (score == "pcor") precision_to_pcor(m) else m
  })
  p <- nrow(est[[1]])
  ut <- upper.tri(est[[1]])
  ij <- which(ut, arr.ind = TRUE)
  if (is.null(varnames)) varnames <- paste0("V", seq_len(p))
  data.frame(i = ij[, 1], j = ij[, 2],
             node1 = varnames[ij[, 1]], node2 = varnames[ij[, 2]],
             est_g1 = est[[1]][ut], est_g2 = est[[2]][ut],
             diff = est[[1]][ut] - est[[2]][ut],
             stringsAsFactors = FALSE)
}

# Pooled permutation p-values: each observed score is compared with the null
# scores of all edges and permutations; p = (1 + #null >= obs) / (1 + #null).
.pooled_perm_pvalues <- function(obs, null) {
  pool <- sort(as.vector(null)[is.finite(as.vector(null))])
  n_lt <- findInterval(obs - sqrt(.Machine$double.eps) * pmax(1, abs(obs)), pool)
  pvalue <- (1 + length(pool) - n_lt) / (1 + length(pool))
  list(pvalue = pvalue, p_adj = stats::p.adjust(pvalue, method = "BH"))
}

#' @rdname diff_edges
#' @param x An \code{ssjgl_diff_edges} object.
#' @param n Number of selected edges to print. Default 10.
#' @export
print.ssjgl_diff_edges <- function(x, n = 10, ...) {
  gn <- attr(x, "group_names")
  method <- attr(x, "method")
  cat("<ssjgl_diff_edges>\n")
  cat(sprintf("  %s vs %s, %s scale, %s (B = %d), v0 step %d\n", gn[1], gn[2],
              attr(x, "score"), method, attr(x, "B"), attr(x, "v0_index")))
  if (method == "permutation") {
    cat(sprintf("  %d of %d edges selected at BH-adjusted p < %g\n",
                sum(x$selected), nrow(x), attr(x, "alpha")))
  } else {
    cat(sprintf("  %d of %d edges with a %g%% percentile interval excluding 0\n",
                sum(x$selected), nrow(x), 100 * (1 - attr(x, "alpha"))))
  }
  sel <- as.data.frame(unclass(x))[x$selected, , drop = FALSE]
  if (nrow(sel) > 0) {
    sel <- sel[order(-abs(sel$diff)), , drop = FALSE]
    print(utils::head(sel, n), row.names = FALSE, digits = 3)
    if (nrow(sel) > n) cat(sprintf("  ... and %d more\n", nrow(sel) - n))
  }
  invisible(x)
}

#' Plot differential edges
#'
#' Two panels: a network of the selected differential edges (nodes on a
#' circle; edges coloured by direction, red where group 1 is larger and blue
#' where group 2 is larger, with width proportional to \eqn{|\Delta|}), and a
#' scatter of the group 1 against the group 2 estimates with the selected
#' edges highlighted.
#'
#' @param x An \code{ssjgl_diff_edges} object from [diff_edges()].
#' @param which Panels to draw: \code{"network"}, \code{"scatter"} or both
#'   (default).
#' @param show_all Logical. In the network, also draw (in light grey) edges
#'   that are non-zero in either group but not selected. Default \code{TRUE}.
#' @param ... Passed to \code{\link[graphics]{plot}} for the scatter.
#' @return \code{x}, invisibly.
#' @export
plot.ssjgl_diff_edges <- function(x, which = c("network", "scatter"),
                                  show_all = TRUE, ...) {
  which <- match.arg(which, several.ok = TRUE)
  gn <- attr(x, "group_names")
  col_dir <- ifelse(x$diff > 0, "#C0392B", "#2471A3")
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old))
  if (length(which) == 2) graphics::par(mfrow = c(1, 2))

  if ("network" %in% which) {
    p <- max(c(x$i, x$j))
    nodes <- unique(rbind(data.frame(k = x$i, name = x$node1),
                          data.frame(k = x$j, name = x$node2)))
    nodes <- nodes[order(nodes$k), ]
    ang <- 2 * pi * (seq_len(p) - 1) / p
    xy <- cbind(cos(ang), sin(ang))
    graphics::par(mar = c(1, 1, 3, 1))
    graphics::plot(NA, xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), asp = 1,
                   axes = FALSE, xlab = "", ylab = "",
                   main = sprintf("Differential edges: %s - %s", gn[1], gn[2]))
    if (show_all) {
      bg <- !x$selected & (x$est_g1 != 0 | x$est_g2 != 0)
      graphics::segments(xy[x$i[bg], 1], xy[x$i[bg], 2], xy[x$j[bg], 1],
                         xy[x$j[bg], 2], col = "grey85")
    }
    s <- x$selected
    if (any(s)) {
      w <- 1 + 4 * abs(x$diff[s]) / max(abs(x$diff[s]))
      graphics::segments(xy[x$i[s], 1], xy[x$i[s], 2], xy[x$j[s], 1],
                         xy[x$j[s], 2], col = col_dir[s], lwd = w)
    }
    graphics::points(xy, pch = 21, bg = "white", cex = 2.2)
    graphics::text(1.15 * xy, labels = nodes$name[match(seq_len(p), nodes$k)],
                   cex = 0.7)
    graphics::legend("bottom", horiz = TRUE, bty = "n", lwd = 2, cex = 0.8,
                     col = c("#C0392B", "#2471A3"),
                     legend = c(paste("higher in", gn[1]), paste("higher in", gn[2])))
  }

  if ("scatter" %in% which) {
    graphics::par(mar = c(4.5, 4.5, 3, 1))
    lim <- range(c(x$est_g1, x$est_g2, 0))
    lab <- if (attr(x, "score") == "pcor") "partial correlation" else "precision"
    graphics::plot(x$est_g1, x$est_g2, xlim = lim, ylim = lim, asp = 1,
                   pch = 19, cex = 0.7, col = "grey70",
                   xlab = paste(gn[1], lab), ylab = paste(gn[2], lab),
                   main = "Edge estimates by group", ...)
    graphics::abline(0, 1, lty = 2)
    graphics::abline(h = 0, v = 0, col = "grey90")
    s <- x$selected
    graphics::points(x$est_g1[s], x$est_g2[s], pch = 19, col = col_dir[s])
  }
  invisible(x)
}
