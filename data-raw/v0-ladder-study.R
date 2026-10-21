# Simulation study: which v0 ladder should ssjgl() use by default?
#
# Compares the three ladder rules that were in use:
#   "old_default": v0s = c(0.1, 0.03, 0.01)                (lambda1/v0 = 10, 33, 100 at lambda1 = 1)
#   "make_v0_ladder": make_v0_ladder(lambda1) as of 2026-10 (10 steps, lambda1/v0 log-spaced 5 to 500)
#   "dina": v0 = lambda1 / (lambda1 + 5k), k = 1:3          (lambda1/v0 = 6, 11, 16 at lambda1 = 1)
#
# Design: K = 2 groups, p in {20, 50, 100}, n in {100, 300} per group,
# band and scale-free graphs (simulate_ssjgl_data defaults otherwise), 10
# replicates. Data are standardised (standardize = TRUE). Fused penalty,
# single spike-and-slab, lambda0 = lambda1 = lambda2 = 1, b = p (the sparse
# prior used by Li et al. 2019).
#
# Metrics at every ladder step: AUC of the PIPs (mean over groups, against
# each group's true graph), F1 at PIP > 0.5 (mean over groups), number of
# selected edges, and the Jaccard similarity of the selected edge set with
# the previous step (stability).
#
# Run from the package root (takes roughly 1-2 h on 8 cores):
#   Rscript data-raw/v0-ladder-study.R
# Each grid cell is cached in data-raw/v0-ladder-study-cache/ (git-ignored),
# so an interrupted run resumes where it stopped.
# Writes data-raw/v0-ladder-study-steps.rds (all steps) and
# inst/extdata/v0_ladder_study.csv (summary used by the vignette).

suppressMessages(devtools::load_all(quiet = TRUE))
library(parallel)

lambda1 <- 1
ladders <- list(
  old_default    = c(0.1, 0.03, 0.01),
  make_v0_ladder = 1 / exp(seq(log(5), log(500), length.out = 10)) * lambda1,
  dina           = lambda1 / (lambda1 + 5 * (1:3))
)

grid <- expand.grid(rep = 1:10, graph = c("band", "scale-free"), n = c(100, 300),
                    p = c(20, 50, 100), stringsAsFactors = FALSE)
grid$seed <- 1000 * seq_len(nrow(grid))

jaccard <- function(a, b) {
  u <- sum(a | b)
  if (u == 0) return(1)
  sum(a & b) / u
}

cache_dir <- "data-raw/v0-ladder-study-cache"
dir.create(cache_dir, showWarnings = FALSE)

run_one <- function(g) {
  cache <- file.path(cache_dir, sprintf("p%d_n%d_%s_rep%02d.rds", g$p, g$n, g$graph, g$rep))
  if (file.exists(cache)) return(readRDS(cache))
  sim <- simulate_ssjgl_data(K = 2, p = g$p, n = g$n, graph_type = g$graph, seed = g$seed)
  out <- list()
  for (rule in names(ladders)) {
    fit <- NULL
    utils::capture.output(fit <- suppressMessages(ssjgl(
      sim$data_list, penalty = "fused", lambda0 = 1, lambda1 = lambda1, lambda2 = 1,
      v0s = ladders[[rule]], b = g$p, standardize = TRUE, impute = FALSE)))
    prev <- NULL
    for (i in seq_along(fit$v0s)) {
      m <- compute_metrics(fit, sim$adj_list, v0_index = i)
      pr <- fit$problist1[[i]]
      sel <- pr[upper.tri(pr)] > 0.5
      out[[length(out) + 1]] <- data.frame(
        g[c("p", "n", "graph", "rep")], rule = rule, step = i,
        n_steps = length(fit$v0s), v0 = fit$v0s[i], ratio = lambda1 / fit$v0s[i],
        AUC = m$overall$mean_AUC,
        F1 = mean(vapply(m$per_group, function(x) x$F1, numeric(1))),
        TPR = m$overall$mean_TPR, FPR = m$overall$mean_FPR,
        edges = sum(sel), true_edges = mean(vapply(sim$adj_list, function(a) sum(a[upper.tri(a)]), numeric(1))),
        jaccard_prev = if (is.null(prev)) NA_real_ else jaccard(sel, prev),
        em_itr = fit$itrlist[i], seconds = fit$timelist[i])
      prev <- sel
    }
  }
  res <- do.call(rbind, out)
  saveRDS(res, cache)
  res
}

# Biggest jobs first for better load balancing. PSOCK workers rather than
# forking (mclapply): on macOS the Accelerate BLAS is not fork-safe, and
# forked workers segfaulted in the p = 100 eigendecompositions.
ord <- order(-grid$p, -grid$n)
cl <- makeCluster(max(1, detectCores() - 1))
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(quiet = TRUE))))
clusterExport(cl, c("ladders", "lambda1", "jaccard", "run_one", "cache_dir"))
res <- parLapplyLB(cl, split(grid[ord, ], seq_len(nrow(grid))),
                   function(g) tryCatch(run_one(g), error = function(e) e))
stopCluster(cl)
ok <- vapply(res, is.data.frame, logical(1))
if (!all(ok)) stop("some fits failed: ", paste(which(!ok), collapse = ", "))
steps <- do.call(rbind, res)
rownames(steps) <- NULL
saveRDS(steps, "data-raw/v0-ladder-study-steps.rds")

# Summary: final step of each rule, plus stability of the last two steps
final <- steps[steps$step == steps$n_steps, ]
agg <- aggregate(cbind(AUC, F1, TPR, FPR, edges, true_edges, jaccard_prev, seconds) ~
                   rule + p + n + graph, data = final, FUN = mean)
time_total <- aggregate(seconds ~ rule + p + n + graph + rep, data = steps, FUN = sum)
agg$total_seconds <- aggregate(seconds ~ rule + p + n + graph, data = time_total, FUN = mean)$seconds
agg$seconds <- NULL
agg <- agg[order(agg$p, agg$n, agg$graph, agg$rule), ]
dir.create("inst/extdata", showWarnings = FALSE)
utils::write.csv(agg, "inst/extdata/v0_ladder_study.csv", row.names = FALSE)

# Also: performance as a function of lambda1/v0 along the 10-step ladder
by_ratio <- aggregate(cbind(AUC, F1, edges, true_edges) ~ ratio + p + n + graph,
                      data = steps[steps$rule == "make_v0_ladder", ], FUN = mean)
utils::write.csv(by_ratio[order(by_ratio$p, by_ratio$n, by_ratio$graph, by_ratio$ratio), ],
                 "inst/extdata/v0_ladder_study_by_ratio.csv", row.names = FALSE)
print(agg, digits = 3)
