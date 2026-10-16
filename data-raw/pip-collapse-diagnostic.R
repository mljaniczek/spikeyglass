# Diagnostic: why do SSJGL's PIPs collapse to ~0 at p = 250 in the DiNA
# simulation?
#
# The DiNA simulation (github: dina_simulation, 03_estimate.R::run_ssjgl)
# fits ssjgl(doubly = TRUE, lambda0 = lambda1 = lambda2 = 1, a = b = 1,
# normalize = FALSE, v0s = lambda1 / (lambda1 + 5 * (1:3))) to raw data from
# a scale-free gold standard, with penalty "fused" (SSJGL) or "group"
# (SSJGL_GGL). At p = 250 both problist1 (P(delta = 1)) and problist2
# (P(xi = 1)) were ~0.
#
# This script re-runs the R EM loop of ssjgl() (R/ssjgl.R, impute = FALSE)
# with instrumentation, recording at every EM iteration of every ladder step:
#   * pi_delta, pi_xi (used in the E-step, and after the update);
#   * the PIP distributions (quantiles, fraction > 0.5, exactly 0 / 1);
#   * the components of the E-step log-odds (gete.doubly(), R/utils-gete.R).
#     Writing A = sum_g |theta_g,jk|, D = |theta_1 - theta_2| (fused, c = 1
#     for G = 2) or sqrt(sum_g theta_g^2) (group, c = 1), the three cells are
#       (0,0): G log(l1/v0) - l1 A/v0 + c log(l2/v0) - l2 D/v0 + log(1-pd) + log(1-px)
#       (1,0): G log(l1/v1) - l1 A/v1 + c log(l2/v0) - l2 D/v0 + log(pd)   + log(1-px)
#       (1,1): G log(l1/v1) - l1 A/v1 + c log(l2/v1) - l2 D/v1 + log(pd)   + log(px)
#     so
#       log[(1,0)/(0,0)] = -G log(v1/v0) + logit(pd) + l1 A (1/v0 - 1/v1)
#       log[(1,1)/(1,0)] = -c log(v1/v0) + logit(px) + l2 D (1/v0 - 1/v1)
#     i.e. a data-free normalising-constant term, a log-pi term, and a
#     |theta| term. We record the first two (scalars) and the distribution
#     of the |theta| terms over all pairs, over true edges and over
#     non-edges, plus the break-even A* = (G log(v1/v0) - logit(pd)) /
#     (l1 (1/v0 - 1/v1)) at which (1,0) and (0,0) are equally likely;
#   * the |theta| scale: median |theta_jk| among nonzero off-diagonal
#     entries and among true edges, fraction of nonzero off-diagonals, and
#     the median diagonal.
#
# Grid (fused penalty, doubly = TRUE, lambda0 = lambda1 = lambda2 = 1, a = 1,
# n = 250 per group, K = 2, one replicate):
#   data   : "sim"  = simulate_ssjgl_data(graph_type = "scale-free"),
#                     perturb_prob = 0.2 / p (see note below);
#            "dina" = DiNA's own scale-free gold standards (rep_0001 of
#                     SF-p50n1000, SF-p100n1000, SF-p250n250; the graph does
#                     not depend on n), sampled as DiNA's gen_dat() does
#                     (MVN, mean 0, Sigma = solve(Theta), unit diagonal
#                     Theta, so raw variances are ~1.1-1.3).
#   p      : 50, 125, 250 for sim; 50, 100, 250 for dina (DiNA has no p = 125)
#   scale  : standardize = TRUE vs raw (normalize = FALSE, as in DiNA)
#   b      : 1 (DiNA) vs p (sparse prior)
#   ladder : "dina"    = lambda1 / (lambda1 + 5k), k = 1:3 (lambda1/v0 = 6, 11, 16)
#            "default" = make_v0_ladder(lambda1) (lambda1/v0 = 10, 31.6, 100)
# plus the group penalty on the p = 250 cells (raw, b = 1, both ladders).
#
# Note on simulate_ssjgl_data(): its default perturb_prob = 0.05 flips each of
# the p(p-1)/2 pairs, which at p = 250 adds ~1550 random edges to group 2 (6x
# the ~250 scale-free edges). To keep both groups sparse and scale-free, as in
# DiNA, we use perturb_prob = 0.2/p (about 10% as many flips as edges).
#
# Run from the package root (about 1.5 h on 6 workers):
#   nohup Rscript -e 'source("data-raw/pip-collapse-diagnostic.R")' > data-raw/pip-collapse-cache/log.txt 2>&1 &
# (sourcing parses the whole file up front, so the file can be edited while it
# runs). Set options(pipdiag.no_run = TRUE) before sourcing to load only the
# functions.
# Each cell is cached in data-raw/pip-collapse-cache/ (git-ignored).
# Writes data-raw/pip-collapse-diagnostic-trace.csv (per EM iteration) and
# data-raw/pip-collapse-diagnostic-steps.csv (per ladder step), both
# git-ignored; then make_outputs() writes the committed
# data-raw/pip-collapse-diagnostic-summary.csv (key columns, per ladder step),
# data-raw/pip-collapse-diagnostic-oracle.csv (E-step at the true Theta) and
# the PNG figures used in data-raw/pip-collapse-diagnostic.md. Six follow-up
# cells (second replicate, n = 2000, fixed pi) are defined with the grid below.

Sys.setenv(VECLIB_MAXIMUM_THREADS = "1", OMP_NUM_THREADS = "1",
           OPENBLAS_NUM_THREADS = "1")
suppressMessages(devtools::load_all(quiet = TRUE))
library(parallel)

dina_dir <- "/Users/TinyDragon/Documents/GitHub/dina_simulation/gold_standards"
cache_dir <- "data-raw/pip-collapse-cache"
dir.create(cache_dir, showWarnings = FALSE)
n_workers <- 6

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
make_data <- function(source, p, n, seed, rep = 1) {
  if (source == "sim") {
    sim <- simulate_ssjgl_data(K = 2, p = p, n = n, graph_type = "scale-free",
                               perturb_prob = 0.2 / p, seed = seed)
    list(Y = sim$data_list, adj = sim$adj_list, omega = sim$Omega_list)
  } else {
    lab <- c("50" = "SF-p50n1000", "100" = "SF-p100n1000", "250" = "SF-p250n250")[[as.character(p)]]
    gold <- readRDS(file.path(dina_dir, lab, sprintf("rep_%04d.rds", rep)))
    set.seed(seed)
    Y <- lapply(gold$sigma, function(S) MASS::mvrnorm(n, rep(0, p), S))
    adj <- lapply(gold$adj, function(a) { a <- 1 * (a != 0); diag(a) <- 0; a })
    list(Y = unname(Y), adj = unname(adj), omega = unname(gold$omega))
  }
}

# ---------------------------------------------------------------------------
# Instrumented copy of the R EM loop of ssjgl() (impute = FALSE)
# ---------------------------------------------------------------------------
auc <- function(score, truth) {
  n1 <- sum(truth); n0 <- sum(!truth)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[truth]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
q <- function(x, probs = c(0.5, 0.9, 0.99, 1)) {
  if (!length(x)) return(rep(NA_real_, length(probs)))
  unname(stats::quantile(x, probs, names = FALSE))
}

estep_record <- function(theta, prob1, prob2, pi_d, pi_x, v0, v1, lambda1,
                         lambda2, penalty, ut, truth1, truth2) {
  G <- length(theta)
  A <- Reduce(`+`, lapply(theta, abs))[ut]
  D <- if (penalty == "fused") abs(theta[[1]] - theta[[2]])[ut] else
    sqrt(Reduce(`+`, lapply(theta, function(t) t^2)))[ut]
  cc <- if (penalty == "fused") G * (G - 1) / 2 else 1
  slope <- 1 / v0 - 1 / v1
  th10 <- lambda1 * A * slope
  th11 <- lambda2 * D * slope
  offd <- unlist(lapply(theta, function(t) t[ut]))
  nz <- offd[offd != 0]
  tr_edges <- unlist(lapply(seq_along(theta), function(g) theta[[g]][ut][truth1]))
  p1 <- prob1[ut]; p2 <- prob2[ut]
  qa <- q(th10[truth1]); qn <- q(th10[!truth1])
  qd <- q(th11[truth2]); qdn <- q(th11[!truth2])
  qp1 <- q(p1); qp2 <- q(p2)
  data.frame(
    pi_delta = pi_d, pi_xi = pi_x,
    const10 = -G * log(v1 / v0), logit_pd = qlogis(pi_d),
    const11 = -cc * log(v1 / v0), logit_px = qlogis(pi_x),
    A_star = (G * log(v1 / v0) - qlogis(pi_d)) / (lambda1 * slope),
    D_star = (cc * log(v1 / v0) - qlogis(pi_x)) / (lambda2 * slope),
    th10_edge_med = qa[1], th10_edge_q90 = qa[2], th10_edge_max = qa[4],
    th10_nonedge_med = qn[1], th10_nonedge_q99 = qn[3], th10_nonedge_max = qn[4],
    th11_diff_med = qd[1], th11_diff_max = qd[4], th11_nondiff_q99 = qdn[3],
    A_edge_med = median(A[truth1]), A_edge_zero = mean(A[truth1] == 0),
    theta_nz_med = if (length(nz)) median(abs(nz)) else NA_real_,
    theta_edge_med = median(abs(tr_edges)),
    frac_nz = mean(offd != 0),
    diag_med = median(unlist(lapply(theta, diag))),
    pip1_med = qp1[1], pip1_q90 = qp1[2], pip1_q99 = qp1[3], pip1_max = qp1[4],
    pip1_gt05 = mean(p1 > 0.5), pip1_n_gt05 = sum(p1 > 0.5),
    pip1_eq0 = mean(p1 == 0), pip1_eq1 = mean(p1 == 1),
    pip1_lt1e6 = mean(p1 < 1e-6),
    pip1_edge_mean = mean(p1[truth1]), pip1_nonedge_mean = mean(p1[!truth1]),
    pip1_auc = auc(p1, truth1),
    pip2_med = qp2[1], pip2_q99 = qp2[3], pip2_max = qp2[4],
    pip2_gt05 = mean(p2 > 0.5), pip2_eq0 = mean(p2 == 0),
    pip2_lt1e6 = mean(p2 < 1e-6),
    pip2_diff_mean = mean(p2[truth2]), pip2_auc = auc(p2, truth2)
  )
}

em_instrumented <- function(Y, truth, penalty = "fused", lambda0 = 1, lambda1 = 1,
                            lambda2 = 1, v1 = 1, v0s, doubly = TRUE, rho = 1,
                            a = 1, b = 1, maxitr.em = 500, tol.em = 1e-4,
                            maxitr.jgl = 500, tol.jgl = 1e-5, truncate = 1e-5,
                            c = 0.1, standardize = FALSE, fix_pi = NULL) {
  p <- ncol(Y[[1]]); K <- length(Y)
  if (standardize) Y <- lapply(Y, function(y) scale(y, center = TRUE, scale = TRUE)[, , drop = FALSE])
  Y <- lapply(Y, function(y) { attributes(y) <- list(dim = dim(y)); y })
  ut <- upper.tri(diag(p))
  truth1 <- (truth[[1]] | truth[[2]])[ut]   # edge in either group
  truth2 <- (truth[[1]] != truth[[2]])[ut]  # edge present in exactly one group
  n_edges <- p * (p - 1) / 2

  theta <- theta_last <- vector("list", K)
  prob1 <- matrix(a / (a + b), p, p); prob2 <- prob1
  diag(prob1) <- diag(prob2) <- 0
  for (k in 1:K) theta[[k]] <- theta_last[[k]] <- solve(cov(Y[[k]]) + diag(c, p))

  trace <- list(); steps <- list(); final <- list()
  for (i in seq_along(v0s)) {
    t0 <- Sys.time()
    pi_delta <- a / (a + b); pi_xi <- a / (a + b)
    if (!is.null(fix_pi)) { pi_delta <- fix_pi[1]; pi_xi <- fix_pi[2] }
    pi_delta_last <- NULL; diff <- 1; v0 <- v0s[i]
    # warm start, as in ssjgl()
    tmp <- theta
    for (k in 1:K) diag(tmp[[k]]) <- 0
    for (k in 1:K) theta[[k]] <- solve(cov(Y[[k]]) + diag(c, p))
    for (k in 1:K) {
      if (sum(unlist(tmp[[k]])) != 0 && sum(diag(theta_last[[k]])) > 1) {
        theta_last[[k]][prob1 > 0.5] <- 1
        theta[[k]][theta_last[[k]] == 0] <- 0
      }
    }
    theta_last <- NULL
    for (itr in 1:maxitr.em) {
      if (diff < tol.em) break
      pd_in <- pi_delta; px_in <- pi_xi; theta_in <- theta
      estep <- gete(p, theta, lambda1, lambda2, v0, v1, pi_delta, pi_xi, penalty, doubly)
      d1 <- estep$d1; d2 <- estep$d2; prob1 <- estep$prob1; prob2 <- estep$prob2
      diag(prob1) <- 0
      if (doubly) diag(prob2) <- 0 else prob2 <- matrix(0, p, p)
      pi_delta <- (a + sum(prob1) / 2 - 1) / (a + b + n_edges - 2)
      if (doubly) pi_xi <- (a + sum(prob2) / 2 - 1) / (a + b + n_edges - 2)
      if (!is.null(fix_pi)) { pi_delta <- fix_pi[1]; pi_xi <- fix_pi[2] }
      rec <- estep_record(theta_in, prob1, prob2, pd_in, px_in, v0, v1, lambda1,
                          lambda2, penalty, ut, truth1, truth2)
      lambda2_current <- if (doubly) lambda2 * d2 else lambda2 * d1
      mstep <- JGL.adaptive(Y, penalty = penalty, lambda0 = lambda0,
                            lambda1 = lambda1 * d1, lambda2 = lambda2_current,
                            rho = rho, maxiter = maxitr.jgl, tol = tol.jgl,
                            warm = NULL, return.whole.theta = TRUE,
                            truncate = truncate, normalize = FALSE)
      theta <- lapply(mstep$theta, unname)
      if (!is.null(pi_delta_last)) {
        diff <- 0
        for (k in seq_along(theta_last)) diff <- max(diff, max(abs(theta_last[[k]] - theta[[k]])))
      }
      pi_delta_last <- pi_delta
      theta_last <- theta
      trace[[length(trace) + 1]] <- cbind(step = i, v0 = v0, ratio = lambda1 / v0,
                                          itr = itr, rec, pi_delta_new = pi_delta,
                                          pi_xi_new = pi_xi, theta_diff = diff)
    }
    # end-of-step record: the E-step quantities at the step's final theta
    # (what ssjgl() reports is prob1 from the last E-step, i.e. from the
    # theta before the last M-step; that is the last trace row)
    steps[[i]] <- cbind(step = i, v0 = v0, ratio = lambda1 / v0, itr = itr,
                        trace[[length(trace)]][, -(1:4)],
                        seconds = as.numeric(Sys.time() - t0, units = "secs"))
    final[[i]] <- list(theta = theta, prob1 = prob1, prob2 = prob2,
                       pi_delta = pi_delta, pi_xi = pi_xi, itr = itr)
  }
  list(trace = do.call(rbind, trace), steps = do.call(rbind, steps), final = final)
}

# ---------------------------------------------------------------------------
# Grid
# ---------------------------------------------------------------------------
ladders <- list(dina = 1 / (1 + 5 * (1:3)), default = make_v0_ladder(1))
grid <- rbind(
  expand.grid(source = "sim", p = c(50, 125, 250), standardize = c(TRUE, FALSE),
              bprior = c("1", "p"), ladder = names(ladders), penalty = "fused",
              stringsAsFactors = FALSE),
  expand.grid(source = "dina", p = c(50, 100, 250), standardize = c(TRUE, FALSE),
              bprior = c("1", "p"), ladder = names(ladders), penalty = "fused",
              stringsAsFactors = FALSE),
  expand.grid(source = c("sim", "dina"), p = 250, standardize = FALSE,
              bprior = "1", ladder = names(ladders), penalty = "group",
              stringsAsFactors = FALSE)
)
grid$n <- 250
grid$rep <- 1
grid$fix_pi <- FALSE
# Follow-up cells, added after the first pass to test the mechanism, all on
# DiNA's p = 250 gold standard, raw data, b = 1, fused (the DiNA setting):
#   * a second replicate (gold standard rep_0002, new data) of both ladders;
#   * n = 2000 per group, both ladders: the E-step threshold does not depend
#     on n, so if the mechanism is the threshold the collapse persists;
#   * pi_delta, pi_xi held fixed at the true edge / differential-edge
#     densities (no pi feedback), both ladders.
followup <- expand.grid(source = "dina", p = 250, standardize = FALSE, bprior = "1",
                        ladder = names(ladders), penalty = "fused",
                        stringsAsFactors = FALSE)
followup <- rbind(cbind(followup, n = 250, rep = 2, fix_pi = FALSE),
                  cbind(followup, n = 2000, rep = 1, fix_pi = FALSE),
                  cbind(followup, n = 250, rep = 1, fix_pi = TRUE))
grid <- rbind(grid, followup)
# biggest cells first so the load balances
grid <- grid[order(-grid$p, -grid$n, grid$source), ]
grid$id <- sprintf("%s_p%d_%s_b%s_%s_%s_rep%d%s%s", grid$source, grid$p,
                   ifelse(grid$standardize, "std", "raw"), grid$bprior,
                   grid$ladder, grid$penalty, grid$rep,
                   ifelse(grid$n != 250, paste0("_n", grid$n), ""),
                   ifelse(grid$fix_pi, "_fixpi", ""))

run_cell <- function(g) {
  cache <- file.path(cache_dir, paste0(g$id, ".rds"))
  if (file.exists(cache)) return(readRDS(cache))
  dat <- make_data(g$source, g$p, g$n, seed = 1000 * g$p + g$rep, rep = g$rep)
  ut <- upper.tri(dat$adj[[1]])
  fix <- if (g$fix_pi) c(mean((dat$adj[[1]] | dat$adj[[2]])[ut]),
                         mean((dat$adj[[1]] != dat$adj[[2]])[ut])) else NULL
  t0 <- Sys.time()
  res <- em_instrumented(dat$Y, dat$adj, penalty = g$penalty,
                         v0s = ladders[[g$ladder]], doubly = TRUE,
                         b = if (g$bprior == "p") g$p else 1,
                         standardize = g$standardize, fix_pi = fix)
  meta <- g[c("id", "source", "p", "n", "standardize", "bprior", "ladder", "penalty", "rep", "fix_pi")]
  raw_sd <- sqrt(unlist(lapply(dat$Y, function(y) apply(y, 2, var))))
  meta$raw_sd_med <- median(raw_sd)
  meta$true_edges <- sum((dat$adj[[1]] | dat$adj[[2]])[upper.tri(dat$adj[[1]])])
  meta$true_diff <- sum((dat$adj[[1]] != dat$adj[[2]])[upper.tri(dat$adj[[1]])])
  om <- unlist(lapply(dat$omega, function(o) o[upper.tri(o)]))
  meta$true_theta_med <- median(abs(om[om != 0]))
  meta$true_diag_med <- median(unlist(lapply(dat$omega, diag)))
  out <- list(meta = meta, trace = cbind(meta, res$trace), steps = cbind(meta, res$steps),
              final_pi = sapply(res$final, function(f) c(f$pi_delta, f$pi_xi)),
              seconds = as.numeric(Sys.time() - t0, units = "secs"))
  saveRDS(out, cache)
  out
}

if (!isTRUE(getOption("pipdiag.no_run"))) {
  t_start <- Sys.time()
  todo <- split(grid, seq_len(nrow(grid)))
  cl <- makeCluster(n_workers, type = "PSOCK")
  clusterEvalQ(cl, {
    Sys.setenv(VECLIB_MAXIMUM_THREADS = "1", OMP_NUM_THREADS = "1")
    suppressMessages(devtools::load_all(quiet = TRUE))
    NULL
  })
  clusterExport(cl, c("make_data", "auc", "q", "estep_record", "em_instrumented",
                      "ladders", "run_cell", "cache_dir", "dina_dir"))
  res <- parLapplyLB(cl, todo, function(g) {
    r <- tryCatch(utils::capture.output(x <- run_cell(g)), error = function(e) e)
    if (inherits(r, "error")) return(list(error = conditionMessage(r), id = g$id))
    x
  }, chunk.size = 1)
  stopCluster(cl)
  errs <- Filter(function(r) !is.null(r$error), res)
  if (length(errs)) print(errs)
  res <- Filter(function(r) is.null(r$error), res)
  # cells cached before the follow-up cells were added lack fix_pi
  fill <- function(x) { if (is.null(x$fix_pi)) x$fix_pi <- FALSE; x }
  trace <- do.call(rbind, lapply(res, function(r) fill(r$trace)))
  steps <- do.call(rbind, lapply(res, function(r) fill(r$steps)))
  steps$cell_seconds <- vapply(res, `[[`, numeric(1), "seconds")[match(steps$id, vapply(res, function(r) r$meta$id, ""))]
  write.csv(trace, "data-raw/pip-collapse-diagnostic-trace.csv", row.names = FALSE)
  write.csv(steps, "data-raw/pip-collapse-diagnostic-steps.csv", row.names = FALSE)
  cat("Total wall time:", format(Sys.time() - t_start), "\n")
}

# ---------------------------------------------------------------------------
# Oracle E-step: PIPs at the TRUE precision matrices
# ---------------------------------------------------------------------------
# The E-step is a deterministic function of theta and pi. Evaluating it at the
# true Theta (no estimation error, no M-step shrinkage) and pi = the true edge
# density shows whether a ladder's spike can separate the true edges at all.
oracle_estep <- function() {
  out <- list()
  for (src in c("sim", "dina")) for (p in if (src == "sim") c(50, 125, 250) else c(50, 100, 250)) {
    dat <- make_data(src, p, 10, seed = 1000 * p + 1)
    for (sc in c("raw", "std")) {
      om <- dat$omega
      if (sc == "std") om <- lapply(om, function(o) { s <- sqrt(diag(solve(o))); o * outer(s, s) })
      ut <- upper.tri(om[[1]])
      truth1 <- (dat$adj[[1]] | dat$adj[[2]])[ut]
      A <- (abs(om[[1]]) + abs(om[[2]]))[ut]
      dens <- mean(truth1)
      for (ratio in c(6, 11, 16, 10, 31.6, 100)) {
        v0 <- 1 / ratio
        for (pi_lab in c("0.5", "true density")) {
          pd <- if (pi_lab == "0.5") 0.5 else dens
          Astar <- (2 * log(1 / v0) - qlogis(pd)) / (1 / v0 - 1)
          e <- gete(p, om, 1, 1, v0, 1, pd, pd, "fused", TRUE)
          pr <- e$prob1[ut]
          out[[length(out) + 1]] <- data.frame(
            source = src, p = p, scale = sc, ratio = ratio, pi = pi_lab, pi_value = pd,
            A_true_edge_med = median(A[truth1]), A_star = Astar,
            frac_edges_above_Astar = mean(A[truth1] > Astar),
            pip1_edge_gt05 = mean(pr[truth1] > 0.5), pip1_edge_mean = mean(pr[truth1]))
        }
      }
    }
  }
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Summary table and figures
# ---------------------------------------------------------------------------
make_outputs <- function() {
  steps <- read.csv("data-raw/pip-collapse-diagnostic-steps.csv")
  trace <- read.csv("data-raw/pip-collapse-diagnostic-trace.csv")
  keep <- c("id", "source", "p", "n", "standardize", "bprior", "ladder", "penalty", "rep",
            "fix_pi", "true_edges", "true_diff", "raw_sd_med", "true_theta_med", "step",
            "ratio", "itr", "pi_delta", "pi_xi", "A_star", "D_star", "A_edge_med",
            "A_edge_zero", "theta_edge_med", "frac_nz", "pip1_n_gt05", "pip1_edge_mean",
            "pip1_nonedge_mean", "pip1_max", "pip1_lt1e6", "pip1_auc", "pip2_max",
            "pip2_gt05", "pip2_lt1e6", "pip2_diff_mean", "pip2_auc")
  summ <- steps[, keep]
  summ <- summ[order(summ$source, summ$penalty, -summ$p, summ$n, summ$fix_pi, summ$rep,
                     summ$ladder, summ$standardize, summ$bprior, summ$step), ]
  write.csv(summ, "data-raw/pip-collapse-diagnostic-summary.csv", row.names = FALSE)
  orc <- oracle_estep()
  write.csv(orc, "data-raw/pip-collapse-diagnostic-oracle.csv", row.names = FALSE)
  A_true <- orc$A_true_edge_med[orc$source == "dina" & orc$p == 250 & orc$scale == "raw"][1]

  ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e4e3df"
  col_lad <- c(dina = "#eb6834", default = "#2a78d6")
  lab_lad <- c(dina = "DiNA ladder (lambda1/v0 = 6, 11, 16)",
               default = "default ladder (lambda1/v0 = 10, 31.6, 100)")

  # Figure 1: EM trajectories in the DiNA setting (p = 250, raw, b = 1, fused)
  png("data-raw/pip-collapse-trace.png", width = 2000, height = 1600, res = 200)
  op <- par(mfrow = c(2, 1), mar = c(3.6, 4.6, 2.4, 1), mgp = c(2.4, 0.7, 0),
            col.axis = ink2, col.lab = ink, fg = ink2, las = 1)
  sel <- trace$source == "dina" & trace$p == 250 & !trace$standardize &
    trace$bprior == "1" & trace$penalty == "fused" & trace$n == 250 &
    trace$rep == 1 & !trace$fix_pi
  tr <- trace[sel, ]
  tr <- tr[order(tr$ladder, tr$step, tr$itr), ]
  tr$x <- ave(seq_len(nrow(tr)), tr$ladder, FUN = seq_along)
  xl <- range(tr$x)
  plot(NA, xlim = xl, ylim = c(-20, 0), xlab = "EM iteration (cumulative over the 3 ladder steps)",
       ylab = "log10 pi", main = "DiNA setting, p = 250: pi_delta (solid) and pi_xi (dashed); labels = lambda1/v0",
       col.main = ink, font.main = 1, cex.main = 0.9)
  abline(h = seq(-20, 0, 5), col = grid_col)
  for (l in names(col_lad)) {
    d <- tr[tr$ladder == l, ]
    for (s in unique(d$step)) {
      ds <- d[d$step == s, ]
      lines(ds$x, log10(pmax(ds$pi_delta, 1e-20)), col = col_lad[l], lwd = 2)
      lines(ds$x, log10(pmax(ds$pi_xi, 1e-20)), col = col_lad[l], lwd = 2, lty = 2)
      text(min(ds$x), if (l == "dina") -1.6 else -0.2, round(ds$ratio[1], 1),
           col = col_lad[l], pos = 4, cex = 0.7)
    }
  }
  legend("bottomright", legend = lab_lad, col = col_lad, lwd = 2, bty = "n", text.col = ink, cex = 0.8)
  plot(NA, xlim = xl, ylim = c(0, 2.5), xlab = "EM iteration (cumulative over the 3 ladder steps)",
       ylab = "A = sum_g |theta_g,jk|",
       main = "Break-even A* for P(delta=1) = 1/2 (solid) vs median estimated A over true edges (dashed)",
       col.main = ink, font.main = 1, cex.main = 0.9)
  abline(h = seq(0, 2.5, 0.5), col = grid_col)
  abline(h = A_true, col = ink2, lty = 3)
  text(xl[2] * 0.85, A_true, "median A at the true Theta", pos = 3, col = ink2, cex = 0.7, offset = 0.2)
  for (l in names(col_lad)) {
    d <- tr[tr$ladder == l, ]
    for (s in unique(d$step)) {
      ds <- d[d$step == s, ]
      lines(ds$x, pmin(ds$A_star, 2.5), col = col_lad[l], lwd = 2)
      lines(ds$x, ds$A_edge_med, col = col_lad[l], lwd = 2, lty = 2)
    }
  }
  par(op); dev.off()

  # Figure 2: final ladder step of every fused cell: threshold vs signal
  png("data-raw/pip-collapse-threshold.png", width = 1600, height = 1400, res = 200)
  op <- par(mar = c(4, 4.6, 2.4, 1), mgp = c(2.4, 0.7, 0), col.axis = ink2, col.lab = ink,
            fg = ink2, las = 1)
  f <- steps[steps$step == 3 & steps$penalty == "fused" & steps$n == 250 & !steps$fix_pi &
               steps$rep == 1, ]
  frac <- f$pip1_n_gt05 / f$true_edges
  plot(f$A_edge_med, f$A_star, log = "y", xlim = c(0.2, 0.75), ylim = c(0.1, 4),
       pch = ifelse(f$source == "dina", 16, 17), col = col_lad[f$ladder],
       cex = 0.6 + 2 * pmin(frac, 1),
       xlab = "median estimated A = sum_g |theta_g,jk| over true edges",
       ylab = "break-even A* (log scale)",
       main = "Last ladder step, all fused cells; size = #(PIP > 0.5) / #true edges",
       col.main = ink, font.main = 1, cex.main = 0.9)
  lines(c(0.1, 1), c(0.1, 1), col = ink2, lty = 2)  # A* = A
  text(0.32, 2.3, "A* > A: true edges fall in the spike", col = ink2, cex = 0.75)
  text(0.62, 0.105, "A* < A: true edges in the slab", col = ink2, cex = 0.75)
  legend("topright", legend = c(lab_lad, "DiNA gold standard", "simulate_ssjgl_data", "dashed: A* = A"),
         col = c(col_lad, ink2, ink2, NA), pch = c(15, 15, 16, 17, NA), bty = "n", text.col = ink, cex = 0.7)
  par(op); dev.off()
  invisible(list(summary = summ, oracle = orc))
}

if (!isTRUE(getOption("pipdiag.no_run")) && file.exists("data-raw/pip-collapse-diagnostic-steps.csv"))
  make_outputs()
