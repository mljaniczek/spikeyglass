# Runtime benchmark for ssjgl() (and, optionally, multiGGM's SSVS sampler).
#
# Times ssjgl() over p in {25, 50, 100, 200, 400} x K in {2, 3} x penalty in
# {fused, group}, with doubly = TRUE, the default v0 ladder
# (make_v0_ladder(lambda1)), n = 200 per group, lambda0 = lambda1 = lambda2 = 1,
# b = p, standardize = TRUE, on band-graph data from simulate_ssjgl_data().
# Fits run one at a time (no parallel jobs), so the times are single-fit
# wall-clock times. The BLAS may still use several threads; it is recorded.
#
# Optionally (--multiggm), the same data are fitted with
# multiGGM::multiggm_mcmc(method = "ssvs") with burnin = 5000 and nsave = 1000
# (the DiNA production setting and the multiggm_mcmc() default), for
# p < 200 only. Larger p takes hours per fit and needs --multiggm-large.
#
# Results: inst/extdata/bench_runtime.csv, one row per fit, with the
# hardware, OS, R and BLAS/LAPACK information in every row.
# Each fit is cached in bench-cache/ (delete it to rerun), so an interrupted
# run resumes.
#
# Usage, from the package root:
#   Rscript inst/scripts/bench_runtime.R [--reps=1] [--multiggm] [--multiggm-large]

args <- commandArgs(trailingOnly = TRUE)
reps <- as.integer(sub("--reps=", "", grep("^--reps=", args, value = TRUE)[1]))
if (is.na(reps)) reps <- 1L
run_multiggm <- "--multiggm" %in% args
multiggm_large <- "--multiggm-large" %in% args

suppressMessages(library(spikeyglass))
if (run_multiggm) suppressMessages(library(multiGGM))

cache_dir <- "bench-cache"
dir.create(cache_dir, showWarnings = FALSE)

# --- Environment ------------------------------------------------------------
sys_cmd <- function(cmd) tryCatch(trimws(system(cmd, intern = TRUE)[1]),
                                  error = function(e) NA_character_)
si <- utils::sessionInfo()
env_info <- data.frame(
  cpu = switch(Sys.info()[["sysname"]],
               Darwin = sys_cmd("sysctl -n machdep.cpu.brand_string"),
               Linux = sub(".*: ", "", sys_cmd("grep -m1 'model name' /proc/cpuinfo")),
               NA_character_),
  cores = parallel::detectCores(logical = FALSE),
  ram_gb = switch(Sys.info()[["sysname"]],
                  Darwin = round(as.numeric(sys_cmd("sysctl -n hw.memsize")) / 2^30),
                  Linux = round(as.numeric(sub("\\D+(\\d+).*", "\\1",
                    sys_cmd("grep MemTotal /proc/meminfo"))) / 2^20),
                  NA_real_),
  os = si$running,
  r_version = R.version$version.string,
  blas = si$BLAS,
  lapack = si$LAPACK,
  spikeyglass_version = as.character(utils::packageVersion("spikeyglass")),
  date = format(Sys.Date()),
  stringsAsFactors = FALSE
)

# --- Grid -------------------------------------------------------------------
grid <- expand.grid(rep = seq_len(reps), penalty = c("fused", "group"),
                    K = c(2L, 3L), p = c(25L, 50L, 100L, 200L, 400L),
                    stringsAsFactors = FALSE)
n <- 200L

time_ssjgl <- function(g) {
  sim <- simulate_ssjgl_data(K = g$K, p = g$p, n = n, graph_type = "band",
                             seed = 1000 * g$p + 10 * g$K + g$rep)
  fit <- NULL
  t <- system.time(utils::capture.output(fit <- suppressMessages(ssjgl(
    sim$data_list, penalty = g$penalty, lambda0 = 1, lambda1 = 1, lambda2 = 1,
    doubly = TRUE, b = g$p, standardize = TRUE, impute = FALSE))))
  data.frame(package = "spikeyglass", method = paste0("ssjgl_", g$penalty),
             p = g$p, K = g$K, n = n, rep = g$rep,
             seconds = t[["elapsed"]], cpu_seconds = t[["user.self"]] + t[["sys.self"]],
             iterations = sum(fit$itrlist), ladder_steps = length(fit$v0s),
             stringsAsFactors = FALSE)
}

time_multiggm <- function(g) {
  sim <- simulate_ssjgl_data(K = g$K, p = g$p, n = n, graph_type = "band",
                             seed = 1000 * g$p + 10 * g$K + g$rep)
  Y <- lapply(sim$data_list, scale)
  t <- system.time(utils::capture.output(fit <- suppressMessages(
    multiggm_mcmc(data_list = Y, method = "ssvs", burnin = 5000, nsave = 1000,
                  seed = g$rep))))
  data.frame(package = "multiGGM", method = "ssvs", p = g$p, K = g$K, n = n,
             rep = g$rep, seconds = t[["elapsed"]],
             cpu_seconds = t[["user.self"]] + t[["sys.self"]],
             iterations = 6000, ladder_steps = NA_integer_,
             stringsAsFactors = FALSE)
}

cached <- function(key, f, g) {
  path <- file.path(cache_dir, paste0(key, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  res <- f(g)
  saveRDS(res, path)
  message(sprintf("%s: %.1f s", key, res$seconds))
  res
}

res <- list()
for (r in seq_len(nrow(grid))) {
  g <- grid[r, ]
  res[[length(res) + 1]] <- cached(
    sprintf("ssjgl_%s_p%d_K%d_rep%d", g$penalty, g$p, g$K, g$rep), time_ssjgl, g)
}
if (run_multiggm) {
  mg <- unique(grid[, c("rep", "K", "p")])
  if (!multiggm_large) mg <- mg[mg$p < 200, ]
  for (r in seq_len(nrow(mg))) {
    g <- mg[r, ]
    res[[length(res) + 1]] <- cached(
      sprintf("multiggm_ssvs_p%d_K%d_rep%d", g$p, g$K, g$rep), time_multiggm, g)
  }
}

out <- cbind(do.call(rbind, res), env_info[rep(1, length(res)), ], row.names = NULL)
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(out, "inst/extdata/bench_runtime.csv", row.names = FALSE)
print(out[, c("package", "method", "p", "K", "rep", "seconds", "iterations")], digits = 3)
