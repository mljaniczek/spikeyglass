# Changelog

## spikeyglass 0.1.0

### CRAN preparation

- [`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
  gains `verbose = FALSE`. Progress (each EM iteration on the R path,
  each ladder step, re-initialisation notices) is reported with
  [`message()`](https://rdrr.io/r/base/message.html) only when
  `verbose = TRUE`. It was printed unconditionally with
  [`cat()`](https://rdrr.io/r/base/cat.html).
  [`SSJGL_select_v0_cv()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_select_v0_cv.md),
  [`SSJGL_final_with_pcor_CI()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_final_with_pcor_CI.md)
  and
  [`SSJGL_CV_final_pcorCI()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_CV_final_pcorCI.md)
  now default to `verbose = FALSE` and use
  [`message()`](https://rdrr.io/r/base/message.html).
- [`simulate_ssjgl_data()`](https://mljaniczek.github.io/spikeyglass/reference/simulate_ssjgl_data.md),
  [`SSJGL_select_v0_cv()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_select_v0_cv.md)
  and
  [`SSJGL_final_with_pcor_CI()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_final_with_pcor_CI.md)
  no longer change the caller’s random-number stream: the RNG state is
  restored on exit after using `seed`.
- [`plot_path()`](https://mljaniczek.github.io/spikeyglass/reference/plot_path.md)
  restores [`par()`](https://rdrr.io/r/graphics/par.html) on exit, has
  dead code removed, and its example now runs. Its legend now says what
  is drawn (the old “Edge” / “Non-edge” legend referred to a true graph
  the function never had).
- `simdat` is regenerated reproducibly with
  [`simulate_ssjgl_data()`](https://mljaniczek.github.io/spikeyglass/reference/simulate_ssjgl_data.md)
  (`data-raw/simdat.R`) and now also contains the true precision
  matrices (`Omega`). The previous version came from a script that could
  not be run.
- The unused `fit1.Rdata` files are removed, so the package no longer
  depends on R \>= 3.5.0 through serialized data. It now depends on R
  \>= 4.1.0.
- New `inst/CITATION` (method paper and package), `CITATION.cff` and
  `cran-comments.md`. The DESCRIPTION has a fuller description citing Li
  et al. (2019), `BugReports`, and the GitHub URL. The Title is in title
  case.
- GitHub Actions: R CMD check on macOS, Windows and Ubuntu (devel,
  release, oldrel-1), plus test coverage.
- The profiling script moved from `inst/profiling/` to `scripts/` (not
  installed).

## spikeyglass 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
