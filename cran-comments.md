## Submission

This is a new submission.

## Test environments

* local: macOS (Apple M1), R 4.5.2
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macos-latest
  (release), windows-latest (release)

## R CMD check results

Local `R CMD check --as-cran` (2026-10-15): 0 errors | 1 warning | 1 note.

* WARNING: "Strong dependencies not in mainstream repositories: ggmtools"
  and the `Remotes` field. This disappears once ggmtools is on CRAN (submit
  it first, then remove `Remotes` from DESCRIPTION).
* NOTE: local HTML Tidy too old to validate the HTML manual (machine-specific).
* "New submission" (expected).

## Dependencies

* spikeyglass imports ggmtools, which provides functions shared with other
  multiple-GGM packages. ggmtools must be accepted on CRAN before this
  package is submitted.

## Notes for CRAN

* The package includes C++ code (via RcppArmadillo) adapted from the MIT-
  licensed JGL package (archived on CRAN in 2025); the original author is
  listed as copyright holder and details are in inst/COPYRIGHTS.
