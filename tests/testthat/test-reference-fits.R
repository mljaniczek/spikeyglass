# Numerical regression test against reference fits saved before JGL was
# vendored (see fixtures/make-reference-fits.R).

source(test_path("fixtures", "make-reference-fits.R"), local = TRUE)

test_that("ssjgl() reproduces the reference fits to 1e-10", {
  skip_on_cran()
  ref <- readRDS(test_path("fixtures", "reference-fits.rds"))
  cases <- reference_cases()
  expect_setequal(names(ref), names(cases))
  for (nm in names(cases)) {
    new <- fit_reference_case(cases[[nm]])
    old <- ref[[nm]]
    expect_identical(new$itrlist, old$itrlist, label = nm)
    for (i in seq_along(old$thetalist)) {
      for (k in seq_along(old$thetalist[[i]])) {
        expect_equal(unname(new$thetalist[[i]][[k]]), unname(old$thetalist[[i]][[k]]),
                     tolerance = 1e-10, label = nm)
      }
      for (el in c("problist1", "penlist1", "problist2", "penlist2")) {
        expect_equal(unname(new[[el]][[i]]), unname(old[[el]][[i]]),
                     tolerance = 1e-10, label = paste(nm, el))
      }
      expect_equal(new$pi1list[[i]], old$pi1list[[i]], tolerance = 1e-10, label = nm)
      expect_equal(new$pi2list[[i]], old$pi2list[[i]], tolerance = 1e-10, label = nm)
    }
    expect_equal(new$imputed, old$imputed, tolerance = 1e-10, label = nm)
    expect_identical(unname(new$missed), unname(old$missed), label = nm)
  }
})
