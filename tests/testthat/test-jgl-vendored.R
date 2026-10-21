# Tests for the routines vendored or ported from JGL 2.3.2.

test_that("flsa_general_cpp() matches JGL:::flsa.general (saved outputs)", {
  cases <- readRDS(test_path("fixtures", "jgl-flsa-general.rds"))
  for (cs in cases) {
    Z <- flsa_general_cpp(cs$A, cs$rho, cs$lam1, cs$lam2)
    expect_equal(Z, cs$Z, tolerance = 1e-12)
  }
})

test_that("connected_components_cpp() labels components like igraph (weak)", {
  adj <- matrix(FALSE, 7, 7)
  adj[1, 3] <- TRUE            # one direction only: weakly connected
  adj[3, 5] <- adj[5, 3] <- TRUE
  adj[2, 6] <- adj[6, 2] <- TRUE
  adj[4, 4] <- NA
  diag(adj) <- TRUE
  expect_equal(connected_components_cpp(adj), c(1L, 2L, 1L, 3L, 1L, 2L, 4L))
})

test_that("penalty.as.matrix() expands scalars and validates matrices", {
  expect_equal(penalty.as.matrix(0.3, 3, penalize.diagonal = TRUE), matrix(0.3, 3, 3))
  m <- penalty.as.matrix(0.3, 3, penalize.diagonal = FALSE)
  expect_equal(diag(m), rep(0, 3))
  expect_error(penalty.as.matrix(matrix(1:4, 2, 2), 2, TRUE), "not symmetric")
  expect_error(penalty.as.matrix(diag(3), 2, TRUE), "wrong dimension")
})

test_that("unconnected-node solver converges to the closed-form solution", {
  S <- list(c(0.8, 1.5, 2), c(1.1, 0.4, 0.9))
  n <- c(20, 15)
  Z <- admm.iters.unconnected.S(S, lambda0 = 0.7, rho = 1, weights = n,
                                maxiter = 1e5, tol = 1e-12)
  for (k in 1:2) expect_equal(Z[[k]], 1 / (S[[k]] + 0.7 / n[k]), tolerance = 1e-8)
})
