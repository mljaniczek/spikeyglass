test_that("gete returns valid probabilities", {
  p <- 5
  K <- 2
  theta <- list(diag(p), diag(p))
  theta[[1]][1, 2] <- theta[[1]][2, 1] <- 0.3
  theta[[2]][1, 2] <- theta[[2]][2, 1] <- 0.2

  out <- gete(p, theta, lambda1 = 0.5, lambda2 = 0.5,
              v0 = 0.01, v1 = 1, pi_delta = 0.5, pi_xi = 0.5,
              penalty = "fused", doubly = FALSE)

  expect_type(out, "list")
  expect_true(all(out$prob1 >= 0 & out$prob1 <= 1))
  expect_true(all(diag(out$prob1) == 0))
  expect_true(all(out$d1 >= 0))
  expect_true(all(diag(out$d1) == 0))

  # d1 should be symmetric
  expect_equal(out$d1, t(out$d1))
  expect_equal(out$prob1, t(out$prob1))
})

test_that("gete.doubly returns three-way probabilities", {
  p <- 5
  K <- 2
  theta <- list(diag(p), diag(p))
  theta[[1]][1, 2] <- theta[[1]][2, 1] <- 0.3

  out <- gete(p, theta, lambda1 = 0.5, lambda2 = 0.5,
              v0 = 0.01, v1 = 1, pi_delta = 0.5, pi_xi = 0.5,
              penalty = "fused", doubly = TRUE)

  expect_type(out, "list")
  # prob1 = P(delta=1), prob2 = P(xi=1)
  expect_true(all(out$prob1 >= 0 & out$prob1 <= 1))
  expect_true(all(out$prob2 >= 0 & out$prob2 <= 1))
  # prob2 <= prob1 (xi=1 implies delta=1)
  expect_true(all(out$prob2 <= out$prob1 + 1e-10))
  # d values are positive
  expect_true(all(out$d1 >= 0))
  expect_true(all(out$d2 >= 0))
})

test_that("gete works with group penalty", {
  p <- 5
  K <- 2
  theta <- list(diag(p), diag(p))

  out <- gete(p, theta, lambda1 = 0.5, lambda2 = 0.5,
              v0 = 0.01, v1 = 1, pi_delta = 0.5, pi_xi = 0.5,
              penalty = "group", doubly = FALSE)

  expect_true(all(out$prob1 >= 0 & out$prob1 <= 1))
})

test_that("smaller v0 produces lower inclusion probabilities for zero entries", {
  p <- 5
  K <- 2
  # Theta close to identity -> off-diag near zero
  theta <- list(diag(p), diag(p))

  out_large <- gete(p, theta, lambda1 = 0.5, lambda2 = 0.5,
                     v0 = 0.1, v1 = 1, pi_delta = 0.5, pi_xi = 0.5,
                     penalty = "fused", doubly = FALSE)

  out_small <- gete(p, theta, lambda1 = 0.5, lambda2 = 0.5,
                     v0 = 0.001, v1 = 1, pi_delta = 0.5, pi_xi = 0.5,
                     penalty = "fused", doubly = FALSE)

  # With theta near identity (off-diag ~0), smaller v0 should shrink more
  expect_true(mean(out_small$prob1) <= mean(out_large$prob1) + 1e-10)
})
