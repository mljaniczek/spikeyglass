test_that("simulate_ssjgl_data returns correct structure", {
  sim <- simulate_ssjgl_data(K = 3, p = 10, n = 50, graph_type = "band", seed = 42)

  expect_type(sim, "list")
  expect_equal(sim$K, 3)
  expect_equal(sim$p, 10)
  expect_equal(sim$n, c(50, 50, 50))
  expect_equal(sim$graph_type, "band")
  expect_length(sim$data_list, 3)
  expect_length(sim$Omega_list, 3)
  expect_length(sim$adj_list, 3)
  expect_length(sim$Sigma_list, 3)

  # Dimensions
  for (k in 1:3) {
    expect_equal(dim(sim$data_list[[k]]), c(50, 10))
    expect_equal(dim(sim$Omega_list[[k]]), c(10, 10))
    expect_equal(dim(sim$adj_list[[k]]), c(10, 10))
    expect_equal(dim(sim$Sigma_list[[k]]), c(10, 10))
  }
})

test_that("simulated precision matrices are positive definite", {
  sim <- simulate_ssjgl_data(K = 2, p = 8, n = 30, graph_type = "band", seed = 1)

  for (k in 1:2) {
    eig <- eigen(sim$Omega_list[[k]], symmetric = TRUE, only.values = TRUE)$values
    expect_true(all(eig > 0))
  }
})

test_that("simulated adjacency matrices are symmetric with 0 diagonal", {
  sim <- simulate_ssjgl_data(K = 2, p = 8, n = 30, graph_type = "random",
                              edge_prob = 0.3, seed = 5)

  for (k in 1:2) {
    adj <- sim$adj_list[[k]]
    expect_true(isSymmetric(adj))
    expect_true(all(diag(adj) == 0))
    expect_true(all(adj %in% c(0L, 1L)))
  }
})

test_that("all graph types work", {
  for (gt in c("band", "random", "hub", "scale-free")) {
    sim <- simulate_ssjgl_data(K = 2, p = 8, n = 20, graph_type = gt, seed = 10)
    expect_equal(sim$graph_type, gt)
    expect_equal(dim(sim$data_list[[1]]), c(20, 8))
  }
})

test_that("different n per group works", {
  sim <- simulate_ssjgl_data(K = 2, p = 5, n = c(30, 50), graph_type = "band", seed = 1)
  expect_equal(nrow(sim$data_list[[1]]), 30)
  expect_equal(nrow(sim$data_list[[2]]), 50)
})

test_that("seed produces reproducible results", {
  sim1 <- simulate_ssjgl_data(K = 2, p = 5, n = 20, seed = 99)

  sim2 <- simulate_ssjgl_data(K = 2, p = 5, n = 20, seed = 99)
  expect_identical(sim1$adj_list, sim2$adj_list)
  expect_identical(sim1$Omega_list, sim2$Omega_list)
})
