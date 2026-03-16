# Data simulation utilities for spikeyglass
# Adapted from multiGGMr (github.com/mljaniczek/multiGGMr)

#' Simulate data from known precision matrices for multiple groups
#'
#' Generates synthetic data from K groups with shared and differential
#' graph structure. Useful for evaluating the performance of
#' \code{\link{ssjgl}}.
#'
#' @param K Integer number of groups. Default 2.
#' @param p Integer number of variables. Default 20.
#' @param n Integer number of observations per group (scalar applied to all
#'   groups, or vector of length K). Default 100.
#' @param graph_type Character string specifying the base graph type.
#'   One of \code{"band"} (AR-2 banded), \code{"random"} (Erdos-Renyi),
#'   \code{"hub"} (star topology), or \code{"scale-free"} (Barabasi-Albert).
#'   Default \code{"band"}.
#' @param edge_prob Numeric edge probability for \code{"random"} graph type.
#'   Default 0.1.
#' @param perturb_prob Numeric probability of perturbing (flipping) each edge
#'   in groups 2, ..., K relative to group 1. Default 0.05.
#' @param signal Numeric signal strength for off-diagonal entries of the
#'   precision matrix. Default 0.3.
#' @param seed Integer random seed for reproducibility. Default \code{NULL}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{data_list}{List of K data matrices, each n_k x p.}
#'     \item{Omega_list}{List of K true precision matrices (p x p).}
#'     \item{adj_list}{List of K binary adjacency matrices (p x p, 0 diagonal).}
#'     \item{Sigma_list}{List of K true covariance matrices (p x p).}
#'     \item{K}{Number of groups.}
#'     \item{p}{Number of variables.}
#'     \item{n}{Vector of sample sizes.}
#'     \item{graph_type}{The graph type used.}
#'   }
#' @export
#'
#' @examples
#' sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, graph_type = "band", seed = 42)
#' str(sim, max.level = 1)
simulate_ssjgl_data <- function(K = 2, p = 20, n = 100,
                                graph_type = c("band", "random", "hub", "scale-free"),
                                edge_prob = 0.1,
                                perturb_prob = 0.05,
                                signal = 0.3,
                                seed = NULL) {
  graph_type <- match.arg(graph_type)
  if (!is.null(seed)) set.seed(seed)
  if (length(n) == 1) n <- rep(n, K)
  stopifnot(length(n) == K)

  # Generate base adjacency matrix
  adj_base <- .make_base_graph(p, graph_type, edge_prob)

  # Generate per-group adjacency with perturbations
  adj_list <- vector("list", K)
  adj_list[[1]] <- adj_base
  if (K > 1) {
    for (k in 2:K) {
      adj_list[[k]] <- .perturb_graph(adj_base, perturb_prob)
    }
  }

  # Convert adjacency to precision matrices and generate data
  Omega_list <- vector("list", K)
  Sigma_list <- vector("list", K)
  data_list <- vector("list", K)

  for (k in seq_len(K)) {
    Omega_list[[k]] <- .adj_to_precision(adj_list[[k]], signal)
    Sigma_list[[k]] <- solve(Omega_list[[k]])
    data_list[[k]] <- MASS::mvrnorm(n = n[k], mu = rep(0, p),
                                     Sigma = Sigma_list[[k]])
  }

  list(
    data_list = data_list,
    Omega_list = Omega_list,
    adj_list = adj_list,
    Sigma_list = Sigma_list,
    K = K,
    p = p,
    n = n,
    graph_type = graph_type
  )
}

#' Generate a base graph adjacency matrix
#' @noRd
.make_base_graph <- function(p, graph_type, edge_prob) {
  adj <- matrix(0L, p, p)

  if (graph_type == "band") {
    for (i in 1:(p - 1)) {
      adj[i, i + 1] <- adj[i + 1, i] <- 1L
    }
    if (p > 2) {
      for (i in 1:(p - 2)) {
        adj[i, i + 2] <- adj[i + 2, i] <- 1L
      }
    }
  } else if (graph_type == "random") {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        if (stats::runif(1) < edge_prob) {
          adj[i, j] <- adj[j, i] <- 1L
        }
      }
    }
  } else if (graph_type == "hub") {
    adj[1, 2:p] <- adj[2:p, 1] <- 1L
  } else if (graph_type == "scale-free") {
    degrees <- rep(0L, p)
    for (i in 2:p) {
      probs <- degrees[1:(i - 1)] + 1
      target <- sample.int(i - 1, size = 1, prob = probs)
      adj[i, target] <- adj[target, i] <- 1L
      degrees[i] <- degrees[i] + 1L
      degrees[target] <- degrees[target] + 1L
    }
  }

  adj
}

#' Perturb a graph by flipping edges with given probability
#' @noRd
.perturb_graph <- function(adj, perturb_prob) {
  p <- nrow(adj)
  adj_new <- adj
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      if (stats::runif(1) < perturb_prob) {
        adj_new[i, j] <- adj_new[j, i] <- 1L - adj_new[i, j]
      }
    }
  }
  adj_new
}

#' Convert adjacency matrix to positive definite precision matrix
#' @noRd
.adj_to_precision <- function(adj, signal) {
  p <- nrow(adj)
  Omega <- diag(p)

  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      if (adj[i, j] == 1L) {
        val <- signal * sample(c(-1, 1), 1)
        Omega[i, j] <- Omega[j, i] <- val
      }
    }
  }

  # Ensure positive definiteness via diagonal loading
  eig_min <- min(eigen(Omega, symmetric = TRUE, only.values = TRUE)$values)
  if (eig_min <= 0.1) {
    Omega <- Omega + diag(abs(eig_min) + 0.1, p)
  }

  Omega
}
