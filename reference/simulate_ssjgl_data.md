# Simulate data from known precision matrices for multiple groups

Generates synthetic data from K groups with shared and differential
graph structure. Useful for evaluating the performance of
[`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md).

## Usage

``` r
simulate_ssjgl_data(
  K = 2,
  p = 20,
  n = 100,
  graph_type = c("band", "random", "hub", "scale-free"),
  edge_prob = 0.1,
  perturb_prob = 0.05,
  signal = 0.3,
  seed = NULL
)
```

## Arguments

- K:

  Integer number of groups. Default 2.

- p:

  Integer number of variables. Default 20.

- n:

  Integer number of observations per group (scalar applied to all
  groups, or vector of length K). Default 100.

- graph_type:

  Character string specifying the base graph type. One of `"band"` (AR-2
  banded), `"random"` (Erdos-Renyi), `"hub"` (star topology), or
  `"scale-free"` (Barabasi-Albert). Default `"band"`.

- edge_prob:

  Numeric edge probability for `"random"` graph type. Default 0.1.

- perturb_prob:

  Numeric probability of perturbing (flipping) each edge in groups 2,
  ..., K relative to group 1. Default 0.05.

- signal:

  Numeric signal strength for off-diagonal entries of the precision
  matrix. Default 0.3.

- seed:

  Integer random seed for reproducibility. Default `NULL`.

## Value

A list with elements:

- data_list:

  List of K data matrices, each n_k x p.

- Omega_list:

  List of K true precision matrices (p x p).

- adj_list:

  List of K binary adjacency matrices (p x p, 0 diagonal).

- Sigma_list:

  List of K true covariance matrices (p x p).

- K:

  Number of groups.

- p:

  Number of variables.

- n:

  Vector of sample sizes.

- graph_type:

  The graph type used.

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, graph_type = "band", seed = 42)
str(sim, max.level = 1)
#> List of 8
#>  $ data_list :List of 2
#>  $ Omega_list:List of 2
#>  $ adj_list  :List of 2
#>  $ Sigma_list:List of 2
#>  $ K         : num 2
#>  $ p         : num 10
#>  $ n         : num [1:2] 50 50
#>  $ graph_type: chr "band"
```
