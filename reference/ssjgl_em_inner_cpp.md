# Full EM inner loop in C++ (E-step + block detection + ADMM)

Runs the EM algorithm for a single v0 step entirely in C++, avoiding R
overhead between iterations. Handles block decomposition via connected
components (replacing igraph), precomputes covariance once, and runs the
ADMM solver per block.

## Usage

``` r
ssjgl_em_inner_cpp(
  S_list,
  addvar_list,
  n_vec,
  theta_init,
  penalty,
  lambda0,
  lambda1,
  lambda2,
  v0,
  v1,
  doubly,
  rho,
  a,
  b,
  maxitr_em,
  tol_em,
  maxitr_jgl,
  tol_jgl,
  truncate
)
```

## Arguments

- S_list:

  List of K sample covariance matrices (p x p, biased)

- addvar_list:

  List of K addvar matrices for imputation (or NULL)

- n_vec:

  Numeric vector of sample sizes (length K)

- theta_init:

  List of K initial precision matrices

- penalty:

  String: "fused" or "group"

- lambda0:

  Scalar diagonal penalty

- lambda1:

  Scalar off-diagonal penalty

- lambda2:

  Scalar cross-group penalty

- v0:

  Spike variance for this step

- v1:

  Slab variance

- doubly:

  Logical: use doubly spike-and-slab?

- rho:

  ADMM step size

- a:

  Beta prior shape1

- b:

  Beta prior shape2

- maxitr_em:

  Max EM iterations

- tol_em:

  EM convergence tolerance

- maxitr_jgl:

  Max ADMM iterations

- tol_jgl:

  ADMM convergence tolerance

- truncate:

  Threshold for zeroing small entries

## Value

List with theta, prob1, prob2, d1, d2, pi_delta, pi_xi, itr
