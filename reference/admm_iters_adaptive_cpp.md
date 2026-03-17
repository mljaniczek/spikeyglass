# ADMM iterations with adaptive penalties (C++ implementation)

ADMM iterations with adaptive penalties (C++ implementation)

## Usage

``` r
admm_iters_adaptive_cpp(
  Y_list,
  lam1,
  lam2,
  penalty,
  rho,
  rho_increment,
  weights,
  maxiter,
  tol,
  warm_list = NULL
)
```

## Arguments

- Y_list:

  List of K data matrices (n_k x p)

- lam1:

  Matrix-valued lambda1 penalty (p x p)

- lam2:

  Matrix-valued lambda2 penalty (p x p)

- penalty:

  String: "fused" or "group"

- rho:

  ADMM step size

- rho_increment:

  Multiplicative rho update factor

- weights:

  Numeric vector of length K (sample sizes)

- maxiter:

  Maximum ADMM iterations

- tol:

  Convergence tolerance

- warm_list:

  Optional list of K warm-start precision matrices (or R_NilValue)

## Value

List with theta (list of K matrices), Z (list of K matrices), diff,
iters
