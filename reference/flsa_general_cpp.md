# Fused-lasso Z-update for general K (internal; exposed for testing)

Fused-lasso Z-update for general K (internal; exposed for testing)

## Usage

``` r
flsa_general_cpp(A_list, rho, lam1, lam2)
```

## Arguments

- A_list:

  List of K p x p matrices

- rho:

  ADMM step size

- lam1:

  Matrix-valued lambda1 penalty (p x p)

- lam2:

  Matrix-valued lambda2 penalty (p x p)

## Value

List of K matrices
