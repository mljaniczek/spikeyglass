# Gaussian negative log-likelihood

Computes `-log|Theta| + tr(S * Theta)` which is proportional to the
negative log-likelihood of a multivariate Gaussian with precision Theta
and sample covariance S. Used for cross-validation scoring.

## Usage

``` r
negloglik_Gaussian(S, Theta)
```

## Arguments

- S:

  Sample covariance matrix (p x p).

- Theta:

  Precision matrix (p x p).

## Value

Scalar negative log-likelihood. Returns `Inf` if Theta is not positive
definite.

## Examples

``` r
S <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
Theta <- solve(S)
negloglik_Gaussian(S, Theta)  # should equal p = 2 (up to constant)
#> [1] 1.712318
```
