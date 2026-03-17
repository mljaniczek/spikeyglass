# Convert precision matrix to partial correlations

Computes partial correlations from a precision matrix using the formula
`pcor[i,j] = -Theta[i,j] / sqrt(Theta[i,i] * Theta[j,j])`. Diagonal
entries are set to 1.

## Usage

``` r
precision_to_pcor(Theta)
```

## Arguments

- Theta:

  Precision matrix (p x p). Must be positive definite.

## Value

A p x p partial correlation matrix with 1s on the diagonal. Returns a
matrix of `NA`s if the diagonal contains non-positive or non-finite
values.

## Examples

``` r
Theta <- matrix(c(2, -0.5, -0.5, 2), 2, 2)
precision_to_pcor(Theta)  # partial correlation = 0.25
#>      [,1] [,2]
#> [1,] 1.00 0.25
#> [2,] 0.25 1.00
```
