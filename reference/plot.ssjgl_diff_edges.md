# Plot differential edges

Two panels: a network of the selected differential edges (nodes on a
circle; edges coloured by direction, red where group 1 is larger and
blue where group 2 is larger, with width proportional to
\\\|\Delta\|\\), and a scatter of the group 1 against the group 2
estimates with the selected edges highlighted.

## Usage

``` r
# S3 method for class 'ssjgl_diff_edges'
plot(x, which = c("network", "scatter"), show_all = TRUE, ...)
```

## Arguments

- x:

  An `ssjgl_diff_edges` object from
  [`diff_edges()`](https://mljaniczek.github.io/spikeyglass/reference/diff_edges.md).

- which:

  Panels to draw: `"network"`, `"scatter"` or both (default).

- show_all:

  Logical. In the network, also draw (in light grey) edges that are
  non-zero in either group but not selected. Default `TRUE`.

- ...:

  Passed to [`plot`](https://rdrr.io/r/graphics/plot.default.html) for
  the scatter.

## Value

`x`, invisibly.
