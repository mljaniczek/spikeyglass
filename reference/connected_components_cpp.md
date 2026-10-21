# Connected components of an undirected graph (internal)

Breadth-first search on the symmetrised adjacency matrix; replaces
igraph::components() (weak connectivity) in the R EM path.

## Usage

``` r
connected_components_cpp(adj)
```

## Arguments

- adj:

  Logical p x p adjacency matrix

## Value

Integer vector of 1-based component labels, numbered in order of each
component's lowest-index node
