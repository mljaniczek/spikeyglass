## code to prepare `simdat` dataset goes here

# usethis::use_data(simdat, overwrite = TRUE)
#
# library(BDgraph)
#
# set.seed(1)
# G <- 3
#
# ## Specify N, P
# p <- 50
# N <- rep(150, G)
# # smaller block dimension
# p0 <- p/10
# prep <- p/p0
#
# ## Generate network
# g <- NULL
# Graph <- matrix(0, p, p)
# for(i in 1:prep){
#   sub0 <- matrix(.C("scale_free", G = as.integer(matrix(0, p0, p0)), as.integer(p0),
#                     PACKAGE = "BDgraph")$G, p0, p0)
#   Graph[((i-1)*p0+1):(i*p0), ((i-1)*p0+1):(i*p0)] <- sub0
# }
# s1 <- Graph
# s2 <- s1
# i <- 1
# s2[((i-1)*p0+1):(i*p0), ((i-1)*p0+1):(i*p0)] <- 0
# s3 <- s2
# i <- 2
# s3[((i-1)*p0+1):(i*p0), ((i-1)*p0+1):(i*p0)] <- 0
# # graph.plot <- igraph::graph.adjacency(s1, mode = "undirected", diag = FALSE)
# # igraph::V(graph.plot)$color <- c(rep("red", p0), rep("blue", p0), rep("white", p-p0*2))
#
#
# # Visualization of the plot
# # set.seed(123)
# # igraph::plot.igraph(graph.plot, main = "Graph structure",
# # 			layout = layout.fruchterman.reingold,
# #             vertex.size = 2, vertex.label = NA, vertex.color=V(graph.plot)$color)
#
# g[[1]] <- s1
# g[[2]] <- s2
# g[[3]] <- s3
#
# # Simulate from G-Wishart
# Theta <- Sigma <- NULL
# result = .C("rgwish_c", as.integer(s1), as.double(chol(diag(p))),
#             K = as.double(matrix(0, p, p)), as.integer(3), as.integer(p), as.double(1E-8),
#             PACKAGE = "BDgraph")
# Theta[[1]] <- matrix(result$K, p, p)
# diag <- diag(Theta[[1]])
# Theta[[1]] <- Theta[[1]] * s1 + diag(diag)
# Sigma[[1]] <- solve(Theta[[1]])
# Sigma[[1]] <- cov2cor(Sigma[[1]])
# Theta[[1]] <- solve(Sigma[[1]])
# Theta[[1]] <- Theta[[1]] * s1 + diag(diag(Theta[[1]]))
#
# # look at summary statistics
# if(FALSE){
#   absvals <- cov2cor(Theta[[1]])
#   diag(absvals) <- 0
#   summary(abs(absvals[absvals!=0]))
#   sum(Theta[[1]]^2) - sum(diag(Theta[[1]])^2)
# }
#
# # Make Sigma correlation matrix
# Theta[[2]] <- Theta[[1]] * s2 + diag(diag(Theta[[1]]))
# Theta[[3]] <- Theta[[1]] * s3 + diag(diag(Theta[[1]]))
# Sigma[[2]] <- solve(Theta[[2]])
# Sigma[[3]] <- solve(Theta[[3]])
#
# # Simulate Data
# Y <- NULL
# Y[[1]] <- rmvnorm(n = N[1], mean = rep(0, p), sigma = Sigma[[1]])
# Y[[2]] <- rmvnorm(n = N[2], mean = rep(0, p), sigma = Sigma[[2]])
# Y[[3]] <- rmvnorm(n = N[3], mean = rep(0, p), sigma = Sigma[[3]])
#
# simdat <- Y
#
# usethis::use_data(simdat, overwrite = TRUE)
