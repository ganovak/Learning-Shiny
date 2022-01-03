
#### Parameters ####

n <- 10L
p <- 0.25
simple <- TRUE

#### Calculate distribution ####

PDF <- vector(mode = "numeric", length = n + 1L)
  
for (k in 0:n) {
  PDF[k + 1] <- dbinom(k, n, p)
}

#### Visualize ####

if (simple) {
  barplot(PDF,
          names.arg = 0:n,
          main = paste0("Binom(", n, ", ", p, ")"),
          xlab = "k", ylab = "P(X = x)")
} else {
  barplot(PDF,
          names.arg = 0:n,
          main = paste0("Binom(n = ", n, ", p = ", p, ")"),
          xlab = "Number of sucesses", ylab = "Probability of observation")
}
