# ---- pla ----
pla_h <- function(X, w)  {
  sign(X %*% w)
}

pla_it <- function(X, z, w) {
  k <- which(pla_h(X, w) != z)
  if (is_empty(k)) {
    return(NULL)
  }
  k <- k[sample.int(length(k), 1)]
  w + z[k] * X[k,]
}

pla <- function(X, z, n = Inf, w = NULL) {
  w <- if (is.null(w)) rep(0, dim(X)[2]) else w
  c(list(as.list(w)), {
    w <- pla_it(X, z, w)
    if (!is.null(w) && n > 0) {
      pla(X, z, n - 1, w)
    }
  })
}

pla_line <- function(w) {
  coef <- c(intercept = -w[1]/w[3], slope = -w[2]/w[3])
  coef[is.nan(coef)] <- 0
  coef
}

# ---- pla-plot ----
plot_pla <- function(D, weights, last_only = TRUE) {
  h <- unlist(weights %>% tail(1))
  f <- D$w
  
  # First, plots the dataset.
  plot_dataset(D)
  
  # Then, the hypotheses.
  if (!last_only) {
    colors <- rep(alpha('darkgray', 0.5), length(weights))
    colors[1] <- alpha('orange', 0.5)
    colors[length(colors)] <- 'green'
    
    lwds <- rep(1, length(weights))
    lwds[1] <- lwds[length(weights)] <- 2
    
    for(wi in enumerate(weights) %>% as.list) {
      w <- unlist(wi$value)
      abline(coef = pla_line(w), col = colors[wi$index], lwd = lwds[wi$index])
    }
  } 
  
  # Plots only final hypothesis.
  else {
    abline(coef = pla_line(h), col = 'green', lty = 1, lwd = 2)
  }
  
  legend(
    'topright', 
    c(
      'target function', 
      'perceptron (convergence)', 
      'perceptron (initial)', 
      'perceptron (intermediate)'
    ), 
    col = c('red', 'green', 'orange', 'gray'), 
    lty = c(2, 1, 1, 1),
    lwd = 2
  )
}