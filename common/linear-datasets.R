# ---- linear-dataset-generators ----
generate_points <- function(n, smin, smax) {
  cbind(1, matrix(runif(2*n, smin, smax), ncol = 2)) 
}

generate_line <- function(smin, smax) {
  x <- runif(2, smin, smax)
  y <- runif(2, smin, smax)
  slope <- (y[2] - y[1]) / (x[2] - x[1])
  c(
    intercept = y[1] - slope * x[1],
    slope = slope
  )
}

generate_ls_dataset <- function(n, w = NULL, target = f) {
  X <- generate_points(n, -1, 1)
  w <- if (is.null(w)) generate_line(-1, 1) else w
  list(
    X = X,
    w = w,
    z = target(X, w)
  )
}

get_x <- function(X, no_bias = FALSE) X[, (no_bias + 1):(ncol(X) - 1), drop = FALSE]
get_y <- function(X) X[, ncol(X), drop = FALSE]

f <- function(X, w) {
  general_f(X, c(w, -1))
}

general_f <- function(X, w) {
  sign(X %*% w)
}

# ---- linear-dataset-plotting -----
plot_dataset <- function(D, colors = c('blue', 'black'), surface = linear_surface) {
  z <- D$z
  X <- D$X
  classes <- unique(z) %>% as.vector()
  for(cls in (enumerate(classes) %>% as.list())) {
    X_f <- X[z == cls$value, , drop = FALSE]
    x <- X_f[,2]
    y <- X_f[,3]
    args <- list(
      y ~ x,
      col = colors[[cls$index]],
      pch = 16,
      cex = 0.8
    )
    if (cls$index == 1) {
      args <- c(
        args,
        list(
          xlim = range(D$X[,2]), 
          ylim = range(D$X[,3])
        )
      )
      do.call(plot, args)
    } else {
      do.call(points, args)
    }
  }
  
  # Decision surface.
  if(!is.null(surface)) surface(D$w)
}

linear_surface <- function(w) {
  abline(coef = w, col = 'red', lty = 2, lwd = 2)
}