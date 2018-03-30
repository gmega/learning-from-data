# ---- olsr ----

olsr <- function(X, y) {
  solve(t(X) %*% X) %*% t(X) %*% y
}

# ---- olsr-lm ----

olsr_lm <- function(X, y) {
  x <- X[,-1]
  lm(y ~ x)$coefficients
}

# ---- plot-olsr ----

plot_olsr <- function(D, w_final) {
  plot_dataset(D)
  abline(coef = w_final, col = 'green', lwd = 2)
}

# ---- plot-olsr-nl ----

plot_olsr_nl <- function(D, w_final, features, x_min, x_max, step) {
  y <- x <- seq(x_min, x_max, step)
  contour(
    x, y, outer(x, y, function(x, y) features(x, y) %*% w_final),
    add = TRUE,
    levels = c(0),
    col = 'green',
    lwd = 2
  )
}