# ---- g ----

g <- function(w) {
  function(x) {
    x %*% w
  }
}

# ---- w-hat ----

w_D <- function(D, features = identity) {
  X <- D()
  olsr(features(get_x(X)), get_y(X))
}

ŵ <- function(w_D, n = 10000) {
  Reduce(`+`, lapply(1:n, function(x) w_D()))/n
}

# ---- bias ----

bias <- function(f, g_bar, x) {
  mean(sapply(x, function(x_i) (f(x_i) - g_bar(x_i))**2))
}

# ---- variance ----

variance <- function(g_bar, g_D, n = 1000, ...) {
  mean(
    sapply(1:n, function(y) { bias(g_D(), g_bar, ...) })
  )
}

