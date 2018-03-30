summary.list <- function(X) {
  as.tibble(t(unclass(summary(unlist(X)))))
}