# Kalman Filter ----------------------------------------------------
get_cmp <- function(x) {
  x[x < 0] <- NA
  k <- dim(x)[2] - 1
  n <- dim(x)[1]
  levels <- matrix(0, n, k, dimnames = list(NULL, names(x)[-1]))
  rates <- matrix(0, n, k, dimnames = list(NULL, names(x)[-1]))
  for (i in 1:k) {
    y <- as.numeric(log(1 + x[[1 + i]]))
    mod <- SSModel(y ~ SSMtrend(2, list(0, NA)) + SSMseasonal(7, NA),
                   H = NA
    )
    vy <- var(diff(y), na.rm = TRUE)
    fit <- fitSSM(mod, log(c(vy / 100, vy / 100, vy / 100)))
    kfs <- KFS(fit$model, smoothing = "state")
    levels[, i] <- as.numeric(exp(kfs$alphahat[, 1] + 0.5 * kfs$V[1, 1, ])) - 1
    levels[levels < 0] <- 0
    rates[, i] <- (levels[, i] - lag(levels[, i])) / lag(levels[, i]) #as.numeric(kfs$alphahat[, 2])
  }
  list(
    livello = as_tibble(data = x[[1]], levels),
    crescita = as_tibble(data = x[[1]], rates * 100)
  )
}
