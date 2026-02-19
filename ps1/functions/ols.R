
ols <- function(x, y, intercept = FALSE) {
  
  X <- as.matrix(x)
  Y <- as.matrix(y)

  if(intercept) {
    intercept_vector <- matrix(1, nrow(X))
    X <- cbind(intercept_vector, X)
  }


  N <- nrow(X)
  k <- ncol(X)
  b_ols <- solve(t(X)%*%X) %*% (t(X)%*%Y) # b = (X'X)^{-1}(X'Y)
  y_hat <- (X) %*% b_ols
  
  r2_uc <- (t(y_hat)%*%y_hat) / (t(Y)%*%Y)
  r2 <- t(y_hat-mean(Y))%*%(y_hat-mean(Y)) / t(Y-mean(Y))%*%(Y-mean(Y))
  r2 <- r2[[1,1]]
  r2_bar <- 1 - ((N-1)/(N-k))*(1-r2)

  e <- Y-y_hat
  s2 <- (t(e)%*%e)/(N-k)



  list(
    beta = b_ols,
    X = X,
    Y = Y,
    N = N,
    k = k,
    df = N-k,
    r2_uc = r2_uc,
    r2 = r2,
    r2_bar = r2_bar,
    y_hat = y_hat,
    e = e,
    s2 = s2
  )
}