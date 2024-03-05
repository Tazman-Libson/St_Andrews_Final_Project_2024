#Minus Log Likelihood Function 
#X: matrix with each column being a separate variable, each row an observation 
#of each variable at the same time (t x n), t = # of times observed, n = # of variables for multivariate norm
library(abind)

mvn.HMM_mllk <- function(parvec, X, m, n, stationary){
  mod <- mvn.w2n(parvec, m, n, stationary)
  if(!stationary){
    mod$ID <- sapply(mod$ID, FUN = threshold)
  }
  #if(runif(1) < 0.1){print(mod)}
  tpm <- mod$TPM
  t <- dim(X)[1] # number of observations
  phi <- mod$ID * diag(mvn.p_matrix(mod, X[1,]))
  l <- log(sum(phi)) #log likelihood
  phi <- phi/sum(phi)
  for(i in 2:t){
    v <- phi %*% tpm * diag(mvn.p_matrix(mod, X[i,]))
    u <- sum(v)
    l <- l + log(u)
    phi <- v/u #rescaled vector of forward probabilities
  }
  #if(runif(1) < 0.1){print(-l)}
  return(-l)
}



mvn.HMM_ml_mod_fit <- function(mod, data, stationary = T){
  n <- dim(mod$CORR )[2]# number of variables for multivariate norm
  m <- dim(mod$TPM)[1] # number of states
  parvec <- mvn.n2w(mod, stationary)
  #print(parvec)
  #print( mvn.w2n(parvec, m, n, stationary))
  fit <- optim(parvec, fn = mvn.HMM_mllk, X = data, m = m, n = n, stationary = T, control = list(maxit = 50000), method = 'BFGS')
  #print(fit)
  mod <- mvn.w2n(fit$par, m = m, n = n, stationary = stationary)
  mod$minimum <- fit$value
  mod$code <- fit$convergence
  mod$iterations <- fit$counts[1]
  return(mod)
}

#Testing:
ret_matrix <- matrix(
  data = c(example_returns$Apple, example_returns$Microsoft, example_returns$Intel, example_returns$Meta),
  ncol = 4
)
#Give some more thought 
#LKJ Distribution


corr <- abind(symMat(rep(0.25, 6), diag = F),
      symMat(rep(0.5, 6), diag = F),
      symMat(rep(0.75, 6), diag = F), along = 3)

corr
returns_tmod <- list(
  TPM = stan_starting_tpm(c(.9,.8,.7)),
  ID = NA,
  MEANS = matrix(0, nrow = 3, ncol = 4),
  CORR = corr,
  VARS = matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), byrow = T, nrow = 3, ncol = 4),
  Stationary = TRUE
)
mvn.n2w(returns_tmod, T)
mvn.w2n(mvn.n2w(returns_tmod, T),3,4, T)
returns_tmod
tmatrix <- ret_matrix[1:100,] #Smaller Matrix for small testing

mvnlktest <- mvn.HMM_ml_mod_fit(returns_tmod, tmatrix)
mvnlktest
nlm_mod <- mvnlktest
#Correlation MATRIX let it be negative
optim_test
optim_test <- optim(mvn.n2w(returns_tmod, T), fn = mvn.HMM_mllk, X = tmatrix, m = 3, n = 4, stationary = T, control = list(maxit = 50000))
optim_mod <- mvn.w2n(optim_test$par, 3, 4, T)

optim_test_diff <- optim(mvn.n2w(returns_tmod, T), fn = mvn.HMM_mllk, X = tmatrix, m = 3, n = 4, stationary = T, control = list(maxit = 50000), method = 'BFGS')
optim_mod_diff <- mvn.w2n(optim_test_diff$par, 3, 4, T)
optim_mod_diff <- mvn.HMM_ml_mod_fit(returns_tmod, tmatrix, T)

hist(mvn.cdf_new(tmatrix, nlm_mod))
hist(qnorm(mvn.cdf_new(tmatrix, nlm_mod)))
     