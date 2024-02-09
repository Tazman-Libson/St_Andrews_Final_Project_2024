#Minus Log Likelihood Function 
#X: matrix with each column being a separate variable, each row an observation 
#of each variable at the same time (t x n), t = # of times observed, n = # of variables for multivariate norm
mvn.HMM_mllk <- function(parvec, X, m, n, stationary){
  mod <- mvn.w2n(parvec, m, n, stationary)
  #print(mod)
  tpm <- mod[[3]]
  #str(tpm)
  #print(tpm)
  t <- dim(X)[1] # number of observations
  #Calculating product of coefficients in order to prevent underflow
  phi <- mod[[4]]
  #str(phi)
  #print(phi)
  #print(phi %*% tpm)
  l <- 0 #log likelihood
  #Calculation is done recursively so have to be done with a for loop
  for(i in 1:t){
    v <- phi %*% tpm %*% mvn.p_matrix(mod, X[i,])
    u <- sum(v)
    l <- l + log(u)
    phi <- v/u #rescaled vector of forward probabilities
  }
  return(-l)
}

mvn.HMM_ml_mod_fit <- function(mod, data){
  n <- dim(mod$VCV)[2] # number of variables for multivariate norm
  m <- dim(mod$TPM)[1] # number of states
  stationary <- mod$Stationary
  parvec <- mvn.n2w(mod)
  print(parvec)
  print( mvn.w2n(parvec, m, n, stationary))
  fit <- nlm(mvn.HMM_mllk, parvec, X = data, n= n, m= m, stationary = stationary)
  print(fit)
  return(mvn.w2n(fit$estimate))
}

#Testing:
ret_matrix <- matrix(
  data = c(example_returns$Apple, example_returns$Microsoft, example_returns$Intel, example_returns$Meta),
  ncol = 4
)
#Give some more thought 
#LKJ Distribution

vcv_vec <- c(
  as.vector(diag(4)+0.1),
  as.vector(diag(4)*2+0.2),
  as.vector(diag(4)*3+0.3)
)

returns_tmod <- list(
  TPM = stan_starting_tpm(c(.9,.8,.7)),
  ID = NA,
  MEANS = matrix(0, nrow = 3, ncol = 4),
  VCV = array(data = vcv_vec, dim = c(4,4,3)),
  Stationary = TRUE
)

tmatrix <- ret_matrix[1:100,] #Smaller Matrix for small testing
returns_tmod[1]
mvn.HMM_ml_mod_fit(returns_tmod, tmatrix)
tvec <- mvn.n2w(returns_tmod, T)
tvecmod <- mvn.w2n(tvec, 3, 4, T)
mvn.HMM_mllk(tvec, ret_matrix, 3, 4, T)

mvnlktest <- nlm(mvn.HMM_mllk, tvec, m = 3, n = 4, stationary = T, X = tmatrix)
mvn.w2n(mvnlktest$estimate, m = 3, n = 4, stationary = T)
#Problems:
#TPM didn't change at all
#Most Means didn't change
#VCV didn't change at all except for the third 4x4 matrix, where every value changed
#Need to investigate using nlm for estimating so many parameters, could potentially be
#causing some problems. 


c(0.5454545, 0.2727273, 0.1818182) %*% tvecmod$TPM
tvecmod$TPM
returns_tmod$TPM
?nlm

str(tvecmod[[4]])
