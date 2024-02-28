#Fitting Multivariate Model
library(mvtnorm)
library(matlib)
#mod is a list with the following elements:
#m: number of states (not an element of list)
#n: dimension of the normal distribution (not an element of list)
#MEANS <- matrix (m x n) of means for the mvt
#VARS <- matrix (m x n) matrix of variances. Each row are the variances for each state
#CORR <- 3D(n x n x m) array of correlation matrices
#TPM <- transition probability matrix (m x m)
#ID <- initial distribution vector (m)
#Stationary <- Boolean, if true, the ID will be taken as the initial distribution


#Function to easily create TPM matrices
stan_starting_tpm <- function(probs){
  m <- length(probs)
  probs_diag <- diag(probs)
  remain <- (rep(1,m) - probs)/(m-1)
  out <- (probs_diag - diag(remain) ) + remain
  return(out)
}


#Need to make a function that takes the VCV array and turns it into the relevant parameters:
#ar is a VCV as described above
mvn.ar_to_vec <- function(ar){
  #Sub function in order to use sapply will take the square matrix in each index of VCV and
  #return a vector of the upper triangular elements
  symmat_to_vec <- function(s){
    return(ar[,,s][upper.tri(ar[,,s], diag = F)])
  }
  m <- dim(ar)[3] #number of states
  t <- sapply(1:m, symmat_to_vec)
  as.vector(t)
}
test_array <- array(1:48, dim = c(4,4, 3))
test_vec <- mvn.ar_to_vec(test_array)

#Function to turn vector into array:
mvn.vec_to_ar <- function(vector, n, m){
  vecs <- matrix(vector, ncol = (n*(n-1)/2), nrow = m, byrow = T)
  fun <- function(s){
    return(as.vector(symMat(vecs[s,], byrow = T, diag = F)))
  }
  dat <- sapply(1:m, FUN = fun)
  dat <- as.vector(dat)
  dat <- array(data = dat, dim = c(n,n,m))
  return(dat)
}

mvn.vec_to_ar(test_vec, 4, 3)
threshold <- function(val){
  if(abs(val) < 1.0e-16){
    return(0)
  }else{
    return(val)
  }
}



#In Order to use nlm, need a function that doesn't have restrictions on it's parameters
#The means aren't restricted
#Correlation matrix is symmetric with values between -1 and 1, diagonal has 1s. 
#
#TPM row values must sum to 1 and are also non-negative. Means that tpm has only m(m-1) parameters to estimate

mvn.n2w <- function(mod, stationary){
  #Reparameterization for tpm:
  tpm <- mod$TPM
  m <- dim(tpm)[1]
  tpm <- log(tpm/diag(tpm))
  tpm <- as.vector(tpm[!diag(m)])
  #Initial Distribution:
  if(stationary == F){
    id <- mod$ID
    id<-log(id[-1]/id[1])
  }
  #correlation:
  corr <-mod$CORR
  #n <- dim(corr)[2]
  corr <- mvn.ar_to_vec(corr) #turn ar into vector
  corr <- tan(corr*pi/2) #take tan
  #variances:
  vars <- mod$VARS
  vars <- as.vector(vars)
  vars <- log(vars)
  #means (don't need reparam, just vectorisation)
  mns <- as.vector(mod$MEANS)
  params <- c(tpm, corr, vars, mns)
  if(!stationary){
    params <- c(params, id)
  }
  return(params)
}


#function for fining stationary distribution from a given tpm
stat_dist <- function(tpm){
  m <- dim(tpm)[1]
  delta<-solve(t(diag(m)-tpm+1),rep(1,m))
  return(delta)
}

mvn.w2n <- function(params, m, n, stationary){
  #index 3 is start of tpm, which goes for 3 + m*(m-1)
  tpm_last <- (m*(m-1))
  corr_last <- tpm_last + (n*(n-1)/2)*m
  vars_last <- corr_last + m*n
  mns_last <- vars_last + m*n
  #Transistion Probability Matrix
  tpm <- params[1:tpm_last]
  TPM <- diag(m)
  TPM[!TPM] <- exp(tpm)
  TPM <- TPM/apply(TPM,1,sum)
  #Correlation Array:
  corr <- params[(tpm_last+1):corr_last]
  corr <- tan(corr)*2/pi
  CORR <- mvn.vec_to_ar(corr, n, m)
  #Variance Matrix:
  vars <- params[(corr_last + 1):vars_last]
  vars <- exp(vars)
  VARS <- matrix(vars, nrow = m, ncol  = n)
  #Means:
  means <- params[(vars_last+1):mns_last]
  MEANS <- matrix(means, nrow = m, ncol = n, byrow = F)
  if(stationary){
    ID <- stat_dist(TPM)
  }else{
    id <- tail(params, n = (m-1))
    foo<-c(1,exp(id))
    ID<-foo/sum(foo)
  }
  return(
    list(
      MEANS = MEANS,
      CORR = CORR,
      VARS = VARS,
      TPM = TPM,
      ID = ID
    )
  )
}




#Function for state_dependent distribution probability matrix
#Inputs:
#mod <- list as described above
#X <- vector of legnth n with the observations at a single time
#Outputs:
#an m x m diagnonal matrix with the marginal probability for each state-dependent distribtion
mvn.p_matrix <- function(mod, X){
  mvn <- function(m){
    sig <- mod$VCV[,,m]
    means <- mod$MEANS[m,]
    return(dmvnorm(X, mean = means, sigma = sig, checkSymmetry = F))
  }
  m <- dim(mod$TPM)[1]
  probs <- lapply(1:m, mvn)
  return(diag(probs))
}

create_arb_2d_mod <- function(seed){
  set.seed(seed)
  arb_means <- matrix(runif(4,-10, 10), nrow =2 )
  arb_vars <- sqrt(runif(4, min = 1, max = 3))
  arb_vars <- matrix(arb_vars, nrow =2 )
  arb_corr <- c(symMat(runif(1, min = -1), diag = F),
              symMat(runif(1, min = -1), diag = F))
  arb_corr <- array(arb_corr, dim = c(2,2,2))
  d <- runif(2, min= 0.5, max = 1)
  arb_tpm <- stan_starting_tpm(d)
  s1 <- runif(1)
  arb_id <- c(s1, 1-s1)
  
  arb_mod <- list(
    MEANS = arb_means,
    CORR = arb_corr,
    VARS = arb_vars,
    ID = arb_id,
    TPM = arb_tpm
  )
  return(arb_mod)
}

create_arb_2d_mod(123)

test_mod <- create_arb_2d_mod(123)

test_vec <- mvn.n2w(test_mod, F)

mvn.w2n(test_vec, 2, 2, F)
test_mod


