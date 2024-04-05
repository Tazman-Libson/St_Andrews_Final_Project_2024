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


#Function to easily create TPM matrices
stan_starting_tpm <- function(probs){
  m <- length(probs)
  probs_diag <- diag(probs)
  remain <- (rep(1,m) - probs)/(m-1)
  out <- (probs_diag - diag(remain) ) + remain
  return(out)
}
threshold <- function(val){
  if(abs(val) < 1.0e-16){
    return(0)
  }else{
    return(val)
  }
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


#function for fining stationary distribution from a given tpm
stat_dist <- function(tpm){
  m <- dim(tpm)[1]
  delta<-solve(t(diag(m)-tpm+1),rep(1,m))
  return(delta)
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



mvn.w2n <- function(params, m, n, stationary){
  #index 3 is start of tpm, which goes for 3 + m*(m-1)
  tpm_last <- (m*(m-1))
  corr_last <- tpm_last + (n*n - n*(n+1)/2)*m
  vars_last <- corr_last + m*n
  mns_last <- vars_last + m*n
  #Transistion Probability Matrix
  tpm <- params[1:tpm_last]
  TPM <- diag(m)
  TPM[!TPM] <- exp(tpm)
  TPM <- TPM/apply(TPM,1,sum)
  #Correlation Array:
  corr <- params[(tpm_last+1):corr_last]
  corr <- atan(corr)*2/pi
  CORR <- mvn.vec_to_ar(corr, n, m)
  #Variance Matrix:
  vars <- params[(corr_last + 1):vars_last]
  #print(vars)
  vars <- exp(vars)
  #print(vars)
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
    sig <- diag(mod$VARS[m,]) %*% mod$CORR[,,m] %*% diag(mod$VARS[m,])
    #print('sig')
    #print(sig)
    means <- mod$MEANS[m,]
    #print('means')
    #print(means)
    return(dmvnorm(X, mean = means, sigma = sig, checkSymmetry = F))
  }
  m <- dim(mod$TPM)[1]
  probs <- lapply(1:m, mvn)
  return(diag(probs))
}

