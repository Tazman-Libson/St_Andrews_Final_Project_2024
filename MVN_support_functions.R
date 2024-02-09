#Fitting Multivariate Model
library(mvtnorm)
library(matlib)
#mod is a list with the following elements:
#m: number of states (not an element of list)
#n: dimension of the normal distribution (not an element of list)
#MEANS <- matrix (m x n) of means for the mvt
#VCV <- 3D(n x n x m) array of variance/covariance matrices
#TPM <- transition probability matrix (m x m)
#ID <- initial distribution vector (m)
#Stationary <- Boolean, if true, the ID will be taken as the initial distribution

#Need to make a function that takes the VCV array and turns it into the relevant parameters:
#ar is a VCV as described above
mvn.ar_to_vec <- function(ar){
  #Sub function in order to use sapply will take the square matrix in each index of VCV and
  #return a vector of the upper triangular elements
  symmat_to_vec <- function(s){
    return(ar[,,s][upper.tri(ar[,,s], diag = TRUE)])
  }
  m <- dim(ar)[3] #number of states
  t <- sapply(1:m, symmat_to_vec)
  as.vector(t)
}

#Function to turn vector into array:
mvn.vec_to_ar <- function(vector, n, m){
  vecs <- matrix(vector, ncol = (n*n -(n*(n-1)/2)), nrow = m, byrow = T)
  fun <- function(s){
    return(as.vector(symMat(vecs[s,], byrow = T)))
  }
  dat <- sapply(1:m, FUN = fun)
  dat <- as.vector(dat)
  dat <- array(data = dat, dim = c(n,n,m))
  return(dat)
}


#In Order to use nlm, need a function that doesn't have restrictions on it's parameters
#The means aren't restricted
#Variance and covariance matrix is non-negative and symmetric. 
#TPM row values must sum to 1 and are also non-negative. Means that tpm has only m(m-1) parameters to estimate

mvn.n2w <- function(mod, stationary){
  #Reparameterization for tpm:
  tpm <- mod$TPM
  m <- dim(tpm)[1]
  stationary <- mod$Stationary
  tpm <- log(tpm/diag(tpm))
  tpm <- as.vector(tpm[!diag(m)])
  #Initial Distribution:
  if(!stationary){
    id <- mod$ID
    id<-log(id[-1]/id[1])
  }
  #var/covariances:
  vcv <-mod$VCV
  n <- dim(vcv)[2]
  vcv <- mvn.ar_to_vec(vcv) #turn ar into vector
  vcv <- log(vcv) #take log
  #means (don't need reparam, just vectorisation)
  mns <- as.vector(mod$MEANS)
  params <- c(tpm, vcv, mns)
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
  vcv_last <- tpm_last + m*n*n - m*n*(n-1)/2
  mns_last <- vcv_last + m*n
  tpm <- params[1:tpm_last]
  TPM <- diag(m)
  TPM[!TPM] <- exp(tpm)
  TPM <- TPM/apply(TPM,1,sum)
  vcv <- params[(tpm_last+1):vcv_last]
  vcv <- exp(vcv)
  VCV <- mvn.vec_to_ar(vcv, n, m)
  means <- params[(vcv_last+1):mns_last]
  MEANS <- matrix(means, nrow = m, ncol = n, byrow = T)
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
      VCV = VCV,
      TPM = TPM,
      ID = ID
    )
  )
}

stat_dist(stan_starting_tpm(c(.9,.8)))
#testmod
tmod<- list( 
  TPM =stan_starting_tpm(c(.9,.8)),
  MEANS = matrix(c(3,4,5,6,7,8), nrow = 2, ncol =3, byrow = T),
  ID = NA,
  VCV = array(1:18, dim = c(3,3,2)),
  Stationary = TRUE
)
tmod
tparams <-mvn.n2w(tmod)
tmod2 <- mvn.w2n(tparams, 2, 3, T)
tmod2$TPM

tmod
#Function for state_dependent distribution probability matrix
#Inputs:
#mod <- list as described above
#X <- vector of legnth n with the observations at a single time
#Outputs:
#an m x m diagnonal matrix with the marginal probability for each state-dependent distribtion
mvn.p_matrix <- function(mod, X){
  mvn <- function(state){
    sig <- mod$VCV[,,m]
    means <- mod$MEANS[m,]
    return(dmvnorm(X, mean = means, sigma = sig))
  }
  m <- dim(mod$TPM)[1]
  probs <- sapply(1:m, mvn)
  return(diag(probs))
}

#Function to easily create TPM matrices
stan_starting_tpm <- function(probs){
  m <- length(probs)
  probs_diag <- diag(probs)
  remain <- (rep(1,m) - probs)/(m-1)
  out <- (probs_diag - diag(remain) ) + remain
  return(out)
}


arrrrr <- array(rep(as.vector(diag(3)),2), dim = c(3,3,2))
tx <- c(4,5,6)
mvn.p_matrix(mvn.w2n(tparams, 2, 3, T),tx)
pmtestmod <- list(
  TPM = stan_starting_tpm(c(.9,.9)),
  ID = NA,
  MEANS = matrix(c(0,0,0,1,2,3),nrow = 2, ncol = 3),
  VCV = arrrrr
)

mvn.p_matrix(pmtestmod, c(0,0,0))
