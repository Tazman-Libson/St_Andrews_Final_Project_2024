#GOAL:
#Calculate conditional probabilities for each individual return, rather than the probability of
#the entire observation. Essentially marginalize over the other values.

#Support functions:

#Function to generate infinite lower and upper bounds except for a given index, where it
#replaces the observed value.
#obs_index is location of obs
#obs is the observation
#n is the length of the vector
gen_lbound <- function(obs_index, obs, n){
  lower <- rep(-Inf, n)
  lower[obs_index] <- obs
  return(lower)
}



#Function which provides the seperate probabilities of each individual return for each state.
#Returns a matrix
#mod is a model, X is a full observation (vector of legnth n)
mvn.cumul_mat <- function(mod, X){
  m <- dim(mod$TPM)[1]
  n <- dim(mod$CORR)[2]
  output <- matrix(nrow = m, ncol = n)
  prob_fun <- function(i){
    prob_sub_fun <- function(m){
      sig <- diag(mod$VARS[m,]) %*% mod$CORR[,,m] %*% diag(mod$VARS[m,])
      means <- mod$MEANS[m,]
      return(pmvnorm(lower = gen_lbound(i, X[i], n), upper = Inf, mean = means, sigma = sig))
    }
    probs <- sapply(1:m,  FUN =prob_sub_fun)
  }
  probs <- sapply(1:n,  FUN =prob_fun, simplify = "matrix")
  return(probs)
}

mvn.cumul_mat(optim_mod, tmatrix[5,])

mvn.cdf_new <- function(x,mod){
  lenx         <- dim(x)[1]
  m         <- dim(mod$TPM)[1]
  n <- dim(mod$CORR)[2]
  dxc       <- matrix(NA,nrow=lenx,ncol=n)
  Px        <- array(NA,dim = c(m, n, lenx))
  for (j in 1:lenx){ Px[,,j] <- mvn.cumul_mat(mod, x[j,])}
  la        <- mvn.lforward(x,mod)
  lb        <- mvn.lbackward(x,mod)
  la        <- rbind(log(mod$ID),la)
  lafact    <- apply(la,1,max)
  lbfact    <- apply(lb,1,max)
  for (i in 1:lenx)
  {
    foo      <- (exp(la[i,]-lafact[i])%*%mod$TPM)*exp(lb[i,]-lbfact[i])
    foo      <- foo/sum(foo)
    for(j in 1:n){
      dxc[i,j]  <- sum(Px[,j,i]%*%t(foo))
    }
  }
  return(dxc)
}

pseudoresids <- mvn.cdf_new(tmatrix, optim_mod)

hist(pseudoresids[,1:4])
mean(qnorm(pseudoresids[,1:4]))
