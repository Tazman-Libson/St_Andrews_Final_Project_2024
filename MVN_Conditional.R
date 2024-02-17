#Function that gives the conditional probability of observing each observation given the rest of the observations

mvn.conditional_v2 <- function(x, mod){
  m <- dim(mod$TPM)[1]
  lenx <- dim(x)[1]
  a <- exp(mvn.lforward(x, mod))
  b <- exp(mvn.lbackward(x, mod))
  cond_prob_single <- function(pos){
    if(pos == 1){
      num <- sum(mod$ID %*% mvn.cumul_matrix(mod, x[1,]) %*% b[1,])
      denom <- sum(mod$ID %*% b[1,])
      print(c(num, denom))
      return(num/denom)
    }
    num <- sum(a[pos - 1,] %*% mod$TPM %*% mvn.cumul_matrix(mod, x[pos,]) %*% b[pos,])
    denom <- sum(a[pos-1,]%*%mod$TPM %*%b[pos,])
    print(c(num, denom))
    return(num/denom)
  }
  print(sapply(1:10, FUN = cond_prob_single))
  condprobs <- matrix(sapply(1:lenx, FUN = cond_prob_single), nrow = lenx)
  return(condprobs)
}

whyyy <- mvn.conditional_v2(tmatrix[1:10,], nlm_mod)
str(nlm_gen_sample)
aa <- mvn.lforward(nlm_gen_sample, gen_mod)


mvn.cumul_vec <- function(mod, X){
  mvn <- function(m){
    sig <- mod$VCV[,,m]
    means <- mod$MEANS[m,]
    return(pmvnorm(lower = X, upper = Inf, mean = means, sigma = sig, algorithm = Miwa))
  }
  m <- dim(mod$TPM)[1]
  probs <- sapply(1:m, mvn)
  return(probs)
}
?pmvnorm
tmatrix
b <- mvn.conditional_v2(tmatrix, nlm_mod)
pseudoresids <- qnorm(mvn.conditional_v2(tmatrix, nlm_mod))
#Doesn't deviate terribly from normal, but definitely not standard normal distributed.
qqnorm(pseudoresids)
hist(pseudoresids)
hist(mvn.conditional_v2(tmatrix, nlm_mod))







#Running Into Underflow Problems:

mvn.conditional_v3 <- function(x, mod){
  m <- dim(mod$TPM)[1]
  lenx <- dim(x)[1]
  a <- mvn.lforward(x, mod)
  b <- mvn.lbackward(x, mod)
  cond_single_value <- function(pos){
    
  }
}

d_from_ab <- function(a, b, lenx){
  a
}
### A.1.9 Conditional probabilities
#==Conditional probability that observation at time t equals
#  xc, given all observations other than that at time t.
#  Note: xc is a vector and the result (dxc) is a matrix.
mvn.cdf <- function(x,mod){
  lenx         <- dim(x)[1]
  m         <- dim(mod$TPM)[1]
  dxc       <- matrix(NA,nrow=lenx,ncol=1)
  Px        <- matrix(NA,nrow=lenx,ncol=m)
  for (j in 1:lenx){ Px[j,] <- mvn.cumul_vec(mod, x[j,])}
  print(Px)
  la        <- mvn.lforward(x,mod)
  lb        <- mvn.lbackward(x,mod)
  la        <- rbind(log(mod$ID),la)
  lafact    <- apply(la,1,max)
  lbfact    <- apply(lb,1,max)
  for (i in 1:lenx)
  {
    foo      <- (exp(la[i,]-lafact[i])%*%mod$TPM)*exp(lb[i,]-lbfact[i])
    foo      <- foo/sum(foo)
    if(i ==1){print(foo)}
    dxc[i]  <- sum(Px[i,]%*%t(foo))
  }
  return(dxc)
}
a <-mvn.cdf(nlm_gen_sample, nlm_mod)

hist(a)
hist(qnorm(a))
hist(pseudoresids)
