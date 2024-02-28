
mvn.cumul_vec <- function(mod, X){
  mvn <- function(m){
    sig <- mod$VCV[,,m]
    means <- mod$MEANS[m,]
    return(pmvnorm(lower = X, upper = Inf, mean = means, sigma = sig))
  }
  m <- dim(mod$TPM)[1]
  probs <- sapply(1:m,  FUN =mvn)
  return(probs)
}


library(mvtnorm)
pmvnorm()

mvn.cdf <- function(x,mod){
  lenx         <- dim(x)[1]
  m         <- dim(mod$TPM)[1]
  dxc       <- matrix(NA,nrow=lenx,ncol=1)
  Px        <- matrix(NA,nrow=lenx,ncol=m)
  for (j in 1:lenx){ Px[j,] <- mvn.cumul_vec(mod, x[j,])}
  #print(Px)
  la        <- mvn.lforward(x,mod)
  lb        <- mvn.lbackward(x,mod)
  la        <- rbind(log(mod$ID),la)
  lafact    <- apply(la,1,max)
  lbfact    <- apply(lb,1,max)
  for (i in 1:lenx)
  {
    foo      <- (exp(la[i,]-lafact[i])%*%mod$TPM)*exp(lb[i,]-lbfact[i])
    foo      <- foo/sum(foo)
    #if(i ==1){print(foo)}
    dxc[i]  <- sum(Px[i,]%*%t(foo))
  }
  return(dxc)
}

mvn.pdf <- function(x,mod){
  lenx         <- dim(x)[1]
  m         <- dim(mod$TPM)[1]
  dxc       <- matrix(NA,nrow=lenx,ncol=1)
  Px        <- matrix(NA,nrow=lenx,ncol=m)
  for (j in 1:lenx){ Px[j,] <- diag(mvn.p_matrix(mod, x[j,]))}
  la        <- mvn.lforward(x,mod)
  lb        <- mvn.lbackward(x,mod)
  la        <- rbind(log(mod$ID),la)
  lafact    <- apply(la,1,max)
  lbfact    <- apply(lb,1,max)
  for (i in 1:lenx)
  {
    foo      <- (exp(la[i,]-lafact[i])%*%mod$TPM)*exp(lb[i,]-lbfact[i])
    foo      <- foo/sum(foo)
    #if(i ==1){print(foo)}
    dxc[i]  <- sum(Px[i,]%*%t(foo))
  }
  return(dxc)
}

qqnorm(mvn.pdf(tmatrix, nlm_mod))
