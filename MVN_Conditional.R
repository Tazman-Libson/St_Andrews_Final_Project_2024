
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

qqnorm(mvn.cdf(tmatrix, optim_mod))
hist(qnorm(mvn.cdf(tmatrix, optim_mod)))
var(qnorm(mvn.cdf(tmatrix, optim_mod)))
a <- qnorm(mvn.cdf(tmatrix, optim_mod))
    mvn.cdf(tmatrix, optim_mod)    

mvn.p_matrix(optim_mod, tmatrix[52,])
mvn.lforward(tmatrix,optim_mod)
mvn.lbackward(tmatrix,optim_mod)

sig <- diag(optim_mod$VARS[2,]) %*% optim_mod$CORR[,,2] %*% diag(optim_mod$VARS[2,])
print('sig')
print(sig)
means <- optim_mod$MEANS[2,]
print('means')
print(means)
dmvnorm(tmatrix[52,], mean = means, sigma = sig, checkSymmetry = F)

tmatrix[52,]

