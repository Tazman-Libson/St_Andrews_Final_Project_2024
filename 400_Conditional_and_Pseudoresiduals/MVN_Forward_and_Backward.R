#Forward and Backward Probabilities:


# log forward probabilities 
#framework from Hidden Markov Models for Time Series: An Introduction Using R
mvn.lforward<-function(x, mod){
  m <- dim(mod$TPM)[1]
  lenx <- dim(x)[1] #number of observations
  lalpha        <- matrix(NA,ncol = m, nrow = lenx)
  foo           <- mod$ID %*% mvn.p_matrix(mod, X = x[1,])
  sumfoo        <- sum(foo)
  lscale        <- log(sumfoo)
  foo           <- foo/sumfoo
  lalpha[1,]    <- lscale+log(foo)
  for (i in 2:(lenx))
  {
    foo          <- foo%*%mod$TPM %*% mvn.p_matrix(mod, X = x[i,])
    sumfoo       <- sum(foo)
    lscale       <- lscale+log(sumfoo)
    foo          <- foo/sumfoo
    lalpha[i,]   <- log(foo)+lscale
  }
  return(lalpha)
}


#Computing log backward probabilities
mvn.lbackward<-function(x,mod)
{
  lenx          <- dim(x)[1]
  m          <- dim(mod$TPM)[1]
  lbeta      <- matrix(NA,ncol = m,nrow = lenx)
  lbeta[lenx,]  <- rep(0,m)
  foo        <- rep(1/m,m)
  lscale     <- log(m)
  for (i in (lenx-1):1)
  {
    foo        <- mod$TPM%*%mvn.p_matrix(mod, x[i+1,])%*%foo
    lbeta[i,]  <- log(foo)+lscale
    sumfoo     <- sum(foo)
    foo        <- foo/sumfoo
    lscale     <- lscale+log(sumfoo)
  }
  return(lbeta)
}

