#Function that gives conditional probabilities of observations (xc), given all other observations(x)


mvn.conditional <- function(xc,x,mod){
  lenx        <- dim(x)[1]
  m         <- dim(mod$TPM)[1]
  nxc       <- dim(xc)[1]
  if(is.null(nxc)){nxc <- 1}
  dxc       <- matrix(NA,ncol=nxc,nrow=lenx)
  Px        <- matrix(NA,ncol=m,nrow=nxc)
  if(nxc == 1){Px[1,] <- diag(mvn.p_matrix(mod, as.vector(xc)))}else{
  for (j in 1:nxc){ Px[j,] <-diag(mvn.p_matrix(mod, xc[j,]))}}
  #print(Px)
  la        <- mvn.lforward(x,mod)
  lb        <- mvn.lbackward(x,mod)
  la        <- rbind(log(mod$ID),la)
  lafact    <- apply(la,1,max)
  #print(lafact)
  lbfact    <- apply(lb,1,max)
  for (i in 1:lenx)
  {
    foo      <-(exp(la[i,]-lafact[i])%*%mod$TPM)*exp(lb[i,]-lbfact[i])
    #print(foo)
    foo      <- foo/sum(foo)
    dxc[i,]  <- (foo)%*%t(Px)
    if(i == 4){
      #print(foo)
      #print(dxc[i,])
    }
  }
  return(dxc)
}

testcond <- mvn.conditional(tmatrix, tmatrix, nlm_mod)

mvn.pseudo_residuals <- function(x,mod)
{
  n        <- dim(x)[1]
  cdists   <- mvn.conditional(x,x,mod)
  cumdists <- apply(cdists,1,cumsum)
  return(cumdists)
  npsr     <- qnorm(cumdists)
  return(npsr)
}
?cumsum
ahhh <-mvn.pseudo_residuals(tmatrix, nlm_mod)

qqnorm(y = as.vector(ahhh))
?apply

max(tmatrix)




#Function that gives the conditional probability of observing each observation given the rest of the observations

mvn.conditional_v2 <- function(x, mod){
  la <- mvn.lforward(x, mod)
  lb <- mvn.lbackward(x, mod)
  cond_prod_single <- function(pos){
      
  }
}