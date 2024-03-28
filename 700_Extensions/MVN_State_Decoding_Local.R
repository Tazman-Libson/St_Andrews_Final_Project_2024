#Quick State Decoding things

### A.1.12 State prediction
# Note that state output 'statepreds' is a matrix even if h=1.
mvn.HMM.state_prediction <- function(h=1,x,mod)
{
  n_obs          <- dim(x)[1]
  la         <- mvn.lforward(x,mod)
  c          <- max(la[,n_obs])
  llk        <- c+log(sum(exp(la[,n]-c)))
  statepreds <- matrix(NA,ncol=h,nrow=mod$m)
  foo <- exp(la[,n]-llk)
  for (i in 1:h){
    foo<-foo%*%mod$TPM
    statepreds[,i]<-foo
  }
  return(statepreds)
}

### A.1.11 State probabilities
mvn.HMM.state_probs <- function(x,mod)
{
  n          <- dim(x)[1]
  m          <- dim(mod$TPM)[1]
  la         <- mvn.lforward(x,mod)
  lb         <- mvn.lbackward(x,mod)
  c          <- max(la[n,])
  llk        <- c+log(sum(exp(la[n,]-c)))
  stateprobs <- matrix(nrow= n,ncol=m)
  stateprobs[1, ] <- exp(la[1,]+lb[1,]-llk)
  print(stateprobs[1, ])
  for(i in 1:n){
    stateprobs[i,] <- exp(la[i,]+lb[i,]-llk)
    }
  return(stateprobs)
}
matrix(nrow = 100, ncol = 3)[100,]
tmatrix[100,]

### A.1.13 Local decoding
mvn.HMM.local_decoding <- function(x,mod)
{
  n          <- dim(x)[1]
  stateprobs <- mvn.HMM.state_probs(x,mod)
  ild        <- rep(NA,n)
  for (i in 1:n) ild[i]<-which.max(stateprobs[i,])
  ild
}


mvn.HMM.local_decoding(tmatrix, optim_mod)




