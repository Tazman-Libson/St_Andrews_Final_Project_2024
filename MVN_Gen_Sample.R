#Generating a dataset from a multivariate HMM:

mc1step <- function(x, tpm){
  n <- dim(tpm)[1]
  row <- tpm[x,]
  #print(row)
  rand <- runif(1)
  threshold <- 0
  for(index in 1:n){
    threshold <- threshold + row[index]
    #print(threshold)
    if(rand < threshold){
      #print(rand)
      return(index)
    }
  }
}

initial_state <- function(delta, tpm){
  n <- dim(tpm)[1]
  rand <- runif(1)
  threshold <- 0
  for(index in 1:n){
    threshold <- threshold + delta[index]
    #print(threshold)
    if(rand < threshold){
      #print(rand)
      return(index)
    }
  }
}

mvn.generate_single_obs <- function(state, mod){
  vcv <- mod$VCV[,,state]
  #print(vcv)
  means <- mod$MEANS[state,]
  #print(means)
  obs <- rmvnorm(1, mean = means, sigma = vcv)
  return(as.vector(obs))
}


mvn.generate_sample <- function(nobs, mod, seed = 138140){
  set.seed(seed)
  m <- dim(mod$TPM)[1]
  n <- dim(mod$VCV)[2]
  state <- initial_state(mod$ID, mod$TPM)
  sample <- matrix(nrow = nobs,  ncol = n)
  for(i in 1:nobs){
    sample[i,] <- mvn.generate_single_obs(state, mod)
    state <- mc1step(state, mod$TPM)
  }
  return(sample)
}


nlm_gen_sample <- mvn.generate_sample(200, nlm_mod)

#Fit Model to Generated Data:

gen_mod <- mvn.HMM_ml_mod_fit(returns_tmod, nlm_gen_sample, T)
