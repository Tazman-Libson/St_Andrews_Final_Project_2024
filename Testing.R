
#Random code:
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

#Trying Different Starting Conditions to see if it helps

#fitting 2 state model
mean(tmatrix[,1]) #= avg return of apl



means <- example_returns %>%
  slice_head(n =100) %>%
  select(Apple, Microsoft, Intel, Meta) %>%
  reframe(a = quantile(Apple, c(0.75, 0.25)),
          m = quantile(Microsoft, c(0.75, 0.25)),
          i = quantile(Intel, c(0.75, 0.25)),
          me = quantile(Meta, c(0.75, 0.25)))



vcv2 <- array(c(as.vector(var(tmatrix[1:50,])),as.vector((var(tmatrix[51:100,])))),
      dim = c(4,4,2))

means_mat <- as.matrix(means)

mod2 <- list(
  MEANS = means_mat,
  VCV = vcv2,
  TPM = stan_starting_tpm(c(0.9, 0.9)),
  Stationary = T
)
testmod2 <- mvn.HMM_ml_mod_fit(mod2, tmatrix)

qqnorm(qnorm(mvn.conditional_v2(tmatrix, testmod2)))
testmod2


#Testing if Pseudoresiduals make sense:

nlm_gen_sample <- mvn.generate_sample(200, nlm_mod)

#Fit Model to Generated Data:

gen_mod <- mvn.HMM_ml_mod_fit(returns_tmod, nlm_gen_sample, T)
gen_mod
hist(qnorm(mvn.cdf(nlm_gen_sample, gen_mod)))
qqnorm(qnorm(mvn.cdf(nlm_gen_sample, gen_mod)))
mvn.conditional_v2(nlm_gen_sample, gen_mod)
qqline(qnorm(mvn.conditional_v2(nlm_gen_sample, gen_mod)))
gen_mod$VCV[,,3]
var(qnorm(mvn.cdf(nlm_gen_sample, gen_mod)))
nlm_mod$VCV[,,3]
?qqnorm
nlm_gen_sample[2,]
str(nlm_gen_sample)
str(tmatrix)
mvn.cumul_matrix(gen_mod, nlm_gen_sample[1,])


#Testing Time to run model fitting based on amount of data:
times <- matrix(NA, nrow = 20, ncol = 1)
for(i in 1:20){
  print("n =")
  print(10*i + 100)
  time_mat <- ret_matrix[1:(10*i+100),]
  time <- system.time(mvn.HMM_ml_mod_fit(returns_tmod, time_mat, T))
  print(time)
  times[i] <- time
}
profvis(mvn.HMM_ml_mod_fit(returns_tmod, tmatrix, T))


#Make an arbitrary model, Generate Data, Then run pseudoresidual functions
arb_mod <- list(
  MEANS <- 
)


arb_gen_data <-


