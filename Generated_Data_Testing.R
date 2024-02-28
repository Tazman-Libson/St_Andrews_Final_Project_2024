#Generate data from an arbitrary HMM: see if I can get good residuals from it
create_arb_2d_mod <- function(seed){
  set.seed(seed)
  arb_means <- matrix(runif(4,-10, 10), nrow =2 )
  arb_vars <- sqrt(runif(4, min = 1, max = 3))
  arb_vcv <- c(diag(arb_vars[1:2])%*%(symMat(runif(1), diag = F))%*%diag(arb_vars[1:2]),
               diag(arb_vars[1:2])%*%(symMat(runif(1), diag = F))%*%diag(arb_vars[1:2]))
  arb_vcv <- array(arb_vcv, dim = c(2,2,2))
  arb_vcv <- array(arb_vcv, dim = c(2,2,2))
  d <- runif(2, min= 0.5, max = 1)
  arb_tpm <- stan_starting_tpm(d)
  s1 <- runif(1)
  arb_id <- c(s1, 1-s1)
  
  arb_mod <- list(
    MEANS = arb_means,
    VCV = arb_vcv,
    ID = arb_id,
    TPM = arb_tpm
  )
  return(arb_mod)
}

arb_mod <- create_arb_2d_mod(1234)


arb_data <- mvn.generate_sample(200, arb_mod)

arb_nlm_fit_mod <- mvn.HMM_ml_mod_fit(create_arb_2d_mod(31415), arb_data, F)



arb_mod$VCV - arb_nlm_fit_mod$VCV
arb_mod$TPM
arb_nlm_fit_mod$TPM
arb_resids <- qnorm(mvn.cdf(arb_data, arb_nlm_fit_mod))
hist(arb_resids)
mean(arb_resids)
var(arb_resids)
#Test if variance and mean of pseudoresiduals are similarly N(-1, 1) distributed
single_rand_test <- function(seed, stationary =F){
  mod <- create_arb_2d_mod(seed)
  print(mod)
  data <- mvn.generate_sample(200, mod)
  return(data)
  initial_mod <- create_arb_2d_mod(seed+100000)
  fit_mod <-mvn.HMM_ml_mod_fit(initial_mod, data, stationary)
  #Get resids:
  if(length(unique(is.infinite(as.vector(fit_mod$VCV)))) == 1){
  #print(fit_mod)
  resids <- qnorm(mvn.cdf(data, fit_mod))
  return(c(var(resids), mean(resids), fit_mod$code, fit_mod$iterations))}
  else{
    print('whoopsies')
    return(
      single_rand_test(seed -10000)
    )
  }
}

as.integer(round(exp(log(1349501 + 16) + 10)))
N <- 100
resid_means_and_vars <- matrix(nrow = N, ncol = 4)


for(i in 100:N){
  print(i)
  resid_means_and_vars[i,] <- single_rand_test(1349507 + i)
}

good_data <-  single_rand_test(1349501 + 37, F)
bad_data <- single_rand_test(1349501 + 38, F)
bad_data2 <- single_rand_test(1349502 + 72)
bad_data3 <- single_rand_test(1349503 + 75)
single_rand_test(1349504 + 76)
single_rand_test(1349505 + 98)
single_rand_test(1349506 + 100)
weird_data <- mvn.generate_sample(200, create_arb_2d_mod(456))
weird_resids <- qnorm(mvn.cdf(weird_data, arb_nlm_fit_mod))
hist(weird_resids)
var(weird_resids)

single_rand_test_show_bug <- function(seed){ #seed at 1349501 + 16
  mod <- create_arb_2d_mod(seed)
  print(mod)
  data <- mvn.generate_sample(200, mod)
  initial_mod <- create_arb_2d_mod(seed+100000)
  fit_mod <-mvn.HMM_ml_mod_fit(initial_mod, data, F)
  #Get resids:
  print(fit_mod)
  resids <- qnorm(mvn.cdf(data, fit_mod))
  return(c(var(resids), mean(resids), fit_mod$code, fit_mod$iterations))
}


#Plotting the resids_means_and_variance
GenModData <- data.frame(
  vars = resid_means_and_vars[,1],
  means = resid_means_and_vars[,2],
  code = resid_means_and_vars[,3],
  iter = resid_means_and_vars[,4]
)

write.csv(GenModData, "~\\GenModData.csv", row.names=FALSE)
library(tidyverse)
genmodstats <-GenModData %>% filter(code < 4) %>%
  summarize(vm = mean(vars), mm = mean(means), vmed = median(vars), mmed = median(means))

ggplot(GenModData) +
  geom_histogram(aes(x = vars))

13


hist(bad_data3[,2])
hist(good_data[,2])
