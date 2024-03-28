#Generate data from an arbitrary HMM: see if I can get good residuals from it
library(matlib)
source('Data_Wrangling.R')
source('MVN_support_functions.R')
source('MVN_Forward_and_Backward.R')
source('MVN_single_Pseudoresids.R')
source('MVN_Conditional.R')
source('MVN_ML_Model_Fitting.R')
create_arb_2d_mod <- function(seed){
  set.seed(seed)
  arb_means <- matrix(runif(4,-1, 1), nrow =2 )
  arb_vars <- runif(4, min = 1, max = 10)
  arb_vars <- matrix(arb_vars, nrow =2 )
  arb_corr <- c(symMat(runif(1, min = -1), diag = F),
                symMat(runif(1, min = -1), diag = F))
  arb_corr <- array(arb_corr, dim = c(2,2,2))
  d <- runif(2, min= 0.5, max = 1)
  arb_tpm <- stan_starting_tpm(d)
  s1 <- runif(1)
  arb_id <- c(s1, 1-s1)
  
  arb_mod <- list(
    MEANS = arb_means,
    CORR = arb_corr,
    VARS = arb_vars,
    ID = arb_id,
    TPM = arb_tpm
  )
  return(arb_mod)
}

create_arb_2d_mod(123)
#Test if variance and mean of pseudoresiduals are similarly N(-1, 1) distributed
single_rand_test <- function(seed, stationary =T, print = F){
  mod <- create_arb_2d_mod(seed)
  data <- mvn.generate_sample(200, mod)
  hist(data[,1])
  hist(data[,2])
  initial_mod <- create_arb_2d_mod(seed+100000)
  fit_mod <-mvn.HMM_ml_mod_fit_nlm(initial_mod, data, stationary, print = print)
  #Get resids:
  vector_resids <- qnorm(mvn.cdf(data, fit_mod))
  element_resids <- qnorm(mvn.cdf_new(data, fit_mod))
  vr_var <- var(vector_resids)
  print(vr_var)
  vr_mn <- mean(vector_resids)
  el_var <- var(as.vector(element_resids))
  el_mean <- mean(as.vector(element_resids))
  print(el_mean)
  print(el_var)
  return(c(vr_var, vr_mn, el_var, el_mean, fit_mod$code, fit_mod$iterations))
}


N <- 200
resid_means_and_vars_2 <- matrix(nrow = N, ncol = 6)
resid_means_and_vars_2 <- rbind(resid_means_and_vars, matrix(NA, nrow = 32, ncol = 6))

for(i in 1:N){
  print(i)
  resid_means_and_vars[i,] <- single_rand_test(114548 + i)
}


single_rand_test(1145561 + i)


