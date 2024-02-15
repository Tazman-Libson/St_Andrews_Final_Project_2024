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

