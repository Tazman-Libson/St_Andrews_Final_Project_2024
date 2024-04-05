#Returns Description
#1
returns_cap <- 'Log returns for all 4 stocks for the first 50 days of the dataset. There are times when the returns rise and fall with eachother, and others where they diverge. This visually demonstrates the changing correlations of the return values. These changing correlations can be accounted for by HMMs.'
#2
marg_ap_pdfs_cap <- 'Marginalized state dependent distributions for the Apple stock from the fitted model. Although each state has a normal distribution, each one has a different mean and variance. For the technology stocks that were fitted, each state has a different 4-dimensional normal distribution.  See appendix A for specific model parameter values for the case study.'
#3
correlation_cap <- 'Graph of the natural correlations to working correlations. The correlation values can range from -1 to 1. The scaled tangent function maps these values onto the entire real line. Most natural correlation valuesare between -0.8 and 0.8.'
#4
marg_cdfs_diff_cap <- "Marginalized bivariate conditional probability distribution functions for Feb 2-5 2022. Each day has a different distribution. Here the colors are indicating the value of the conditional distribution at that point. On Feb 3, there is a massive drop in the Meta stock (see Figure 5), which has a significant impact on the following day's distribution. The full conditional distributions for the case study cannot be displayed because they are 4-dimensional."
#5
marg_cdfs_feb34_cap <- "Marginalized conditional univariate conditional pdfs for Meta and Intel for Feb 3-4 2022. The black lines indicate the observed return values for that day. Once again can see the impact of the Meta stock drop where the variance of the univariate conditional pdfs both increase for the following day."
#6
presid_dem_cap <- "Demonstation of pseudo-residuals for a univariate model. The solid and dashed lines represent observations as they are transformed into uniform pseudo-residuals by taking the conditional cdf of the values. This is expected to be uniformly distributed. Then the inverse normal is taken to get normal pseudo-residuals. Notice how the blue/dashed line and the red/solid line are quite far apart in the conditional distribution. The dashed line is significantly less likely than the solid line. In the uniform pseudo-residuals they do not have as much distance between them. For the normal pseudo-residuals, the two lines are farther apart, and thus more distinguishable"
#8
fulldataset_cap <- "Magnitude of returns for the full dataset plotted against time. The color scale is the magnitude of the element and vector normal pseudo-residuals. From our definition of what outliers are for the returns data, the element pseudo-residuals are significantly better than the vector pseudo-residuals at outlier detection. Draw particular attention to the large magnitude returns with high element pseudo-residuals, but small vector pseudo-residuals. Some of these instances are around day 790 for Meta and around day 375 for Intel. The Meta return is the largest stock drop in Meta's history. It has the largest element pseudo-residual, but it does not have a particularly large vector pseudo-residual. The color scale here is turbo from the `viridis` package, which is a library of colorblind friendly color palletes."

#9
day200400_cap <- "Magnitude of returns for Nov 18 2019 - Sept 3 2020. The magnitude of the element pseudo-residuals seems to increase with the magnitude of the returns. One can see this clearly in the range of return values between day number 250-300. The clear color gradient that was expected (see Figure 7) is seen here for the element pseudo-residuals. Vector pseudo-residuals do not seem to display as strong of a relationship where there is a mix of magnitudes throughout the returns."
#Not used:
day600800_cap <- "Magnitude of returns for June 22 2021 - April 6 2022. There are a couple particularly large magnitude returns for which the magnitude of the element pseudo-residual is also large, but the magnitude of the vector pseudo-residual is not particularly high. Most notably around day number 750, which is the large Meta stock drop on Feb 3 2022."
#10
lmresids_cap <- "Scatterplot of the normal pseudo-residuals and stock return values seperated by stock with linear models plotted with the normal pseudo-residual as the response variable. The 99% standard error regions are plotted for each line as the gray region for each line. The slope for the element pseudo-residuals is greater for all 4 stocks than the vector pseudo-residuals, meaning that an increase in the magnitude of return is expected to have a greater impact on the element pseudo-residuals than the vector pseudo-residuals. In addition to a greater slope, the element pseudo-residuals seem to fit the linear model a lot better than the vector pseudo-residuals. The element pseudo-residuals are more clustered around the line, wheras the vector pseudoresiduals are more disperse. "
#11
elresdens_cap <- "Denisites for the element pseudo-residuals for each stock. Although they do not conform perfectly to the standard normal, they are close. The variance is slightly too small so the peak is higher than that of the standard normal. However, this is not to show that the model fitted for the case study is perfectly well fitted, it is rather to show element pseudo-residuals are exhibiting behavior expected of pseudo-residuals for univariate models."
#12
vecresdens_cap <- "Density plot for the vector pseudo-residuals. Density also appears to be normal, but the mean is shifted left from where one would expect for pseudo-residuals. The normal shape is expected because of the use of the inverse standard normal. The relevent indicator here is whether or not the normal pseudo-residuals are standard normal, not just that they are normally distributed. For the vector pseudo-residuals, there is clear deviation from the standard normal. The mean is significantly far away from 0. Once again vector pseudo-residuals are not behaving like pseudo-residuals for univariate models."
#13
gendatahist_cap <- "Histograms for statistics of pseudo-residuals of generated data trials. Here the mean and variance of the element and vector pseudo-residuals are used as tests for conformation to the standard normal. The statistics for the element pseudo-residuals indicate that they are predominantly in line with standard normal. There is not a clear conclusion from the vector pseudo-residual statistics aside that they are not standard noraml."
#14
vecgenden_cap <- "Densities for Means and Variances of vector pseudo-residuals for the generated data trials. Once again there does not seem to be a clear pattern for these values."
 
#7
outliercap <- "Demonstration of expected behaviour for good and bad outlier identification. For the case study, large magnitude returns are consisdered outliers. Large magnitude pseudo-residuals label an observation as an outlier. So for good outlier identification, we expect as the magnitude of the observation increases, so would the magnitude of the pseudo-residual. For the bad outlier identification example, the pseudo-residual magnitude is uniformly randomly distributed, so actual outliers are not identified as outliers by the pseudoresidual." 

get_date <- function(num){
  return(filter(resid_df, day_num == num)$Date)
}
