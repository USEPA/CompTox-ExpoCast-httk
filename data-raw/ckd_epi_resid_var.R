library(data.table)
library(EnvStats)

set.seed(42)

#We're going to try to back calculate the log-scale residual variability
#using the info on the natural scale residual variability given in the paper
#and the info on the eGFR distribution given in the paper

#eGFR distribution
#Table 4
# 3.7% (144) with eGFR < 15
# 12.1% (473) with GFR 15-29
# 33.2% (1295) with GFR 30-59
# 25.5% (992) with GFR 60-89
# 25.4% (989) with GFR >90

#this gives us a rough eCDF for eGFR

egfr_pct <- data.table(eGFR = c(15,29,59,89),
                       pct_bin = c(3.7,12.1,33.2,25.5))
egfr_pct[, pctile:=cumsum(pct_bin)/100]

#compute approximate CDF using approxfun with linear interpolation


# inverse CDF function: numerically solve approximate CDF
egfr_opt_fun <- function(x, y_spec, x_in, y_in){
  y_ecdf <- approx(x =x_in,
                         y = y_in ,
                     xout = x,
                         method = "linear",
                     rule = 2)$y
  return((y_spec - y_ecdf))
}

#inverse CDF: numerically solve CDF at a specified y value
egfr_inv_cdf <- function(y_spec, x_in, y_in){
  return(uniroot(f = egfr_opt_fun,
          interval = c(0,150),
          f.lower = egfr_opt_fun(0, y_spec, x_in, y_in),
          f.upper = egfr_opt_fun(150, y_spec, x_in, y_in),
          y_spec = y_spec,
          x_in = x_in,
          y_in = y_in)$root)
}

#add lower and upper bounds on eGFR based on visual inspection of Figure 1
x_in <-  c(0, egfr_pct$eGFR, 150)
y_in <- c(0.0, egfr_pct$pctile, 1)

#randomly draw values on unif(0,1)
rvals <- runif(3896, min = 0, max=1)
#back-convert using inverse CDF into draws from the approximated eGFR distribution
eGFR_samp <- sapply(rvals,
                    function(r) egfr_inv_cdf(y_spec = r,
                          x_in = x_in,
                          y_in = y_in))

#Now: Use EnvStates::rlnorm3() (3-paramter log-normal)
#to get draws from a 3-param log-normal dist
#where meanlog is log(eGFR)
#sdlog is a parameter to be fitted
#This distribution describes the natural-scale residuals
#from a log-scale fit that had zero mean and constant variance on the log scale

resid_draw <- function(sdlog, eGFR) {
  #residuals = measured GFR - estimated GFR
  resids <- EnvStats::rlnorm3(n=length(eGFR),
                                 meanlog = log(eGFR),
                                 sdlog = sdlog,
                                 threshold = -eGFR)
  mGFR <- resids + eGFR
  log_resids <- log(mGFR) - log(eGFR)
  #compute: median, IQR, P30, RMSE
  #note that I am pretty sure RMSE *has* to be of the log resids
  #or else the reported scale makes no sense at all.
  #P30 = percentage of eGFR values that are within 30% of measured GFR,
  #i.e. abs((measured - estimated)/measured) <= 0.3.
  return(c("median" = median(resids),
              "IQR" = iqr(resids),
               "P30" = 100*sum(abs(resids)/mGFR <= 0.3)/length(resids),
              "RMSE" = sqrt(mean(log_resids^2))))
  
}

#now, simultaneously optimize for median, IQR, and RMSE
#Here is the function to be minimized:
optim_fun <- function(sdlog, eGFR, nrep = 1000) {
  #do 1000 replicates and average,
  #to "average out" variability in the randomly sampled residuals
  mean(replicate(nrep,
            # sqrt(
  sum(
    (resid_draw(sdlog,
                eGFR) - c("median" = 2.5,
                          "IQR" = 16.6,
                          "P30" = 84.1,
                          "RMSE" = 0.250)
    )^2
  )
# )
))
}

optim_results <- optim(par = 0.2, #initial value for sdlog
      fn = optim_fun,
      eGFR = eGFR_samp,
      method = "L-BFGS-B",
      lower = 1e-3,
      upper = Inf)

#Calculated median, IQR, RMSE?
#just to show that they are close
resid_draw(optim_results$par,
           eGFR_samp)
#results are as follows:
# median         IQR         P30        RMSE 
# 0.04378443 16.50573123  0.81108830  0.23057456 

optim_results$par #the resulting sdlog is 0.2324888

#and just to show that 1000 replicates is probably sufficient
#do two different 1000-replicate runs of the function to be optimized
#and show that the distributions of each 1000-replicate run are pretty much the same
#i.e. the simulation is converged
foo <- replicate(1000,
sqrt(
sum(
(resid_draw(optim_results$par,
eGFR_samp) - c("median" = 2.5,
"IQR" = 16.6,
"P30" = 84.1,
"RMSE" = 0.250)
)^2
)
))
foo2 <- replicate(1000,
sqrt(
sum(
(resid_draw(optim_results$par,
eGFR_samp) - c("median" = 2.5,
"IQR" = 16.6,
"P30" = 84.1,
"RMSE" = 0.250)
)^2
)
))

#showing that these two have the same mean to 3 decimal places
mean(foo)
mean(foo2)

#showing that they are approximately normally distributed
#such that using the average of 1000 replicates makes sense
#as a measure of central tendency
hist(foo)
hist(foo2)

#qqplot
qqplot(foo, foo2)
#qqnorm again to test normality
qqnorm(foo)
qqline(foo)
qqnorm(foo2)
qqline(foo2)




