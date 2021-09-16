rmed0non0u95 <- function(
  N, # Number of samples to draw,
  x.95, # Non-zero upper 95th percentile
  x.min = 0, # minimum allowed value
  x.LOD = 0.005 # limit of detection
  )
{
# A set a values above the 95th percentile:
  temp1 <- runif(N, x.95, x.LOD + 2*(x.95 - x.LOD))
# Initially draw N values between minimum and LOD: 
  out <- runif(N, x.min, x.LOD)
# replace 2.5% of the values with values above the u95:
  which.temp1 <- rbinom(N, 1, 0.025)==1
  out[which.temp1] <- temp1[which.temp1]

# A set of values between the LOD and the 95th percentile:
  temp2 <- runif(N, x.LOD, x.95)
# Replace 47.5% of the values with values btween LOD and 95th percentile:
  which.temp2 <- rbinom(N, 1, 0.5 - 0.025)==1
  out[which.temp2] <- temp2[which.temp2]

  return(out)
}



Fup.95 <- 0.02
N <- 1000

set.seed(1235)
Fup.vec <- rmed0non0u95(N, Fup.95)
quantile(Fup.vec,c(0.5,0.975))
 


