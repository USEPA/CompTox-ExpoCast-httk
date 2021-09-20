#' Draw random numbers with LOD median but non-zero upper 95th percentile
#' 
#' This function draws N random numbers from a distribution that approximates
#' a median that is equal to the limit of detection (LOD, value x.LOD) but has 
#' an upper 95th percentile (x.u95) that is above x.LOD. We make the assumption 
#' that values above x.u95 are uniformly distributed between x.u95 and x.u95 +
#' (x.u95 - x.LOD)
#' 
#' @param N Number of samples to draw
#' 
#' @param x.u95 The upper limit on the 95th confidence/credible intervale (this
#'   is the 97.5% quantile)
#' 
#' @param x.min The minimum allowed value (defaults to 0)
#' 
#' @param x.LOD The limit of detection (defaults to 0.005)
#' 
#' @return A vector of N samples where the 50th and 97.5th quantiles approximate
#'   x.LOD and x.u95 respectively
#' 
#' @author John Wambaugh
#' 
#' @references Breen et al., in preparation
#'
#' @example
#' Fup.95 <- 0.02
#' N <- 1000
#'
#' set.seed(1235)
#' Fup.vec <- rmed0non0u95(N, Fup.95)
#' quantile(Fup.vec,c(0.5,0.975))
#' 
#' @keywords httk-pop
rmed0non0u95 <- function(
  N, # Number of samples to draw,
  x.u95, # Non-zero upper 95th percentile
  x.min = 0, # minimum allowed value
  x.LOD = 0.005 # limit of detection
  )
{
# Initially draw N values between minimum and LOD: 
  out <- runif(N, x.min, x.LOD)

# A set a values above the 95th percentile:
  temp1 <- runif(N, x.u95, x.LOD + 2*(x.u95 - x.LOD))
# replace 2.5% of the values with values above the u95:
  which.temp1 <- rbinom(N, 1, 0.025)==1
  out[which.temp1] <- temp1[which.temp1]

# A set of values between the LOD and the 95th percentile:
  temp2 <- runif(N, x.LOD, x.u95)
# Replace 47.5% of the values with values btween LOD and 95th percentile:
  which.temp2 <- rbinom(N, 1, 0.5 - 0.025)==1
  out[which.temp2] <- temp2[which.temp2]

  return(out)
}




 


