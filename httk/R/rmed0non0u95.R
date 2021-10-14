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
#'   is the 97.5 percentile)
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
#' Fup.vec <- rmed0non0u95(n=N, x.u95=Fup.95)
#' quantile(Fup.vec,c(0.5,0.975))
#' 
#' quantile(rmed0non0u95(200,x.u95=0.05,x.min=10^-4,x.LOD=0.01),c(0.5,0.975))
#' hist(rmed0non0u95(1000,x.u95=0.05,x.min=10^-4,x.LOD=0.01))
#'
#' quantile(rmed0non0u95(200,x.u95=0.005,x.min=10^-4,x.LOD=0.01),c(0.5,0.975))
#' hist(rmed0non0u95(1000,x.u95=0.005,x.min=10^-4,x.LOD=0.01))
#'
#' @keywords httk-pop
rmed0non0u95 <- function(
  n, # Number of samples to draw,
  x.u95, # Non-zero upper 95th percentile
  x.min = 0, # minimum allowed value
  x.LOD = 0.005 # limit of detection
  )
{
  # Check that x.u95 is non-zero:
  if (x.u95 == 0) warning("rmed0nonu95 called with x.u95 == 0")

  if (x.LOD == 0) stop("LOD can be small but not zero")
  
  # Check that x.u95 is greater than minimum:
  if (x.u95 < x.min)
  {
    # Otherwise, just replicate the minimum:
    out <- rep(x.min, times = n)
  } else {
    n.half <- floor(n/2)
    # Check to see if the u95 is higher than the LOD:
    if ( x.u95 > x.LOD)
    {
      # Ensure half are below LOD and half are above:
      below <- runif(n.half, x.min, x.LOD)
      above <- exp(runif(n.half, log(x.LOD),log(x.u95)))
      
      # Combine the two samples:
      out <- c(below,above)
      # Make sure we have n values (if n is odd)
      if (length(out) < n) out <- c(out,x.LOD)
      
      # Generate a set a values above the 95th percentile:
      temp1 <- runif(n, x.u95, x.u95 + (x.u95 - x.LOD))
    } else {
      # Ensure half are x.min and half are between x.min and x.u95:
      below <- rep(x.min, n.half)
      above <- exp(runif(n.half, log(x.min),log(x.u95)))
   
      # Combine the two samples:
      out <- c(below,above)
      # Make sure we have n values (if n is odd)
      if (length(out) < n) out <- c(out,x.min)

      # Generate a set a values above the 95th percentile:
      temp1 <- runif(n, x.u95, x.u95 + (x.u95 - x.min))
    }
    # Shuffle the values:
    out <- sample(out,n)
    # replace 2.5% of the values with values above the u95:
    which.temp1 <- sample(which(out > median(out)),0.05*n/2)
    out[which.temp1] <- temp1[which.temp1]
  }

  # Make sure we respecti x.min:
  out[out < x.min] <- x.min

  return(out)
}




 


