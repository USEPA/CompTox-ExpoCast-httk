#' Returns draws from a normal distribution with a lower censoring limit of lod
#' (limit of detection)
#' 
#' Returns draws from a normal distribution with a lower censoring limit of lod
#' (limit of detection)
#' 
#' 
#' @param n Number of samples to take
#' @param mean Mean of censored distribution. Default 0.
#' @param sd Standard deviation of censored distribution. Default 1.
#' @param lod Bound below which to censor. Default 0.005.
#' @param lower Lower bound on censored distribution. Default 0.
#' @param upper Upper bound on censored distribution. Default 1.
#
#' @return A vector of samples from the specified censored distribution.
#'
#' @import stats
#' @importFrom msm ptnorm
#' @importFrom msm rtnorm
#'
#' @export r_left_censored_norm
r_left_censored_norm <- function(n,
                                 mean=0,
                                 sd=1,
                                 lod=0.005,
                                      lower=0,
                                      upper=1)
{
  censored <- runif(n,0,1) < msm::ptnorm(lod,
                                        mean=mean,
                                        sd=sd,
                                        lower=lower,
                                        upper=upper)
  out <- rep(0,length=n)
  out[censored] <- runif(sum(censored),0,lod)
  out[!censored] <- msm::rtnorm(sum(!censored),mean=mean,sd=sd,lower=lod, upper=upper)
  return(out)
}
