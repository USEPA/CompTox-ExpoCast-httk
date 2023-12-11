#' Randomly draws from a one-dimensional KDE
#' 
#' Randomly draws from a one-dimensional KDE
#' 
#' 
#' @param n Number of samples to draw
#' @param fhat A list with elements x, w, and h (h is the KDE bandwidth).
#' @return A vector of n samples from the KDE fhat
#' @author Caroline Ring
#' @references 
#' \insertRef{ring2017identifying}{httk}
#' @keywords httk-pop
#' @import stats
#' @export rfun
rfun <- function(n,
                 fhat){
  tmp <- fhat$x[sample(seq_along(fhat$x), 
                       size=n, 
                       replace=TRUE, 
                       prob=fhat$w)] + 
    fhat$h*rnorm(n=n, mean=0, sd=1)
  return(tmp)
}





#' Checks whether a value, or all values in a vector, is within inclusive
#' limits
#' 
#' Checks whether a value, or all values in a vector, is within inclusive
#' limits
#' 
#' 
#' @param x A numeric value, or vector of values.
#' @param lims A two-element vector of (min, max) values for the inclusive
#' limits. If \code{x} is a vector, \code{lims} may also be a two-column matrix
#' with \code{nrow=length(x)} where the first column is lower limits and the
#' second column is upper limits. If \code{x} is a vector and \code{lims} is a
#' two-element vector, then each element of \code{x} will be checked against
#' the same limits. If \code{x} is a vector and \code{lims} is a matrix, then
#' each element of \code{x} will be checked against the limits given by the
#' corresponding row of \code{lims}.
#' @return A logical vector the same length as \code{x}, indicating whether
#' each element of \code{x} is within the inclusive limits given by
#' \code{lims}.
#' @author Caroline Ring
#' @references 
#' \insertRef{ring2017identifying}{httk}
#' @keywords httk-pop
#' @export is_in_inclusive
is_in_inclusive <- function(x, lims) {
  if (is.numeric(x)){
    if (length(x)!=1) {
      if (is.matrix(lims)){
        if (length(x)!=nrow(lims)){
          stop('x must either be length 1 or the same length as nrow(lims) if lims is a matrix')
        }
      }
    }
  } else stop('x must be a numeric vector')
  
  if (is.numeric(lims)){
    if (length(lims)==2){
      a <- lims[1]
      b <- lims[2]
    } else stop('If lims is a numeric vector, it must be length 2.') 
  } else if (is.matrix(lims)) {
    if (ncol(lims)==2){
      if (nrow(lims)>0){
        a <- lims[,1]
        b <- lims[,2]
        
      } else {
        #warning('x was numeric(0) and lims was a matrix with 0 rows; returning logical(0)')
        return(logical(0))
      }
    } else stop('If lims is a matrix, it must have two columns.')
  } else stop('lims must be either a 2-element numeric vector or a 2-column numeric matrix')
  
  if (!is.numeric(a) & is.numeric(b)) {
    print('a = ')
    print(a)
    stop('The lower limit was not numeric')
  }
  if (!is.numeric(b) & is.numeric(a)){
    print('b = ')
    print(b)
    stop('The upper limit was not numeric')
  }
  if (!is.numeric(a) & !is.numeric(b)){
    print('a = ')
    print(a)
    print('b = ')
    print(b)
    stop('The upper and lower limits were not numeric')
  }
  return((x - a)  *  (b - x) >= 0)
}
