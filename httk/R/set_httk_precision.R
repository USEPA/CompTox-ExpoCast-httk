#' set_httk_precision
#' 
#' Although the ODE solver and other functions return very precise numbers,
#' we cannot (or at least do not spend enough computing time to) be sure of the 
#' precioion to an arbitrary level. This function both limits the number of
#' signficant figures reported and truncates the numerical precision. 
#' 
#' @param in.num The numeric variable (or assembly of numerics) to be
#' processed.
#' @param sig.fig The number of significant figures reported. Defaults to 4.
#' @param num.prec The precision maintained, digits below 10^num.prec are 
#' dropped. Defaults to 9.
#' 
#' @return numeric values
#' 
#' @author John Wambaugh 
set_httk_precision <- function(in.num, sig.fig = 4, num.prec = 9)
{  
  if (is.numeric(in.num))
  {
    out <- signif(in.num, sig.fig)
    out <- round(out, num.prec)
  } else out <- in.num
  return(out)
}