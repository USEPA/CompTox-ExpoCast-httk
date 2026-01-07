#' Interact with HTTK vignettes
#'
#' @description
#' This function lists the available vignettes, including those from the
#' opetional httkexamples package.
#'
#' @param vignette The name of a vignette to be displayed.
#'
#' @param ... Additional arguments to function \code{\link{vignette}}
#'  
#'@examples 
#'calc_analytic_css(chem.name='Bisphenol-A',output.units='mg/L',
#'                  model='3compartment',concentration='blood')
#' 
#'
#' @seealso \code{\link{vignette}}
#'
#' @author John Wambaugh
#'
#' @keywords vignettes
#'
#'@export httk_vignettes
httk_vignettes <- function(vignette=NULL,
                           ...
                           )
{  
  # Check if httk examples is installed:
  httkexamples.installed <- do.call(require,list("httkexamples",
                                              quietly=TRUE))
  # If argument vignette is NULL, list all available vignettes:
  if (is.null(vignette))
  {
    httk.vignette <- vignette(package="httk", ...)
    if (httkexamples.installed) 
    {
      examples.vignette <- vignette(package="httkexamples",...) 
      # Create output with data from both vignettes:
      out <- httk.vignette
      out$results <- rbind(httk.vignette$results,
                           examples.vignette$results)
      out$LibPath <- rbind(httk.vignette$LibPath,
                           examples.vignette$LibPath)
      out$Item <- rbind(httk.vignette$Item,
                        examples.vignette$Item)
      out$Title <- rbind(httk.vignette$Title,
                         examples.vignette$Title)
    } else {
      out <- vignette(package="httk", ...)
    }
  } else {                   
    if (httkexamples.installed)
    {
      # Look for vignette in httk first (warnings suppressed since it might be
      # in httkexamples):
      out <- suppressWarnings(vignette(topic=vignette, package="httk", ...))
      # Check for error:
      if (is(out, "character")) 
      {
        # Look for vignette in httkexamples with no warning suppression:
        out <- vignette(topic=vignette, 
                        package="httkexamples", ...)
      }
    } else {
      # Look for vignette in httk with no warning suppression:
      out <- vignette(topic=vignette, package="httk", ...)
      # Check for error:
      if (is(out, "character")) 
      {
        warning("Perhaps this vignette is available from R package httkexamples.")
      }
    }
  }
  
  return (out)
}
