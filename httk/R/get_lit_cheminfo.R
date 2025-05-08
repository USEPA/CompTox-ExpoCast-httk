#' Get literature Chemical Information.
#' 
#' This function provides the information specified in "info=" for all
#' chemicals with data from the Wetmore et al. (2012) and (2013) publications
#' and other literature.
#' 
#' 
#' @param info A single character vector (or collection of character vectors)
#' from
#' "Compound", "CAS", "MW", "Raw.Experimental.Percentage.Unbound",
#' "Entered.Experimental.Percentage.Unbound", "Fub", "source_PPB",
#' "Renal_Clearance", "Met_Stab", "Met_Stab_entered",
#' "r2", "p.val", "Concentration..uM.", "Css_lower_5th_perc.mg.L.", 
#' "Css_median_perc.mg.L.", "Css_upper_95th_perc.mg.L.",
#' "Css_lower_5th_perc.uM.","Css_median_perc.uM.","Css_upper_95th_perc.uM.",
#' and "Species".
#' 
#' @param species Species desired (either "Rat" or default "Human").
#' 
#' @return \item{info}{Table/vector containing values specified in "info" for
#' valid chemicals.}
#' 
#' @author John Wambaugh
#' 
#' @references
#' \insertRef{wetmore2012integration}{httk}
#' 
#' \insertRef{wetmore2013relative}{httk}
#' 
#' \insertRef{wetmore2015incorporating}{httk}
#' 
#' @keywords Literature Retrieval
#' 
#' @examples
#' 
#' get_lit_cheminfo()
#' get_lit_cheminfo(info=c('CAS','MW'))
#' 
#' @export get_lit_cheminfo
get_lit_cheminfo <- function(info="CAS",species="Human")
{
  Wetmore.data <- Wetmore.data
  valid.info <- c(
                  "Compound",
                  "CAS",
                  "MW",
                  "Raw.Experimental.Percentage.Unbound",
                  "Entered.Experimental.Percentage.Unbound",
                  "Fub","source_PPB",
                  "Renal_Clearance",
                  "Met_Stab",
                  "Met_Stab_entered",
                  "r2",
                  "p.val",
                  "Concentration..uM.",
                  "Css_lower_5th_perc.mg.L.",
                  "Css_median_perc.mg.L.",
                  "Css_upper_95th_perc.mg.L.",
                  "Css_lower_5th_perc.uM.",
                  "Css_median_perc.uM.",
                  "Css_upper_95th_perc.uM.",
                  "Species")

  if (any(!(info %in% valid.info))) stop(
      paste("Data on",info[!(info %in% valid.info)],
            "not available. Valid options are:",
            paste(valid.info,collapse=" ")))
  
  if (!(toupper(species) %in% toupper(unique(Wetmore.data[,"Species"])))) stop(
      paste("Species",
            species,
            "not found.  Available data for:",
            unique(Wetmore.data[,"Species"])))
  
  return(unique(Wetmore.data[toupper(Wetmore.data[,"Species"]) ==
         toupper(species),info]))
}


#' Get literature Chemical Information. (deprecated).
#' 
#' This function is included for backward compatibility. It calls
#' \code{\link{get_lit_cheminfo}} which
#' provides the information specified in "info=" for all
#' chemicals with data from the Wetmore et al. (2012) and (2013) publications
#' and other literature.
#' 
#' 
#' @param info A single character vector (or collection of character vectors)
#' from
#' "Compound", "CAS", "MW", "Raw.Experimental.Percentage.Unbound",
#' "Entered.Experimental.Percentage.Unbound", "Fub", "source_PPB",
#' "Renal_Clearance", "Met_Stab", "Met_Stab_entered",
#' "r2", "p.val", "Concentration..uM.", "Css_lower_5th_perc.mg.L.", 
#' "Css_median_perc.mg.L.", "Css_upper_95th_perc.mg.L.",
#' "Css_lower_5th_perc.uM.","Css_median_perc.uM.","Css_upper_95th_perc.uM.",
#' and "Species".
#' 
#' @param species Species desired (either "Rat" or default "Human").
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @return \item{info}{Table/vector containing values specified in "info" for
#' valid chemicals.}
#' 
#' @author John Wambaugh
#' 
#' @references
#' \insertRef{wetmore2012integration}{httk}
#' 
#' \insertRef{wetmore2013relative}{httk}
#' 
#' \insertRef{wetmore2015incorporating}{httk}
#' 
#' @keywords Literature Retrieval
#' 
#' @examples
#' 
#' get_lit_cheminfo()
#' get_lit_cheminfo(info=c('CAS','MW'))
#' 
#' @export get_wetmore_cheminfo
get_wetmore_cheminfo <- function(
    info="CAS",
    species="Human",
    suppress.messages=FALSE)
{
  if (!suppress.messages) warning(
    "Function \"get_wetmore_cheminfo\" has been renamed to \"get_lit_cheminfo\".")

    return(do.call(get_lit_cheminfo, 
                   args=purrr::compact(list(info = info, species=species))))
}