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
#' @references Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Sochaski, M.A.,
#' Rotroff, D.M., Freeman, K., Clewell, H.J., Dix, D.H., Andersen, M.E., Houck,
#' K.A., Allen, B., Judson, R.S., Sing, R., Kavlock, R.J., Richard, A.M., and
#' Thomas, R.S., "Integration of Dosimetry, Exposure and High-Throughput
#' Screening Data in Chemical Toxicity Assessment," Toxicological Sciences 125
#' 157-174 (2012)
#' 
#' Wetmore, B.A., Wambaugh, J.F., Ferguson, S.S., Li, L., Clewell, H.J. III,
#' Judson, R.S., Freeman, K., Bao, W, Sochaski, M.A., Chu T.-M., Black, M.B.,
#' Healy, E, Allen, B., Andersen M.E., Wolfinger, R.D., and Thomas R.S., "The
#' Relative Impact of Incorporating Pharmacokinetics on Predicting in vivo
#' Hazard and Mode-of-Action from High-Throughput in vitro Toxicity Assays"
#' Toxicological Sciences, 132:327-346 (2013).
#' 
#' Wetmore, B. A., Wambaugh, J. F., Allen, B., Ferguson, S. S., Sochaski, M.
#' A., Setzer, R. W., Houck, K. A., Strope, C. L., Cantwell, K., Judson, R. S.,
#' LeCluyse, E., Clewell, H.J. III, Thomas, R.S., and Andersen, M. E. (2015).
#' "Incorporating High-Throughput Exposure Predictions with Dosimetry-Adjusted
#' In Vitro Bioactivity to Inform Chemical Toxicity Testing" Toxicological
#' Sciences, kfv171.
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
  valid.info <- c("Compound","CAS","MW","Raw.Experimental.Percentage.Unbound","Entered.Experimental.Percentage.Unbound","Fub","source_PPB","Renal_Clearance","Met_Stab","Met_Stab_entered" ,"r2","p.val","Concentration..uM.","Css_lower_5th_perc.mg.L.","Css_median_perc.mg.L.","Css_upper_95th_perc.mg.L.","Css_lower_5th_perc.uM.","Css_median_perc.uM.","Css_upper_95th_perc.uM.","Species")

  if (any(!(info %in% valid.info))) stop(paste("Data on",info[!(info %in% valid.info)],"not available. Valid options are:",paste(valid.info,collapse=" ")))
  
  if (!(toupper(species) %in% toupper(unique(Wetmore.data[,"Species"])))) stop(paste("Species",species,"not found.  Available data for:",unique(Wetmore.data[,"Species"])))
  
  return(unique(Wetmore.data[toupper(Wetmore.data[,"Species"])==toupper(species),info]))
}
