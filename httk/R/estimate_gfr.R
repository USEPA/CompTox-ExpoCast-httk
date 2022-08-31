#'Predict GFR.
#'
#'Predict GFR using CKD-EPI equation (for adults) or BSA-based equation (for children).
#'
#'Add residual variability based on reported residuals for each equation.
#'
#' @param gfrtmp.dt A data.table with columns \code{gender}, \code{reth}, 
#' \code{age_years}, \code{age_months}, \code{BSA_adj}, \code{serum_creat}.
#' @param gfr_resid_var Logical value indicating whether or not to include
#' residual variability when generating GFR values. (Default is TRUE.)
#' @param ckd_epi_race_coeff Logical value indicating whether or not to use the
#' "race coefficient" from the CKD-EPI equation when estimating GFR values.
#' (Default is FALSE.)
#'  
#'@return The same data.table with a \code{gfr_est} column added, containing 
#'  estimated GFR values.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references Ring, Caroline L., et al. "Identifying populations sensitive to 
#'environmental chemicals by simulating toxicokinetic variability." Environment 
#'International 106 (2017): 105-118
#' @import stats
#' @export estimate_gfr
estimate_gfr <- function(gfrtmp.dt,
                         gfr_resid_var = TRUE,
                         ckd_epi_race_coeff = FALSE){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL!
  age_years <- serum_creat <- gfr_est <- gender <- reth <- BSA_adj <- NULL
  #End of R CMD CHECK appeasement.

  gfrtmp.dt <- data.table::copy(gfrtmp.dt) #to avoid altering the original item
  #Take a list of sample individuals with gender, race, age_years, height, BSA in cm^2, serum creatinine
  #Then estimate their GFR in mL/min/1.73m^2 using either the CKD-EPI equation (for adults)
  #or the bedside Schwartz equation (for children).
  
  if(any(gfrtmp.dt[, age_years>=18])){
    #get CKD-EPI-predicted GFR
    gfrtmp.dt[age_years>=18,
              gfr_est:=ckd_epi_eq(scr=serum_creat, 
                                  gender=gender, 
                                  reth=reth, #note that this is actually no longer used
                                  age_years=age_years,
                                  ckd_epi_race_coeff = ckd_epi_race_coeff)]
    if(isTRUE(gfr_resid_var)){
    #Add residual variability
    #see data-raw/ckd_epi_resid_var.R for calculations
    #estimating sdlog of residual distribution that is zero-mean and constant-variance on the log scale
    #based on CKD-EPI paper info on residual median, IQR on the natural scale,
    #percentage of eGFR within 30% of mGFR, and RMSE of residuals on the log scale
    #this rlnorm is equivalent to gfr_est + rlnorm3(meanlog = log(gfr_est), sdlog = 0.2061534, threshold = -gfr_est)
      gfrtmp.dt[age_years>=18,
                gfr_est:= rlnorm(n=.N,
                         meanlog = log(gfr_est),
                         sdlog = 0.2061534)]
    }
  }

  if(any(gfrtmp.dt[, age_years<18])){
    #get model-predicted GFR
  gfrtmp.dt[age_years<18, 
           gfr_est:=estimate_gfr_ped(BSA=BSA_adj/(100^2))] #convert BSA to m^2
  #gfr_est in units of mL/min/1.73m^2 BSA
    
    if(isTRUE(gfr_resid_var)){
    #Add residual variability -- constant CV of 30%, per Johnson et al. 2006
    #and *log-normal* data also per Johnson et al. 2006
    #30% CV = sqrt(exp(sdlog^2)-1)
    #so, sdlog = sqrt(log(0.3^2+1))
    #arithmetic mean is the predicted GFR value,
    #arith mean = exp(meanlog + sdlog^2/2)
    #so, log(arith mean) - sdlog^2/2 = meanlog
    gfrtmp.dt[age_years<18, 
              gfr_est:=rlnorm(n=.N,
                              meanlog = log(gfr_est) - log(0.3^2+1)/2,
                              sdlog = sqrt(log(0.3^2+1)))]
    }
  }
  return(gfrtmp.dt)
}
