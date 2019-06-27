#'Predict GFR.
#'
#'First predict serum creatinine using smoothing spline, then predict GFR using
#'CKD-EPI equation.
#'
#'@param gfrtmp.dt A data.table with columns \code{gender}, \code{reth}, 
#'  \code{age_years}, \code{age_months}, \code{BSA_adj}, \code{serum_creat}.
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
estimate_gfr <- function(gfrtmp.dt){
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data. table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  id <- age_years <- log_serum_creat <- sc_spline <- age_months <- NULL
  sc_kde <- serum_creat <- gfr_est <- gender <- reth <- BSA_adj <- NULL
  #End of R CMD CHECK appeasement.

  gfrtmp.dt <- data.table::copy(gfrtmp.dt) #to avoid altering the original item
  #Take a list of sample individuals with gender, race, age_years, height, BSA in cm^2
  #Draw their serum creatinine levels from the appropriate KDE distribution
  #Then estimate their GFR in mL/min/1.73m^2 using either the MDRD equation (for adults)
  #or the bedside Schwartz equation (for children).
  gfrtmp.dt[, id:=1:nrow(gfrtmp.dt)]
  
  gfrtmp.tmp <- merge(gfrtmp.dt, 
                      spline_serumcreat,
                      by=c('gender', 'reth'))
  
  if (gfrtmp.tmp[, any(age_years>=12)]){
  gfrtmp.tmp[age_years>=12, 
             log_serum_creat:=predict(sc_spline[[1]], 
                     x=age_months)$y + 
               rfun(n=sum(age_years>=12),
                    fhat=sc_kde[[1]]),
             by=list(gender, reth)] #Note: need to go by gender and reth!
  
  gfrtmp.dt <- merge(gfrtmp.dt,
                     gfrtmp.tmp[, list(id, log_serum_creat)],
                     by='id')
  gfrtmp.dt[, id:=NULL]
  
  gfrtmp.dt[age_years>=12, 
            serum_creat:=exp(log_serum_creat)]
  
gfrtmp.dt[age_years>=18,
         gfr_est:=ckd_epi_eq(scr=serum_creat, 
                                        gender=gender, 
                                        reth=reth, 
                                        age_years=age_years)]
}
  gfrtmp.dt[age_years<18, 
           gfr_est:=estimate_gfr_ped(BSA=BSA_adj/(100^2))] #convert BSA to m^2
  #gfr_est in units of mL/min/1.73m^2 BSA
  return(gfrtmp.dt)
}
