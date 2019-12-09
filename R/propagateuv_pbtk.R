#' Propagates uncertainty and variability in in vitro HTTK data into PBPK
#' model parameters
#'
#' @param parameters.dt The data table of parameters being used by the Monte
#' Carlo sampler
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author John Wambaugh
#'
#' @keywords monte-carlo pbtk
propagateuv_pbtk <- function(
                             parameters.dt,
                             ...)
{
      #Compute Vdist, volume of distribution
      parameters.dt[, Clmetabolismc := 
        as.numeric(calc_hep_clearance(
          hepatic.model="unscaled",
          parameters=list(
            Clint=Clint, #uL/min/10^6 cells
            Funbound.plasma=Funbound.plasma, # unitless fraction
            Fhep.assay.correction=
              Fhep.assay.correction, 
            million.cells.per.gliver= million.cells.per.gliver, # 10^6 cells/g-liver
            liver.density= liver.density, # g/mL
            Dn=0.17,
            BW=BW,
            Vliverc=Vliverc, #L/kg
            Qtotal.liverc=
              ((Qgutf+Qliverf)*as.numeric(Qcardiacc))/1000*60),
          suppress.messages=T,
          ...))]

  return(parameters.dt)
}