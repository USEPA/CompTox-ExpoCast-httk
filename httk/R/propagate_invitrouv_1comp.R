#' Propagates uncertainty and variability in in vitro HTTK data into one
#' compartment model parameters
#'
#' @param parameters.dt The data table of parameters being used by the Monte
#' Carlo sampler
#' @param ... Additional arguments passed to \code{\link{calc_elimination_rate}}
#'
#'@return A data.table whose columns are the parameters of the HTTK model
#'  specified in \code{model}.
#'
#' @author John Wambaugh
#'
#' @keywords monte-carlo 1compartment
propagate_invitrouv_1comp <- function(
                             parameters.dt,
                             ...)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  RBC.vol<-plasma.vol<-hematocrit<-Vdist<-Krbc2pu<-Funbound.plasma <- NULL
  Krest2pu<-Vrestc<-Clint<-Qtotal.liverc<-Qgfrc<-million.cells.per.gliver<-NULL
  BW<-Rblood2plasma<-Vliverc<-Fhep.assay.correction<-liver.density<-NULL
  kelim<-NULL
  #End R CMD CHECK appeasement.
  
  
      #for 1-compartment model, don't need to compute total hepatic clearance,
      #but do need to compute volume of distribution and elimination rate.

      #HTTK contains a function to compute volume of distribution, but it pulls
      #Funbound.plasma from its table of default values, meaning we can't give
      #that function our vector of individual Funbound.plasma values. So
      #instead, I've re-implemented the Vdist equation here.
  
      #Funbound.plasma was already adjusted (if necessary),
      #and tissues were already lumped,
      # in create_mc_samples() before it called this function.
      #so we can just proceed.
      
      #To compute volume of distribution, need to get volume of red blood cells.
      #Can compute that from plasma volume and hematocrit.

      parameters.dt[, RBC.vol:=plasma.vol/
                    (1 - hematocrit)*
                    hematocrit]
      #Compute Vdist, volume of distribution
      parameters.dt[,Vdist:=plasma.vol +
                    RBC.vol*
                    Krbc2pu*
                    Funbound.plasma+
                    Krest2pu*
                    Vrestc*
                    Funbound.plasma]
      #Compute kelim: Elimination rate, units of 1/h. 
      #Call HTTK function to calculate total elimination rate. This one is OK
      #because it uses the vector of Funbound.plasma that we give it.
      ke <- httk::calc_elimination_rate(parameters=parameters.dt,
                                        suppress.messages=TRUE,
                                        ...)

      #Add kelim to the population data.table.
      parameters.dt[, kelim:=ke]

  return(parameters.dt)
}
