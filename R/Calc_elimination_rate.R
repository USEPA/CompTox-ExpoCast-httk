#' Calculate the elimination rate for a one compartment model. 
#' 
#' This function calculates an elimination rate from the three compartment
#' steady state model where elimination is entirely due to metablism by the
#' liver and glomerular filtration in the kidneys.
#' 
#' Elimination rate calculated by dividing the total clearance (using the
#' default -stirred hepatic model) by the volume of distribution. When
#' species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitues human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' 
#' @param chem.name Either the chemical name or the cas number must be
#' specified. 
#' @param chem.cas Either the cas number or the chemical name must be
#' specified. 
#' @param parameters Chemical parameters from parameterize_steadystate or
#' 1compartment function, overrides chem.name and chem.cas.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' @param restrictive.clearance In calculating elimination rate, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' @param .stirred.correction Uses correction in calculation of hepatic
#' clearance for -stirred model if TRUE.  This assumes clearance relative
#' to amount unbound in whole blood instead of plasma, but converted to use
#' with plasma concentration.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @return \item{Elimination rate}{Units of 1/h.}
#' @author John Wambaugh
#' @keywords Parameter
#' @examples
#' 
#' calc_elimination_rate(chem.name="Bisphenol A")
#' calc_elimination_rate(chem.name="Bisphenol A",species="Rat")
#' calc_elimination_rate(chem.cas="80-05-7")
#' 
#' @export calc_elimination_rate
calc_elimination_rate <- function(chem.cas=NULL,
                                  chem.name=NULL,
                                  parameters=NULL,
                                  species="Human",
                                  suppress.messages=F,
                                  default.to.human=F,
                                  restrictive.clearance=T,
                                  adjusted.Funbound.plasma=T,
                                  regression=T,
                                  well.stirred.correction=T,
                                  clint.pvalue.threshold=0.05,
                                  minimum.Funbound.plasma=0.0001)
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  .stirred.correction <- NULL
  #End R CMD CHECK appeasement.
  
  name.list <- c("Clint",
                 "Funbound.plasma",
                 "Qtotal.liverc",
                 "million.cells.per.gliver",
                 "Vliverc",
                 "BW",
                 "liver.density",
                 'Fhep.assay.correction')

# This function likes to have the blood flow to the liver per kg bw^0.75 in a 
# variable named Qtotal.liverc:
  if (!is.null(parameters))
  {
    if (all(c("Qcardiacc","Qgutf","Qliverf")%in%names(parameters)))
    {
      parameters[["Qtotal.liverc"]] <- parameters[["Qcardiacc"]]*(parameters[["Qgutf"]]+parameters[["Qliverf"]])
    }
  }
  
  if(is.null(parameters))
  {
    parameters <- parameterize_steadystate(chem.cas=chem.cas,
                    chem.name=chem.name,
                    species=species,
                    default.to.human=default.to.human,
                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                    clint.pvalue.threshold=clint.pvalue.threshold,
                    minimum.Funbound.plasma=minimum.Funbound.plasma)
    Vd <- calc_vdist(chem.cas=chem.cas,
                     chem.name=chem.name,
                     species=species,
                     suppress.messages=T,
                     default.to.human=default.to.human,
                     adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                     regression=regression,
                     minimum.Funbound.plasma=minimum.Funbound.plasma) 
  } else { 
    if(!all(name.list %in% names(parameters)))
    {
      if(is.null(chem.cas) & is.null(chem.name))
      { 
        stop('chem.cas or chem.name must be specified when not including all 3compartment or pbtk parameters.')
      }
      params <- parameterize_steadystate(chem.cas=chem.cas,
                  chem.name=chem.name,
                  species=species,
                  default.to.human=default.to.human,
                  adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                  minimum.Funbound.plasma=minimum.Funbound.plasma)
      parameters <- c(parameters,params[name.list[!(name.list %in% names(parameters))]])
    }
    if('Vdist' %in% names(parameters)){
      Vd <- parameters[['Vdist']]
    }else{
#        if(is.null(chem.name) & is.null(chem.cas))stop('chem.cas or chem.name must be specified when Vdist is not included in parameters.')
      Vd <- calc_vdist(chem.cas=chem.cas,
                       chem.name=chem.name,
                       parameters=parameters,
                       species=species,
                       suppress.messages=T,
                       default.to.human=default.to.human,
                       adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                       regression=regression,
                       minimum.Funbound.plasma=minimum.Funbound.plasma) 
    }    
  } 
  clearance <- calc_total_clearance(chem.name=chem.name,
                                    chem.cas=chem.cas,
                                    species=species,
                                    parameters=parameters,
                                    suppress.messages=T,
                                    default.to.human=default.to.human,
                                    .stirred.correction=.stirred.correction,
                                    restrictive.clearance=restrictive.clearance,
                                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                    clint.pvalue.threshold=clint.pvalue.threshold,
                                    well.stirred.correction=well.stirred.correction,
                                    minimum.Funbound.plasma=minimum.Funbound.plasma) #L/h/kgBW

  if(!suppress.messages)cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"elimination rate returned in units of 1/h.\n")
  return(as.numeric(clearance/Vd))
}
