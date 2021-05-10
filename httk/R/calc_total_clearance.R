#' Calculate the total clearance.
#' 
#' This function calculates the total clearance rate for a one compartment model
#' where clearance is entirely due to metablism by the liver and glomerular
#' filtration in the kidneys, identical to clearance of three compartment
#' steady state model.
#' 
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param parameters Chemical parameters from parameterize_steadystate
#' function, overrides chem.name and chem.cas.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE.
#' @param restrictive.clearance Protein binding is not taken into account (set
#' to 1) in liver clearance if FALSE.
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for well-stirred model if TRUE.  This assumes clearance relative
#' to amount unbound in whole blood instead of plasma, but converted to use
#' with plasma concentration.
#' @param ... Additional parameters passed to parameterize_steadystate if
#' parameters is NULL.
#' @return \item{Total Clearance}{Units of L/h/kg BW.}
#' @author John Wambaugh
#' @keywords Parameter 1compartment
#' @examples
#' 
#' calc_total_clearance(chem.name="Ibuprofen") 
#' 
#' 
#' @export calc_total_clearance
calc_total_clearance<- function(chem.cas=NULL,
                                chem.name=NULL,
                                dtxsid=NULL,
                                parameters=NULL,
                                species="Human",
                                suppress.messages=FALSE,
                                default.to.human=FALSE,
                                well.stirred.correction=TRUE,
                                restrictive.clearance=TRUE,
                                adjusted.Funbound.plasma=TRUE,
                                ...)

{
    if(is.null(parameters)) 
    {
      parameters <- parameterize_steadystate(chem.cas=chem.cas, 
                                             chem.name=chem.name, 
                                             dtxsid=dtxsid,
                                             species=species,
                                             default.to.human=default.to.human,
                                             adjusted.Funbound.plasma=
                                               adjusted.Funbound.plasma,
                                             suppress.messages=suppress.messages,
                                             ...)
    }
    Qgfrc <- get_param("Qgfrc",parameters,"calc_Css") / 
      parameters[['BW']]^0.25 #L/h/kgBW
    fup <- parameters[["Funbound.plasma"]]# unitless fraction
    clearance <- Qgfrc*fup+
                   calc_hep_clearance(chem.cas=chem.cas,
                     chem.name=chem.name,
                     dtxsid=dtxsid,
                     species=species,
                     parameters=parameters,
                     suppress.messages=TRUE,
                     well.stirred.correction=well.stirred.correction,
                     restrictive.clearance=restrictive.clearance,
                     adjusted.Funbound.plasma=
                       adjusted.Funbound.plasma) #L/h/kgBW

    if (!suppress.messages)
    {
      cat(paste(toupper(substr(species,1,1)),
        substr(species,2,nchar(species)),sep=''),
        "total clearance returned in units of L/h/kg BW.\n")
    }
    
    return(set_httk_precision(as.numeric(clearance)))
}
