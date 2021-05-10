#' Calculate the hepatic clearance.
#' 
#' This function calculates the hepatic clearance in plasma for a well-stirred model
#' or other type if specified. Based on  Ito and Houston (2004)
#' 
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @param parameters Chemical parameters from parameterize_steadystate
#' function, overrides chem.name and chem.cas.
#' @param hepatic.model Model used in calculating hepatic clearance, unscaled,
#' parallel tube, dispersion, or default well-stirred.
#' @param suppress.messages Whether or not to suppress the output message.
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for well-stirred model if TRUE for hepatic.model well-stirred.
#' This assumes clearance relative to amount unbound in whole blood instead of
#' plasma, but converted to use with plasma concentration.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE.
#' @param ... Additional parameters passed to parameterize_steadystate if
#' parameters is NULL.
#'
#' @return \item{Hepatic Clearance}{Units of L/h/kg BW.}
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @keywords Parameter
#'
#' @references
#' Ito, K., & Houston, J. B. (2004). "Comparison of the use of liver models for 
#' predicting drug clearance using in vitro kinetic data from hepatic microsomes 
#' and isolated hepatocytes." Pharmaceutical Tesearch, 21(5), 785-792.
#'
#' @examples
#' 
#' calc_hep_clearance(chem.name="Ibuprofen",hepatic.model='unscaled')
#' calc_hep_clearance(chem.name="Ibuprofen",well.stirred.correction=FALSE)
#' 
#' 
#' @export calc_hep_clearance
calc_hep_clearance <- function(chem.name=NULL,
                               chem.cas=NULL,
                               dtxsid = NULL,
                               parameters=NULL,
                               species='Human',
                               default.to.human=FALSE,
                               hepatic.model='well-stirred',
                               suppress.messages=FALSE,
                               well.stirred.correction=TRUE,
                               restrictive.clearance=TRUE,
                               adjusted.Funbound.plasma=TRUE,
                               ...)
{
  model <- hepatic.model
  name.list <- c("Clint",
                 "Funbound.plasma",
                 "Qtotal.liverc",
                 "million.cells.per.gliver",
                 "Vliverc",
                 "BW",
                 "liver.density",
                 'Fhep.assay.correction')
                 
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
  if (any(!is.null(chem.cas),!is.null(chem.name),!is.null(dtxsid)))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  }
  
# This function likes to have the blood flow to the liver per kg bw^0.75 in a 
# variable named Qtotal.liverc:
  if (!is.null(parameters))
  {
    if (all(c("Qcardiacc","Qgutf","Qliverf")%in%names(parameters)))
    {
      parameters[["Qtotal.liverc"]] <- 
        parameters[["Qcardiacc"]]*
        (parameters[["Qgutf"]]+parameters[["Qliverf"]])
    }
  }
  if(is.null(parameters))
  {
    parameters <- parameterize_steadystate(
                    chem.cas=chem.cas,
                    chem.name=chem.name,
                    dtxsid=dtxsid,
                    species=species,
                    default.to.human=default.to.human,
                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                    suppress.messages=suppress.messages,
                    ...)
    Qtotal.liverc <- get_param(
                       "Qtotal.liverc",
                       parameters,
                       "calc_hep_clearance",
                       default=1.24) # L/h/kgBW
    Vliverc <- get_param(
                 "Vliverc",
                 parameters,
                 "calc_hep_clearance") #  L/kg BW
    liver.density <- get_param(
                       "liver.density",
                       parameters,
                       "calc_hep_clearance") # g/mL
    Dn <- get_param(
            "Dn",
            parameters,
            "calc_hep_clearance",
            default=0.17) #

    million.cells.per.gliver <- get_param(
                                  "million.cells.per.gliver",
                                  parameters,
                                  "calc_hep_clearance") # 10^6 cells/g-liver

  } else if (!all(name.list %in% names(parameters)))
  {
    if (is.null(chem.cas) & is.null(chem.name) & is.null(dtxsid)) stop(
'chem.cas, chem.name, or dtxsid must be specified when not including all necessary 3compartmentss parameters.')
    params <- parameterize_steadystate(
                chem.cas=chem.cas,
                chem.name=chem.name,
                dtxsid=dtxsid,
                species=species,
                default.to.human=default.to.human,
                adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                suppress.messages=suppress.messages,
                ...)
    parameters <- c(parameters,params[
                      name.list[!(name.list %in% names(parameters))]])
  }

  Clint <- get_param(
             "Clint",
             parameters,
             "calc_hep_clearance") # uL/min/10^6 cells
  
  #If Clint is at this point represented by a string as a distribution
  #with median, 5th and 95th percentile values, and p value, perform
  #string operation to obtain the numeric form of the median value for
  #Clint. 
  if (nchar(Clint[1]) - nchar(gsub(",","",Clint[1]))==3) 
  {
    Clint.pValue <- as.numeric(strsplit(Clint,",")[[1]][4])
    Clint <- as.numeric(strsplit(Clint,",")[[1]][1])
    if (!suppress.messages) warning("Clint is provided as a distribution.")
  }
  
             
  fu_hep <- get_param(
              "Fhep.assay.correction",
              parameters,
              "calc_hep_clearance") 
              
# Correct for fraction of chemical unbound in in vitro hepatocyte assay:
  Clint <- Clint / fu_hep

  fup <- get_param(
           "Funbound.plasma",
           parameters,
           "calc_hep_clearance") # unitless fraction
  if (!restrictive.clearance) fup <- 1
  
  Qtotal.liverc <- get_param(
                     "Qtotal.liverc",
                     parameters,
                     "calc_hep_clearance",
                     default=1.24) # L/h/kgBW
                     
  Vliverc <- get_param(
               "Vliverc",
               parameters,
               "calc_hep_clearance") #  L/kg BW
               
  liver.density <- get_param(
                     "liver.density",
                     parameters,
                     "calc_hep_clearance") # g/mL
  
  Dn <- get_param(
          "Dn",
          parameters,
          "calc_hep_clearance",
          default=0.17) #
  
  million.cells.per.gliver <- get_param(
                                "million.cells.per.gliver",
                                parameters,
                                "calc_hep_clearance") # 10^6 cells/g-liver

  if (!(tolower(model) %in% 
    c("well-stirred","parallel tube","dispersion","unscaled")))
    stop(
"Model other than \"well-stirred,\" \"parallel tube,\", \"dispersion\", or \"unscaled\" specified.")

  # Convert from uL/min/10^6 cells to uL/min/g-liver to uL/min/kg BW
  Clint <- Clint*million.cells.per.gliver
  # Convert from uL/min/g-liver to uL/min/kg BW
  Clint <- Clint*(Vliverc*1000*liver.density)
  # Convert from uL/min/kg BW to L/h/kg BW
  Clint <- Clint/10^6*60 

  Qtotal.liverc <- Qtotal.liverc / parameters[['BW']]^0.25
  if (tolower(model) == "unscaled")
  {
    CLh <- Clint
  } else if (tolower(model) == "well-stirred") {
    if(well.stirred.correction)
    {
      if ('Rblood2plasma' %in% names(parameters))
      {
        Rblood2plasma <- parameters$Rblood2plasma
      } else if (!(is.null(chem.cas) & is.null(chem.name)))
      {
        Rblood2plasma <- available_rblood2plasma(chem.name=chem.name,
          chem.cas=chem.cas,
          species=species,
          adjusted.Funbound.plasma=adjusted.Funbound.plasma,
          suppress.messages=suppress.messages)
      } else(stop("Enter chem.cas or chem.name with corresponding species or enter Rblood2plasma as a parameter for the well-stirred model correction."))
      CLh <- Qtotal.liverc*fup*Clint/(Qtotal.liverc+fup*Clint / Rblood2plasma)
    } else CLh <- Qtotal.liverc*fup*Clint/(Qtotal.liverc+fup*Clint)   
  } else if(tolower(model) == "parallel tube")
  {
    CLh <- Qtotal.liverc*(1-exp(-fup*Clint/Qtotal.liverc))
  } else if(tolower(model) == "dispersion")
  {
    Rn <- fup*Clint/Qtotal.liverc
    a <- sqrt(1 + 4*Rn*Dn)
    CLh <- Qtotal.liverc*
      (1 - 4*a/((1+a)^2*exp((a-1)/2/Dn)-(1-a)^2*exp(-(a+1)/2/Dn)))
  }
  if (!suppress.messages) cat(
                            "Hepatic clearance calculated with the",
                            hepatic.model,
                            "model in units of L/h/kg.\n")  
  return(set_httk_precision(as.numeric(CLh)))
}

#' Calculate the hepatic clearance (deprecated).
#' 
#' This function is included for backward compatibility. It calls
#' \code{\link{calc_hep_clearance}} which
#' calculates the hepatic clearance in plasma for a well-stirred model
#' or other type if specified. Based on  Ito and Houston (2004)
#' 
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' @param parameters Chemical parameters from parameterize_steadystate
#' function, overrides chem.name and chem.cas.
#' @param hepatic.model Model used in calculating hepatic clearance, unscaled,
#' parallel tube, dispersion, or default well-stirred.
#' @param suppress.messages Whether or not to suppress the output message.
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for well-stirred model if TRUE for hepatic.model well-stirred.
#' This assumes clearance relative to amount unbound in whole blood instead of
#' plasma, but converted to use with plasma concentration.
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE.
#' @param ... Additional parameters passed to parameterize_steadystate if
#' parameters is NULL.
#'
#' @return \item{Hepatic Clearance}{Units of L/h/kg BW.}
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @keywords Parameter
#'
#' @references
#' Ito, K., & Houston, J. B. (2004). "Comparison of the use of liver models for 
#' predicting drug clearance using in vitro kinetic data from hepatic microsomes 
#' and isolated hepatocytes." Pharmaceutical Tesearch, 21(5), 785-792.
#'
#' @examples
#' 
#' calc_hep_clearance(chem.name="Ibuprofen",hepatic.model='unscaled')
#' calc_hep_clearance(chem.name="Ibuprofen",well.stirred.correction=FALSE)
#' 
#' 
#' @export calc_hepatic_clearance
calc_hepatic_clearance <- function(chem.name=NULL,
                               chem.cas=NULL,
                               dtxsid = NULL,
                               parameters=NULL,
                               species='Human',
                               default.to.human=FALSE,
                               hepatic.model='well-stirred',
                               suppress.messages=FALSE,
                               well.stirred.correction=TRUE,
                               restrictive.clearance=TRUE,
                               adjusted.Funbound.plasma=TRUE,
                               ...)
{
  warning("Function \"calc_hepatic_clearance\" has been renamed to \"calc_hep_clearance\".")
  return(calc_hep_clearance(chem.name=chem.name,
                               chem.cas=chem.cas,
                               dtxsid = dtxsid,
                               parameters = parameters,
                               species = species,
                               default.to.human = default.to.human,
                               hepatic.model = hepatic.model,
                               suppress.messages = suppress.messages,
                               well.stirred.correction = 
                                 well.stirred.correction,
                               restrictive.clearance = restrictive.clearance,
                               adjusted.Funbound.plasma = 
                                 adjusted.Funbound.plasma,
                               ...))
}