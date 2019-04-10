# from Ito and Houston (2004)




#' Calculate the hepatic clearance.
#' 
#' This function calculates the hepatic clearance in plasma for a well-stirred model
#' or other type if specified.
#' 
#' 
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
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
#' @return \item{Hepatic Clearance}{Units of L/h/kg BW.}
#' @author John Wambaugh
#' @keywords Parameter
#' @examples
#' 
#' calc_hepatic_clearance(chem.name="Ibuprofen",hepatic.model='unscaled')
#' calc_hepatic_clearance(chem.name="Ibuprofen",well.stirred.correction=FALSE)
#' 
#' 
#' @export calc_hepatic_clearance
calc_hepatic_clearance <- function(chem.name=NULL,
                                   chem.cas=NULL,
                                   parameters=NULL,
                                   species='Human',
                                   default.to.human=F,
                                   hepatic.model='well-stirred',
                                   suppress.messages=F,
                                   well.stirred.correction=T,
                                   restrictive.clearance=T,
                                   adjusted.Funbound.plasma=T,
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
                    ...)
    Qtotal.liverc <- get_param("Qtotal.liverc",parameters,"calc_Hepatic_Clearance",default=1.24) # L/h/kgBW
    Vliverc <- get_param("Vliverc",parameters,"calc_Hepatic_Clearance") #  L/kg BW
    liver.density <- get_param("liver.density",parameters,"calc_Hepatic_Clearance") # g/mL
    Dn <- get_param("Dn",parameters,"calc_Hepatic_Clearance",default=0.17) #
    #model <- get_param("model",parameters,"calc_Hepatic_Clearance",default="well-stirred")
    million.cells.per.gliver <- get_param("million.cells.per.gliver",parameters,"calc_Hepatic_Clearance") # 10^6 cells/g-liver

  } else if (!all(name.list %in% names(parameters)))
  {
    if(is.null(chem.cas) & is.null(chem.name))stop('chem.cas or chem.name must be specified when not including all necessary 3compartmentss parameters.')
    params <- parameterize_steadystate(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,adjusted.Funbound.plasma=adjusted.Funbound.plasma,...)
    parameters <- c(parameters,params[name.list[!(name.list %in% names(parameters))]])
  }
#  Clint <- get_param("Clint",parameters,"calc_Hepatic_Clearance") # uL/min/10^6 cells
#  # Check if clint is a distribution (median,low95,high95,pvalue):
#  if (nchar(Clint) - nchar(gsub(",","",Clint))==3) 
#  {
#    Clint <- as.numeric(strsplit(Clint,",")[[1]][1])
#  }
#
#  fu_hep <- get_param("Fhep.assay.correction",parameters,"calc_Hepatic_Clearance") 
#   #try(get_param("Fraction_unbound_hepatocyteassay",parameters,"calc_Hepatic_Clearance")) # fraction set if paramaterize function called with fu_hep_correct=TRUE
#  #if (class(fu_hep) == "try-error") fu_hep <- 1
## Correct for fraction of chemical unbound in in vitro hepatocyte assay:
#  Clint <- Clint / fu_hep
#
#  fub <- get_param("Funbound.plasma",parameters,"calc_Hepatic_Clearance") # unitless fraction
  Clint <- parameters[["Clint"]]
  fup <- parameters[["Funbound.plasma"]]
  if(!restrictive.clearance) fup <- 1
  
  fu_hep <- parameters[["Fhep.assay.correction"]]
  Qtotal.liverc <- parameters[["Qtotal.liverc"]] # L/h/kgBW
  Vliverc <- parameters[["Vliverc"]] #  L/kg BW
  liver.density <- parameters[["liver.density"]] # g/mL
  Dn <- parameters[["Dn"]] #
  million.cells.per.gliver <- parameters[["million.cells.per.gliver"]] # 10^6 cells/g-liver

  if(!(tolower(model) %in% c("well-stirred","parallel tube","dispersion","unscaled")))
    stop("Model other than \"well-stirred,\" \"parallel tube,\", \"dispersion\", or \"unscaled\" specified.")

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
          adjusted.Funbound.plasma=adjusted.Funbound.plasma)
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
    CLh <- Qtotal.liverc*(1 - 4*a/((1+a)^2*exp((a-1)/2/Dn)-(1-a)^2*exp(-(a+1)/2/Dn)))
  }
  if (!suppress.messages) cat("Hepatic clearance calculated with the",hepatic.model,"model in units of L/h/kg.\n")  
  return(as.numeric(CLh))
}
