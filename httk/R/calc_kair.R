#' Calculate air:matrix partition coefficients
#'
#' This function uses the methods colleced by Linakis et al. (2020) to calculate
#' air partition coefficients for blood, water, and mucus. 
#' 
#' The blood:air partition coefficient (PB:A) was calculated as 
#' \ifelse{html}{\out{P<sub>B:A</sub> = P<sub>B:A</sub> * R<sub>B:P</sub> / f<sub>up</sub>}}{\deqn{P_{B:A} = \frac{P_{B:A} * R_{B:P}}{f_{up}}}}
#' where P_B:A is the blood:air partition, RB:P is the blood:plasma partition
#' ratio, fup is the fraction unbound in the plasma, and
#' P_W:A is the water:air partition coefficient:
#' \ifelse{html}{\out{R * T<sub>body</sub> / HLC / P}}{\deqn{\frac{R * T_{body}}{HLC * P}}}
#' where R is the gas constant (8.314 J/mol/K), T_body is the 
#' species-specific body temperature (K) from \code{\link{physiology.data}},
#' HLC is the Henry's Law Constant (atm*m^3 / mol), and P is conversion factor 
#' from atmospheres to Pascals (1 atm = 101325 Pa).
#' 
#' In the isopropanol PBTK
#' model published by Clewell et al. (2001) it was noted that certain 
#' chemicals are likely to be absorbed into the mucus or otherwise
#' trapped in the upper respiratory tract (URT). Following Scott (2014), 
#' the air:mucus partition coefficient (PA:M) calculated as
#' \ifelse{html}{\out{log<sub>10</sub>(1/K<sub>water2air</sub>) - (log<sub>10</sub>(P<sub>ow</sub>) - 1) * 0.524}}{\deqn{log_{10}(\frac{1}{K_{water2air}}) - (log_{10}(P_{ow}) - 1) * 0.524}}
#' where Pow is the octanol/water partition coefficient
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#'
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#'
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model. Can include parameters "logHenry"
#' and "body_temp", but if not included standard values are looked up from httk tables.
#'
#' @param species Species used for body temperature, defaults to "Human"
#'
#' @param default.to.human Substitutes missing species-specific values with human values if
#' TRUE (default is FALSE).
#' 
#' @param adjusted.Funbound.plasma Uses Pearce et al. (2017) lipid binding adjustment
#' for Funbound.plasma (which impacts partition coefficients) when set to TRUE (Default).
#'
#' @param suppress.messages Whether or not the output messages are suppressed.
#'
#' @return A named list containing the blood:air, water:air, and mucus:air 
#' partition coefficients
#'
#' @author John Wambaugh and Matt Linakis
#'
#' @references Linakis, Matthew W., et al. "Development and evaluation of a 
#' high throughput inhalation model for organic chemicals." Journal of exposure 
#' Science & Environmental Epidemiology 30.5 (2020): 866-877.
#' 
#' Clewell III, Harvey J., et al. "Development of a physiologically based 
#' pharmacokinetic model of isopropanol and its metabolite acetone." 
#' Toxicological Sciences 63.2 (2001): 160-172.
#' 
#' Scott, John W., et al. "Tuning to odor solubility and sorption pattern in 
#' olfactory epithelial responses." Journal of Neuroscience 34.6 (2014): 
#' 2025-2036.
#' 
#' @keywords parameter
#'
#' @export calc_kair
#'
calc_kair <- function(
                 chem.cas=NULL,
                 chem.name=NULL,
                 dtxsid = NULL,
                 parameters = NULL,
                 species="Human",
                 adjusted.Funbound.plasma = TRUE,
                 default.to.human = FALSE,
                 suppress.messages = FALSE) 
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')

  # Look up the chemical name/CAS, depending on what was provided:
  if (any(is.null(chem.cas),is.null(chem.name),is.null(dtxsid)))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  }

  if (is.null(parameters) |  (!("logHenry" %in% names(parameters)))) 
  { 
    #henry's law in atm * m^3 / mol, converted atm to Pa
    logHenry = get_physchem_param(param = 'logHenry', 
                                  chem.cas=chem.cas,
                                  chem.name=chem.name,
                                  dtxsid=dtxsid) #for log base 10 compiled Henry's law values
  } else {
    logHenry <- parameters$logHenry 
  }
    
  if (is.null(parameters) |  (!("body_temp" %in% names(parameters)))) 
  { 
   # Check the species argument for capitilization problems and whether or not it is in the table:  
    if (!(species %in% colnames(physiology.data)))
    {
      if (toupper(species) %in% toupper(colnames(physiology.data)))
      {
        phys.species <- colnames(physiology.data)[
                                 toupper(colnames(physiology.data))==
                                 toupper(species)]
      } else stop(paste("Physiological PK data for",species,"not found."))
    } else phys.species <- species
  
  # Load the physiological parameters for this species
    this.phys.data <- physiology.data[,phys.species]
    names(this.phys.data) <- physiology.data[,1]
    #human body temperature of 310 Kelvin
    body_temp = as.numeric(this.phys.data['Average Body Temperature']) + 273.15 #C ->
  } else {
    body_temp <- parameters$body_temp 
  }
  
  if (!is.null(parameters))
  {
    if (!all(c("Rblood2plasma","Funbound.plasma","Pow") 
      %in% names(parameters))) 
      stop("Missing parameters needed in calc_kair.")
    
    Rblood2plasma <- parameters$Rblood2plasma
    Funbound.plasma <- parameters$Funbound.plasma
    Pow <- parameters$Pow
  } else {
    params <- parameterize_steadystate(
                                       chem.cas=chem.cas,
                                       chem.name=chem.name,
                                       dtxsid=dtxsid,
                                       default.to.human=default.to.human,
                                       adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                       suppress.messages=TRUE)
      Rblood2plasma <- params$Rblood2plasma
      Funbound.plasma <- params$Funbound.plasma
      Pow <- 10^get_physchem_param(param = 'logP', 
                                  chem.cas=chem.cas,
                                  chem.name=chem.name,
                                  dtxsid=dtxsid)  # Octanol:water partition coeffiecient
  }  

  hl <- 10^logHenry #Henry's constant in atm*m^3 / mol 
# Linakis et al. (2020) Equation 3:
# Gas constant 8.314 in units of J/(mol*K), body temperature 
  Kwater2air <- 8.314 * body_temp / (hl * 101325)   #101325 atm to Pa 
# Linakis et al. (2020) Equation 2:
  Kblood2air <- Kwater2air * Rblood2plasma / Funbound.plasma
# Linakis et al. (2020) Equation 4:
  lKair2muc <- log10(1/Kwater2air) - (log10(Pow) - 1) * 0.524 # Scott et al. 2014
  Kair2muc <- 10^(lKair2muc)
  Kmuc2air <- 1/Kair2muc
  
  outlist <- list(Kblood2air = Kblood2air,
                  Kwater2air = Kwater2air,
                  Kmuc2air = Kmuc2air)
                  
  return(lapply(outlist, set_httk_precision))
}
