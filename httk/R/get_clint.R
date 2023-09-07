#' Retrieve and parse intrinsic hepatic clearance
#' 
#' This function retrieves the chemical- and species-specific intinsic
#' hepatic clearance (\ifelse{html}{\out{Cl<sub>int</sub>}}{\eqn{Cl_{int}}},
#' inits of uL/min/million hepatocytes)  
#' from \code{\link{chem.physical_and_invitro.data}}. 
#' If that parameter is described by a distribution (that is, a median, 
#' lower-, upper-95th percentile and p-value separated by commas) this 
#' function splits those quantiles into separate values. Most 
#' \ifelse{html}{\out{Cl<sub>int</sub>}}{\eqn{Cl_{int}}} values have an
#' accompanying p-value indicating the probability that no decrease was 
#' observed. If the p-values exceeds a threhsold (default 0.05) the clearance is
#' set to zero (no clearance). Some values extracted from the literature do not
#' have a p-value.
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param default.to.human Substitutes missing hepatic clearance with
#' human values if true.
#' 
#' @param force.human.clint If a non-human species value (matching argument
#' species) is available, it is ignored and the human intrinsic clearance is 
#' used
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' 
#' @return list containing:
#' \item{CLint.point}{Point estimate (central tendency) of the intrinsic hepatic clearance}
#' \item{Clint.dist}{Quantiles of a distribution (median, lower, upper 95th percentiles) and pvalue}
#' \item{Clint.pvalue}{pvalue for whether disapperance of parent compound was observed}
#'
#' @author John Wambaugh
#'
#' @keywords Parameter in-vitro
#'
#' @seealso \code{\link{chem.physical_and_invitro.data}}
get_clint <- function(chem.cas=NULL,
                    chem.name=NULL,
                    dtxsid = NULL,
                    species="Human",
                    default.to.human=FALSE,
                    force.human.clint=FALSE,
                    suppress.messages=FALSE,
                    clint.pvalue.threshold=0.05)
{
 # We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')
    
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

  # Assume that Clint exists for the combination of species specified
  # Clint has units of uL/min/10^6 cells
  Clint.db <- try(get_invitroPK_param(
                    "Clint",
                    species,
                    chem.cas=chem.cas),
                silent=TRUE)
  # Check that the trend in the CLint assay was significant:
  Clint.pValue <- try(get_invitroPK_param(
                        "Clint.pValue",
                        species,
                        chem.cas=chem.cas),
                    silent=TRUE)
  
  # Need to check cases of missing data on Clint 
  if((is(Clint.db,"try-error") & species == "Human")){ # Case 1: Human does not have Clint values in current HTTK data
    stop("Missing metabolic clearance data for Human. \n\
          Check for complete invitro TK data using `<chem.id>%in%get_cheminfo(info='chem.id.col',model = 'MODEL',...)`.")
  }else if(is(Clint.db,"try-error") & species !="Human" & !default.to.human & !force.human.clint){ # Case2: Species does not have Clint values in current HTTK data and default.to.human == FALSE
    stop("Missing metabolic clearance data for given species. \n\
          Set default.to.human to true to substitute human value.")
  }
  
  if ((is(Clint.db,"try-error") & default.to.human) || force.human.clint){
    Clint.db <- try(get_invitroPK_param(
                      "Clint",
                      "Human",
                      chem.cas=chem.cas),
                  silent=TRUE)
    Clint.pValue <- try(get_invitroPK_param(
                          "Clint.pValue",
                          "Human",
                          chem.cas=chem.cas),
                      silent=TRUE)

    if (!suppress.messages) warning(paste(species,"coerced to Human for metabolic clearance data."))
  }
  
  if (is(Clint.db,"try-error") & !force.human.clint){ # Case 3: Species does not have Clint values in current HTTK data and default.to.human == TRUE; HTTK data also does not have Human data
    stop("Missing metabolic clearance data for given species and human. \n\
          Check for complete invitro TK data using <chem.id>%in%get_cheminfo(info='chem.id.col',model = 'MODEL',...).")
  }else if(is(Clint.db,"try-error") & force.human.clint){ # Case 4: The function is forced to obtain Human values, but HTTK data does not have any Human Clint values for specified compound
    stop("Missing metabolic clearance data for Human. \n\
          Check for complete Human invitro TK data using `<chem.id>%in%get_cheminfo(info='chem.id.col',model = 'MODEL')`.")
  }
    
  # Check if clint is a point value or a distribution, if a distribution, use the median:
  if (nchar(Clint.db) - nchar(gsub(",","",Clint.db))==3) 
  {
    Clint.dist <- Clint.db
    Clint.point <- as.numeric(strsplit(Clint.db,",")[[1]][1])
    Clint.pValue <- as.numeric(strsplit(Clint.db,",")[[1]][4])
    if (!suppress.messages) warning("Clint is provided as a distribution.")
  } else {
    Clint.point  <- Clint.db
    Clint.dist <- NA
  }
  
  # Check for significant pvalue:
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint.point <- 0
  
  return(list(Clint.point = set_httk_precision(Clint.point),
              Clint.dist = set_httk_precision(Clint.dist),
              Clint.pValue = set_httk_precision(Clint.pValue)))
}
