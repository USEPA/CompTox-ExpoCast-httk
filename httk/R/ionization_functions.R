# alpha: Ratio of Distribution coefficient D of totally charged species and that of the neutral form
calc_dow <- function(Pow,pH=NA,pKa_Donor=NA,pKa_Accept=NA,fraction_charged=NULL,alpha=0.001) 
{
  # Octanol:water distribution coefficient,
  if (is.null(fraction_charged))
  {
    if (is.na(pH)) stop("pH or fraction_charged must be specified in calc_dow.")
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_charged  <- ionization[["fraction_charged"]]
  }
  
  Dow <- Pow*(1 + (alpha - 1)*fraction_charged)
  return(Dow)
}

#' Calculate the ionization.
#' 
#' This function calculates the ionization of a compound at a given pH. The 
#' pKa's are either entered as parameters or taken from a specific compound in
#' the package.
#' 
#' The arguments pKa_Donor and pKa_Accept may be single numbers, characters, or 
#' vectors. We support characters because there are many instances with multiple 
#' predicted values and all those values can be included by concatenating with 
#' commas (for example, pKa_Donor = "8.1,8.6". Finally, pka_Donor and pKa_Accept 
#' may be vectors of characters representing different chemicals or instances of
#' chemical parameters to allow for uncertainty analysis.
#' 
#' The fractions are calculated by determining the coefficients for each
#' species and dividing the particular species by the sum of all three.  The
#' positive, negative and zwitterionic/neutral coefficients are given by:
#' \deqn{zwitter/netural = 1} \deqn{for(i in 1:pkabove) negative = negative +
#' 10^(i * pH - pKa1 - ... - pKai)} \deqn{for(i in 1:pkbelow) positive =
#' positive + 10^(pKa1 + ... + pKai - i * pH)} where i begins at 1 and ends at
#' the number of points above(for negative) or below(for positive) the
#' neutral/zwitterionic range.  The neutral/zwitterionic range is either the pH
#' range between 2 pKa's where the number of acceptors above is equal to the
#' number of donors below, everything above the pKa acceptors if there are no
#' donors, or everything below the pKa donors if there are no acceptors.  Each
#' of the terms in the sums represent a different ionization.
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param parameters Chemical parameters from a parameterize_MODEL function,
#' overrides chem.name and chem.cas.
#' 
#' @param pH pH where ionization is evaluated.
#' 
#' @param pKa_Donor Compound H dissociation equilibirum constant(s).
#' Overwrites chem.name and chem.cas.
#' 
#' @param pKa_Accept Compound H association equilibirum constant(s).
#' Overwrites chem.name and chem.cas.
#' 
#' @return
#' \item{fraction_neutral}{fraction of compound neutral}
#' \item{fraction_charged}{fraction of compound charged}
#' \item{fraction_negative}{fraction of compound negative}
#' \item{fraction_positive}{fraction of compound positive}
#' \item{fraction_zwitter}{fraction of compound zwitterionic}
#' 
#' @author Robert Pearce and John Wambaugh
#'
#' @references 
#' Pearce, Robert G., et al. "Evaluation and calibration of
#' high-throughput predictions of chemical distribution to tissues." Journal of
#' Pharmacokinetics and Pharmacodynamics 44.6 (2017): 549-565.
#'
#' Strope, Cory L., et al. "High-throughput in-silico prediction of ionization 
#' equilibria for pharmacokinetic modeling." Science of The Total Environment 
#' 615 (2018): 150-160.
#'
#' @keywords Parameter
#' 
#' @examples
#' # Donor pKa's 9.78,10.39 -- Should be almost all neutral at plasma pH:
#' out <- calc_ionization(chem.name='bisphenola',pH=7.4)
#' print(out)
#' out[["fraction_neutral"]]==max(unlist(out))
#'
#' # Donor pKa's 9.78,10.39 -- Should be almost all negative (anion) at higher pH:
#' out <- calc_ionization(chem.name='bisphenola',pH=11)
#' print(out)
#' out[["fraction_negative"]]==max(unlist(out))
#'
#' # Fictious compound, should be almost all all negative (anion):
#' out <- calc_ionization(pKa_Donor=8,pKa_Accept="1,4",pH=9)
#' print(out)
#' out[["fraction_negative"]]>0.9
#'
#' # Donor pKa 6.54 -- Should be mostly negative (anion):
#' out <- calc_ionization(chem.name='Acephate',pH=7)
#' print(out)
#' out[["fraction_negative"]]==max(unlist(out))
#'
#' #Acceptor pKa's "9.04,6.04"  -- Should be almost all positive (cation) at plasma pH:
#' out <- calc_ionization(chem.cas="145742-28-5",pH=7.4)
#' print(out)
#' out[["fraction_positive"]]==max(unlist(out))
#'
#' #Fictious Zwitteron:
#' out <- calc_ionization(pKa_Donor=6,pKa_Accept="8",pH=7.4)
#' print(out)
#' out[["fraction_zwitter"]]==max(unlist(out))
#'
#' @export calc_ionization
calc_ionization <- function(
                     chem.cas=NULL,
                     chem.name=NULL,
                     dtxsid=NULL,
                     parameters=NULL,
                     pH=NULL,
                     pKa_Donor=NULL,
                     pKa_Accept=NULL)
{
  if (is.null(pH)) stop("pH is required to calculate the ionization.")

  if ((!is.null(chem.cas) | !is.null(chem.name) | !is.null(dtxsid)) & 
      !all(c("pKa_Donor","pKa_Accept") %in% names(parameters)) &
      (is.null(pKa_Donor) & is.null(pKa_Accept))) 
  {
    out <- get_chem_id(
             chem.cas=chem.cas,
             chem.name=chem.name,
             dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    parameters$pKa_Donor <- 
      suppressWarnings(get_physchem_param("pKa_Donor",chem.cas=chem.cas))
    parameters$pKa_Accept <- 
      suppressWarnings(get_physchem_param("pKa_Accept",chem.cas=chem.cas))
  }
  
  # Check if any of these arguments are vectors:
  if (length(pKa_Donor) < 2 & length(pKa_Accept) < 2 & length(pH) < 2)
  {
    if (all(c("pKa_Donor","pKa_Accept") %in% names(parameters)))
    {
      pKa_Donor <- parameters$pKa_Donor
      pKa_Accept <- parameters$pKa_Accept
    } else if(!is.null(pKa_Donor) | !is.null(pKa_Accept))
    {
  # A null value means no argument provided, while NA means no equlibria:
      if (is.null(pKa_Donor)) pKa_Donor <- NA
      if (is.null(pKa_Accept)) pKa_Accept <- NA
    } else {
      stop(
  "Either pKa_Donor and pKa_Accept must be in input parameters or chemical identifier must be supplied.")
    }
    # Number of ionizations to calculate:
    if (is.null(parameters))
    {
      calculations <- 1
  # If pKa's aren't actually varying let's not waste computing time:    
    } else if (all(c(length(unique(parameters$pKa_Donor)==1),
        length(unique(parameters$pKa_Accept)==1),
        length(unique(parameters$pH)==1))))
    {
      calculations <- 1
    } else {
      calculations <- max(c(
      length(unique(parameters$pKa_Donor)),
      length(unique(parameters$pKa_Accept)),
      length(unique(parameters$pKa_pH))))
    }
  } else calculations <- max(c(
    length(unique(pKa_Donor)),
    length(unique(pKa_Accept)),
    length(unique(pH))))
  
  fraction_neutral <- NULL
  fraction_charged <- NULL
  fraction_negative <- NULL
  fraction_positive <- NULL
  fraction_zwitter <- NULL
  
  for (index in 1:calculations)
  {
    if (calculations > 1)
    {
      this.pKa_Donor <- pKa_Donor[[index]]
      this.pKa_Accept <- pKa_Accept[[index]]
      this.pH <- pH[[index]]
    } else {
      this.pKa_Donor <- pKa_Donor[[1]]
      this.pKa_Accept <- pKa_Accept[[1]]
      this.pH <- pH[[1]]
    }
    if (is.null(this.pKa_Donor)) this.pKa_Donor <- NA
    if (is.null(this.pKa_Accept)) this.pKa_Accept <- NA
    if (!is.na(this.pKa_Donor))
    {
      if (is.character(this.pKa_Donor)) this.pKa_Donor <- 
      {
        if (regexpr(",",this.pKa_Donor)!=-1)
          sort(as.numeric(unlist(strsplit(this.pKa_Donor, ","))))
        else this.pKa_Donor <- suppressWarnings(as.numeric(this.pKa_Donor))
      }
    }

    if (!is.na(this.pKa_Accept))
    {
      if (is.character(this.pKa_Accept)) this.pKa_Accept <- 
      {
        if (regexpr(",",this.pKa_Accept)!=-1)
          sort(as.numeric(unlist(strsplit(this.pKa_Accept, ","))))
        else this.pKa_Accept <- suppressWarnings(as.numeric(this.pKa_Accept))
      }
    }

  # Need to calculate the amount of un-ionized parent:

  # Multiple equilibirum points may still be separated by commas, split them into vectors here:
     
    if(all(is.na(this.pKa_Donor))) this.pKa_Donor <- NULL
    if(all(is.na(this.pKa_Accept))) this.pKa_Accept <- NULL
  
  # Make a vector of all equilibirum points:
    eq.points <- c(this.pKa_Donor,this.pKa_Accept)
    if (all(!is.null(eq.points)))
    {
  # Annotate whether each equilibirum point is a H-donation or acceptance:
      eq.point.types <- c(rep("Donate",length(this.pKa_Donor)),
        rep("Accept",length(this.pKa_Accept)))
      eq.point.types <- eq.point.types[order(eq.points)]    #label each point
      eq.points <- eq.points[order(eq.points)]     #order points
    }
  
    neutral <- 0
    negative <- 0
    positive <- 0
    zwitter <- 0
    denom <- 1
      
    if(is.null(this.pKa_Donor) & is.null(this.pKa_Accept)){
      neutral <- 1
    }else{
      nz <- NULL;
    #Find where charge is neutral or zwitter
      if(all(eq.point.types == "Donate") | all(eq.point.types == "Accept")){
        neutral <- 1
        if(all(eq.point.types == "Donate")){
          nz <- 0
        }else  nz <- length(eq.points)
      }else{ 
        for(i in 1:(length(eq.points) - 1)){
          charge <- 
            sum(eq.point.types[(i+1):length(eq.point.types)]=="Accept") - 
            sum(eq.point.types[1:i]=="Donate")
          if (charge == 0)
          {
            nz <- i
            if(sum(eq.point.types[(i+1):length(eq.point.types)]=="Accept") == 0)
            {
              neutral <- 1
            } else zwitter <- 1
          }  
        }   
      }    
      if (nz == 0) {
        for (i in 1:length(eq.points))
        {
          negative <- negative + 10^(i * pH - sum(eq.points[1:i]))
        }
      } else if (nz == length(eq.points))
        {
        for (i in 1:length(eq.points)) 
        {
          positive <- positive + 
            10^(sum(eq.points[(length(eq.points) + 1 - i):
              length(eq.points)])- i * pH)
        }
      } else {
        for (i in 1:nz) 
        {
          positive <- positive + 10^(sum(eq.points[(nz + 1 - i):nz])- i * pH)
        }
        for (i in 1:(length(eq.points)-nz)) 
        {
          negative <- negative + 10^(i * pH - sum(eq.points[(nz+1):(nz+i)])) 
        }
      }
      denom <- denom + positive + negative      
    }
    if (length(neutral)>1) browser()
    fraction_neutral[index] <- neutral/denom
    fraction_charged[index] <- (negative+positive)/denom
    fraction_negative[index] <- negative/denom
    fraction_positive[index] <- positive/denom
    fraction_zwitter[index] <- zwitter/denom
  }
  
  # If pKa's aren't actually varying let's not waste computing time:  
  if (!is.null(parameters))
  {
    if (length(parameters$pKa_Donor)>1 & calculations == 1)
    {
      fraction_neutral <- rep(fraction_neutral,length(parameters$Pow))
      fraction_charged <- rep(fraction_charged,length(parameters$Pow))
      fraction_negative <- rep(fraction_negative,length(parameters$Pow))
      fraction_positive <- rep(fraction_positive,length(parameters$Pow))
      fraction_zwitter <- rep(fraction_zwitter,length(parameters$Pow))
    }
  }  
  
  return(list(fraction_neutral = fraction_neutral,
    fraction_charged = fraction_charged,
    fraction_negative = fraction_negative,
    fraction_positive = fraction_positive,
    fraction_zwitter = fraction_zwitter))
}

is_acid <- function(pH=7,pKa_Donor=NA,pKa_Accept=NA,fraction_negative=NULL)
{
  if (is.null(fraction_negative))
  {
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_negative  <- ionization[["fraction_negative"]]
  }
  if (fraction_negative > 0.5) return(TRUE)
  else return(FALSE)
}

is_base <- function(pH=7,pKa_Donor=NA,pKa_Accept=NA,fraction_positive=NULL)
{
  if (is.null(fraction_positive))
  {
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_positive  <- ionization[["fraction_positive"]]
  }
  if (fraction_positive > 0.5) return(TRUE)
  else return(FALSE)
}

is_neutral <- function(pH=7,pKa_Donor=NA,pKa_Accept=NA,fraction_postive=NULL)
{
  if (is.null(fraction_neutral))
  {
    ionization <- calc_ionization(pH=pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_neutral  <- ionization[["fraction_neutral"]]
  }
  if (fraction_neutral > 0.5) return(TRUE)
  else return(FALSE)
}
