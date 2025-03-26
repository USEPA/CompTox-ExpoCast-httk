#' Calculate the distribution coefficient
#' 
#' This function estimates the ratio of the equilibrium concentrations of
#' a compound in octanol and water, taking into account the charge of the
#' compound. Given the pH, we assume the neutral (uncharged) fraction of
#' compound partitions according to the hydrophobicity 
#' (\ifelse{html}{\out{P<sub>ow</sub>}}{\eqn{P_{ow}}}). We assume that
#' only a fraction alpha (defaults to 0.001 -- Schmitt (2008)) of the charged
#' compound partitions into lipid (octanol):
#' \ifelse{html}{\out{D<sub>ow</sub> = P<sub>ow</sub>*(F<sub>neutral</sub> + alpha*F<sub>charged</sub>)}}{\deqn{D_{ow} = P_{ow}*(F_{neutral} + \alpha*F_{charged})}}
#' Fractions charged are calculated
#' according to hydrogen ionization equilibria (pKa_Donor, pKa_Accept) using
#' \code{\link{calc_ionization}}.
#' 
#' @param Pow Octanol:water partition coefficient (ratio of concentrations)
#' 
#' @param fraction_charged Fraction of chemical charged at the given pH
#' 
#' @param alpha Ratio of Distribution coefficient D of totally charged species and that of the neutral form
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
#' @return Distribution coefficient (numeric)
#' 
#' @author Robert Pearce and John Wambaugh
#'
#' @references 
#' Schmitt, Walter. "General approach for the calculation of tissue to plasma 
#' partition coefficients." Toxicology in vitro 22.2 (2008): 457-467.
#'
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
#' @seealso \code{\link{calc_ionization}}
#' 
#' @export calc_dow
calc_dow <- function(Pow=NULL,
                     chem.cas=NULL,
                     chem.name=NULL,
                     dtxsid=NULL,
                     parameters=NULL,
                     pH=NULL,
                     pKa_Donor=NULL,
                     pKa_Accept=NULL,
                     fraction_charged=NULL,
                     alpha=0.001) 
{
# Check to see if Pow was provided:
  if (is.null(Pow))
# If not, see if we have chem id's:
  {
    if ((!is.null(chem.cas) | !is.null(chem.name) | !is.null(dtxsid))) 
    {
      if (is.null(dtxsid))
      {
        out <- get_chem_id(
               chem.cas=chem.cas,
               chem.name=chem.name,
               dtxsid=dtxsid)
        dtxsid <- out$dtxsid
      }
      Pow <- 
        suppressWarnings(10^get_physchem_param("logP", dtxsid=dtxsid))
    } else if (!all(c("Pow") %in% names(parameters)))
# If not see if "parameters" was provided
    {
      Pow <- parameters$Pow
    } else stop("Must provide chemical descriptors or identifiers for calc_dow")  
  }

# Check to see if fraction_charged was provided:
  if (is.null(fraction_charged))
  {
    # If not, see if they gave us pKa_Accept and pKa_Donor:
    if (is.null(pKa_Donor) | is.null(pKa_Accept))
    {
      if ((!is.null(chem.cas) | !is.null(chem.name) | !is.null(dtxsid))) 
      {
        if (is.null(dtxsid))
        {
          out <- get_chem_id(
                 chem.cas=chem.cas,
                 chem.name=chem.name,
                 dtxsid=dtxsid)
          dtxsid <- out$dtxsid
        }
        pKa_Donor <- 
          suppressWarnings(get_physchem_param("pKa_Donor", dtxsid=dtxsid))
        pKa_Accept <- 
          suppressWarnings(get_physchem_param("pKa_Accept", dtxsid=dtxsid))
      } else if (all(c("pKa_Donor","pKa_Accept") %in% names(parameters)))
  # If not see if "parameters" was provided
      {
        pKa_Donor <- parameters$pKa_Donor
        pKa_Accept <- parameters$pKa_Accept
      } else stop("Must provide chemical descriptors or identifiers for calc_dow")  
    }
 
    if (is.null(pH)) stop("pH or fraction_charged must be specified in calc_dow.")
    ionization <- calc_ionization(pH=pH,
                                  pKa_Donor=pKa_Donor,
                                  pKa_Accept=pKa_Accept)
    fraction_charged  <- ionization[["fraction_charged"]]
  }
  
# Schmitt (2008) section 2.5.1: neutral lipid:water partition coefficient
# This is a generalized version of Schmitt (2008) equations 13 and 14 to
# calculate Dow. 
# Note that this form captures fraction neutral and zwitterions, that is
# 1 - fraction_charged = fraction_neutral + fraction_zwitter):
  Dow <- Pow*(1 + (alpha - 1)*fraction_charged)
  
  return(set_httk_precision(Dow))
}

#' Calculate the ionization.
#' 
#' This function calculates the ionization of a compound at a given pH. The 
#' pKa's are either entered as parameters or taken from a specific compound in
#' the package. The arguments pKa_Donor and pKa_Accept may be single numbers, characters, or 
#' vectors. We support characters because there are many instances with multiple 
#' predicted values and all those values can be included by concatenating with 
#' commas (for example, pKa_Donor = "8.1,8.6". Finally, pka_Donor and pKa_Accept 
#' may be vectors of characters representing different chemicals or instances of
#' chemical parameters to allow for uncertainty analysis. A null value for
#' pKa_Donor or pKa_Accept is interpretted as no argument provided, while " " 
#' is taken as a prediction of no ionization possible at any pH.
#'
#' It is very important to note that pKb = 14 - pKa. But if a predictor gives us
#' a doinor pKa, we just accept it as a pKa.
#' 
#' For hydrogen donor sites, a hydrogen is present in the molecule that can be 
#' donated to the solution if the concentration of hydrogens gets low enough. 
#' This causes the molecule to become more negatively charged. This is an acid. 
#' For hydrogen acceptor suits a location exist in the molecule that can accept 
#' an additional history if the concentration of hydrogens gets sufficiently 
#' high. This causes the molecule to become more positively charged. This is a 
#' base.
#'
#' We make several assumptions about ionization in order to make our calculations.
#' First, we assume ionization is either due to either "donating" (losing) a
#' hydrogen ion (a positively charge proton) to the solution or by "accepting"
#' (gaining) a hydrogen ion from the solution. Generally, acids are hydrogen 
#' donors
#' and bases are hydrogen acceptors. Second, pH is the negative log10 
#' concentration
#' of hydrogen atoms. The lower the pH, the more hydrogen atoms. So, acids 
#' donate
#' their hydrogen atoms as pH of the solution increases. Bases accept their 
#' hydrogen
#' atoms as pH decreases. Third, each predicted pKa is a prediction that a 
#' specific
#' location (or site) on molecule X can either donate or accept a hydrogen. 
#' Fourth, the pKa
#' value indicates the pH at which half of the molecules of X have ionized at
#' the site, and half have not. The concentration of the two forms are equal.
#' Fifth, if there are N pKa's for molecule X, then there are N sites that can
#' ionize. Technically this means that there are 2^N different ionization states
#' for molecule X (where each site is or is not ionized). However, pKa 
#'predictors
#' give the equlibrium only for pairs of ionization states. So, we only consider
#' N + 1 ionizations states for X -- the state immediately above and below each
#' pKa.
#'
#' To understand the different charge states we annotate the nonionizable 
#' backbone
#' of a molecule as "X". For each site on X that is capable of donating a 
#' hydrogen
#' we add a "D" to the right of "X". For each site on X that has accepted a 
#' hydrogen,
#' we add a "A" to the right of "X". We read the A's and D's from left to right,
#' with the one occuring at the lowest pH first. So a typical acid ionization 
#' would be:
#' XD -> X- and a typical base ionization would be XA+ -> X. Where things get 
#' complicated
#' is if there are multiple donor and acceptor states. In particular, it is 
#' possible 
#' for a compound to have a net zero charge, but be simultaneously positively 
#' and
#' negatively charged. Such a state is called a Zwitter ion. For example:
#' XDAA+ -> XAA++ -> XA+ -> XA -> X- . The state XA is technically neutral 
#' because
#' X has donated one hydrogen, but also accepted one hydrogen. XA is a Zwitter 
#' ion.
#'
#' Each pKa gives the equlibrium ratio of two states pH - pKa = log10[X/XD] for
#' donation or pOH - pka = log10[X/XA] for accepting. pOH = 14 - pH. Separating the
#' logarithm into log10[X] - log10[XD] lets us see that Cn = Xn - Xn-1 where
#' Cn = pH -pKa for donor pKa's and Cn = 14 - pH - pKa for acceptor pKa's.
#' We can rewrite log10Xn = Sum_i=1:n Ci + log10X1.  So we can calculate each Xn
#' by summing all the ratios between Xn and the lowest state (X1). 
#' Then, by requiring that all Xi sum to 1, we have:
#' 1 = Sum_i=1:N 10^Xi = Sum_i=1:N 10^(Sum_j=1:i (Cj + log10X1)) = X1 * Sum_i=1:N 10^(Sum_j=1:i Cj)
#' so that X1 = 1 / Sum_i=1:N 10^(Sum_j=1:i Cj)
#'
#' The sum im the denominator is the ratio from X1 to each state (including X1).
#' We use a table called "charge_matrix" to keep track of all N + 1 ionization
#' states and the ratio of each state to the next. We use these ratios to calculate
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
#' @param return_charge_matrix If TRUE, the function returns a table describing
#' each ionization state considered by the calculations in this function
#' (defaults to FALSE)
#' 
#' @return
#' \item{fraction_neutral}{fraction of compound neutral}
#' \item{fraction_charged}{fraction of compound charged}
#' \item{fraction_negative}{fraction of compound negative}
#' \item{fraction_positive}{fraction of compound positive}
#' \item{fraction_zwitter}{fraction of compound zwitterionic}
#' \item{charge_matrix}{Description of each ionization state if argument return_charge_matrix==TRUE}
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
#' # Ficticious compound, should be almost all all negative (anion):
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
#' #Ficticious Zwitteron:
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
                     pKa_Accept=NULL,
                     return_charge_matrix = FALSE)
{
  if (is.null(pH)) stop("pH is required to calculate the ionization.")

  if ((!is.null(chem.cas) | !is.null(chem.name) | !is.null(dtxsid)) & 
      !all(c("pKa_Donor","pKa_Accept") %in% names(parameters)) &
# A null value means no argument provided, while NA means no equlibria:
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
  
  if (all(c("pKa_Donor","pKa_Accept") %in% names(parameters)))
  {
    pKa_Donor <- parameters$pKa_Donor
    pKa_Accept <- parameters$pKa_Accept
  } else if(!is.null(pKa_Donor) | !is.null(pKa_Accept))
  {
# If one of pKa_Donor/Accept is specified but not the other, we assume the other
# is not present:
    if (is.null(pKa_Donor)) pKa_Donor <- NA
    if (is.null(pKa_Accept)) pKa_Accept <- NA
  } else {
    stop(
"Either pKa_Donor and pKa_Accept must be in input parameters or chemical identifier must be supplied.")
  }
  
  # Check if any of these arguments are vectors:
  if (length(pKa_Donor) < 2 & length(pKa_Accept) < 2 & length(pH) < 2)
  {
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
  } else {
    calculations <- max(c(
    length(unique(pKa_Donor)),
    length(unique(pKa_Accept)),
    length(unique(pH))))
  }
  
  # Define NULL vectors to hold multiple values if multiple sets of pKa's are
  # given (calculations > 1):
  fraction_neutral <- NULL
  fraction_charged <- NULL
  fraction_negative <- NULL
  fraction_positive <- NULL
  fraction_zwitter <- NULL
  # charge_matrix is a data.frame so need to use a list:
  charge_matrix_list <- list()
  
  for (index in 1:calculations)
  {
    if (calculations > 1)
    {
      this.pKa_Donor <- pKa_Donor[[min(index,length(pKa_Donor))]]
      this.pKa_Accept <- pKa_Accept[[min(index,length(pKa_Donor))]]
      this.pH <- pH[[min(index,length(pKa_Donor))]]
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
          suppressWarnings(
            sort(as.numeric(unlist(strsplit(this.pKa_Donor, ",")))))
        else this.pKa_Donor <- suppressWarnings(as.numeric(this.pKa_Donor))
      }
    }

    if (!is.na(this.pKa_Accept))
    {
      if (is.character(this.pKa_Accept)) this.pKa_Accept <- 
      {
        if (regexpr(",",this.pKa_Accept)!=-1)
          suppressWarnings(
            sort(as.numeric(unlist(strsplit(this.pKa_Accept, ",")))))
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
    
    # There are one more charged species that eq.points:
    NUM.SPECIES <- 1 + length(eq.points)
    
    # Figure out the charge of each species of ions for the equilbria: 
    charge_matrix <- data.frame(pKa = c(NA,eq.points),
                                Type = c(NA,eq.point.types),
                                Donated = 0,
                                Accepted = 0,
                                Ratio = NA)
                                
    #Denote the parts of the molecule that don't ionize as "X":
    # Now, in order of pKa's from lowest to highest, indicate acceptor sites
    # with "A" and donor sites with "D":
    lowest.state <- "X"
    for (this.type in eq.point.types)
    {
      if (this.type == "Donate") lowest.state <- paste(lowest.state, 
                                                      "D", 
                                                      sep ="")
      else if (this.type == "Accept") lowest.state <- paste(lowest.state,
                                                            "A",
                                                            sep="")
    }
    charge_matrix[1,"Notation"] <- lowest.state
    # Now strip off ionization sites in order:
    for (this.row in 2:NUM.SPECIES)
    {
      prev.notation <- charge_matrix[this.row-1, "Notation"]
      charge_matrix[this.row, "Notation"] <- paste("X",
        substr(prev.notation, 3, nchar(prev.notation)),
        sep="")
    }                                                        
                               
    for (this.eq in 1:length(eq.points))
    {
      this.index <- which(charge_matrix[,"pKa"]==eq.points[this.eq])
      if (eq.point.types[this.eq] == "Donate")
      {
        # If it is an acid, (donates a hydrogen), then all species after this
        # equilibrium (including this one) have donated a hydrogen
        charge_matrix[this.index:NUM.SPECIES,"Donated"] <- 
          charge_matrix[this.index:NUM.SPECIES,"Donated"] + 1
      } else {
        # If it is an base, (accepts a hydrogen), then all species before this
        # equilibrium have donated a hydrogen
        charge_matrix[1:(this.index-1),"Accepted"] <- 
          charge_matrix[1:(this.index-1),"Accepted"] + 1
      }
    }  
    charge_matrix <- as.data.frame(charge_matrix)

    # Calculate the charge of each species:
    charge_matrix[,"Charge"] <- charge_matrix[,"Accepted"] - 
                                  charge_matrix[,"Donated"]
    # Indicate the true neutral and zwitterions (net neutral):
    charge_matrix[,"Neutral"] <- "No"
    charge_matrix[charge_matrix["Charge"] == 0, "Neutral"] <- "Zwitter"
    charge_matrix[charge_matrix["Charge"] == 0 &
                  charge_matrix[,"Accepted"] == 0 &
                  charge_matrix[,"Donated"] == 0, "Neutral"] <- "Neutral"
    
    # Determine the ratio of each state to the previous state using the pKa's:
    charge_matrix[1,"Ratio"] <- 0
    for (this.row in 2:NUM.SPECIES)
    if (!is.na(charge_matrix[this.row,"Type"]))
    {
      if (charge_matrix[this.row, "Type"] == "Donate")
         charge_matrix[this.row, "Ratio"] <- pH - charge_matrix[this.row,"pKa"]
      if (charge_matrix[this.row, "Type"] == "Accept")
         charge_matrix[this.row, "Ratio"] <- pH - charge_matrix[this.row,"pKa"]
 # Folllowing is for pKb's, which we don't have:
 #        charge_matrix[this.row, "Ratio"] <- 14 - pH - charge_matrix[this.row,"pKa"]
    }          
    
    # Calculate the ratio of each state to the lowest state: 
    # x1
    charge_matrix[1,"RatioToLowest"] <- 0
    # Calculate cumulative constant:
    for (this.row in 2:NUM.SPECIES)
    {
      charge_matrix[this.row,"RatioToLowest"] <- 
        charge_matrix[this.row-1,"RatioToLowest"] + 
        charge_matrix[this.row,"Ratio"]
    }
    # So the concentration x_n can be written as a function of the
    # concentration of the lowest state (x_1) and the ratios:
    # 
    # log10(x_n) = log10(x_1)+RatioToLowest
  
    # Find concentration of lowest state by requiring all state sum to 1:
    sum.constants <- sum(10^charge_matrix[,"RatioToLowest"])
    logx1 <- log10(1/sum.constants)
    
    # Use above expression for log10(x_n) to calculate fraction in each state:
    charge_matrix[,"Fraction"] <- 10^(logx1 + charge_matrix[,"RatioToLowest"])
    
    neutral.row <- which(charge_matrix[,"Neutral"] == "Neutral")
    negative.rows <- which(charge_matrix[,"Charge"] < 0)
    positive.rows <- which(charge_matrix[,"Charge"] > 0)
    zwitter.rows <- which(charge_matrix[,"Neutral"] == "Zwitter")
        
    fraction_neutral[index] <- sum(charge_matrix[neutral.row,"Fraction"])
    fraction_negative[index] <- sum(charge_matrix[negative.rows, "Fraction"])
    fraction_positive[index] <- sum(charge_matrix[positive.rows, "Fraction"])
    fraction_charged[index] <- fraction_negative[index] + fraction_positive[index] 
    fraction_zwitter[index] <- sum(charge_matrix[zwitter.rows, "Fraction"]) 
  }
  
  if (return_charge_matrix) 
  {
    charge_matrix[,"Fraction"] <- set_httk_precision(charge_matrix[,"Fraction"])
    charge_matrix_list[[index]] <- charge_matrix
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
      if (return_charge_matrix) charge_matrix_list <- list(charge_matrix)
    }
  }  
  
  out <- lapply(list(fraction_neutral = fraction_neutral,
    fraction_charged = fraction_charged,
    fraction_negative = fraction_negative,
    fraction_positive = fraction_positive,
    fraction_zwitter = fraction_zwitter), set_httk_precision)
  
  if (return_charge_matrix) out <- c(out, 
                                     list(charge_matrix = charge_matrix_list))
                                     
  return(out)
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
