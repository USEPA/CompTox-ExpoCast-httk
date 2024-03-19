#' Calculate Monte Carlo Oral Equivalent Dose
#' 
#' @description
#' This function converts a chemical plasma concetration to an oral adminstered
#' equivalent
#' dose (AED) using a concentration obtained from \code{\link{calc_mc_css}}.  
#' This function uses reverse dosimetry-based 
#' '\emph{in vitro-in vivo} extrapolation (IVIVE) 
#' for high throughput risk screening.  The user can input the
#' chemical and \emph{in vitro} bioactive concentration, select the TK
#' model, and then automatically predict the \emph{in vivo} AED
#' which would produce a body concentration equal to
#' the \emph{in vitro} bioactive concentration. This function relies on the
#' Monte Carlo method (via funcion \code{\link{create_mc_samples}} to simulate
#' both uncertainty and variability so that the result is a distribution of
#' equivalent doses, from which we provide specific quantiles
#' (specified by \code{which.quantile}), though the full set of predictions can
#' be obtained by setting \code{return.samples} to \code{TRUE}. 
#' 
#' @details
#' The chemical-specific steady-state concentration for a dose rate of 
#' 1 mg/kg body weight/day can be used for in IVIVE of
#' a bioactive \emph{in vitro} concentration by dividing the \emph{in vitro}
#' concentration
#' by the steady-state concentration to predict a dose rate (mg/kg/day) that
#' would produce that concentration in plasma. Using quantiles of the 
#' distribution (such as the upper 95th percentile) allow incorporation of 
#' uncertainty and variability into IVIVE.
#' 
#' This approach relies on thelinearity
#' of the models to calculate a scaling factor to relate in vitro
#' concentrations (uM) with AED. The scaling factor is the
#' inverse of the steady-state plasma concentration (Css) predicted
#' for a 1 mg/kg/day exposure dose rate
#' where \emph{in vitro} concentration [X] and Css must be in the same
#' units. Note that it is typical for \emph{in vitro} concentrations to be
#' reported in units of uM and Css in units of mg/L, in which case
#' one must be converted to the other. 
#' 
#' Reverse Dosimetry Toxicodynamic IVIVE
#' 
#' \if{html}{\figure{ivive.png}{options: width="60\%" alt="Reverse Dosimetry Toxicodynamic IVIVE"}}
#' \if{latex}{\figure{ivive.pdf}{options: width=12cm alt="Reverse Dosimetry Toxicodynamic IVIVE"}}
#' 
#' Figure from Breen et al. (2021) (\doi{10.1080/17425255.2021.1935867}) shows 
#' reverse dosimetry toxicodynamic IVIVE. Equivalent external dose is
#' determined by solving the TK model in reverse by deriving the external dose
#' (that is, TK model input) that produces a specified internal concentration 
#' (that is, TK model output). Reverse dosimetry and IVIVE using HTTK relies on 
#' the linearity of the models. We calculate a scaling factor to relate 
#' \emph{in vitro} 
#' concentrations (uM) to AEDs. The scaling 
#' factor is the inverse of the Css 
#' predicted for a 1 mg/kg/day exposure dose rate. We use Monte Carlo to 
#' simulate variability and propagate uncertainty; for example, to calculate an 
#' upper 95th percentile Css,95 for individuals who get higher plasma 
#' concentrations from the same exposure.
#'
#' The Monte Carlo methods used here were recently updated and described by
#' Breen et al. (submitted).
#'
#' All arguments after httkpop only apply if httkpop is set to TRUE and species
#' to "Human".
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitutes human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
#' 
#' Tissue concentrations are calculated for the pbtk model with oral infusion
#' dosing.  All tissues other than gut, liver, and lung are the product of the
#' steady state plasma concentration and the tissue to plasma partition
#' coefficient.
#' 
#' The six sets of plausible IVIVE
#' assumptions identified by Honda et al. (2019) 
#' (\doi{10.1371/journal.pone.0217564}) 
#' are: \tabular{lrrrr}{
#' \tab \emph{in vivo} Conc. \tab Metabolic Clearance \tab Bioactive Chemical
#' Conc. \tab TK Statistic Used* \cr Honda1 \tab Veinous (Plasma) \tab
#' Restrictive \tab Free \tab Mean Conc. \cr Honda2 \tab Veinous \tab
#' Restrictive \tab Free \tab Max Conc. \cr Honda3 \tab Veinous \tab
#' Non-restrictive \tab Total \tab Mean Conc. \cr Honda4 \tab Veinous \tab
#' Non-restrictive \tab Total \tab Max Conc. \cr Honda5 \tab Target Tissue \tab
#' Non-restrictive \tab Total \tab Mean Conc. \cr Honda6 \tab Target Tissue
#' \tab Non-restrictive \tab Total \tab Max Conc. \cr } *Assumption is
#' currently ignored because analytical steady-state solutions are currently
#' used by this function.
#' 
#' @param conc Bioactive in vitro concentration in units of uM. 
#'
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#'
#' @param chem.cas Either the CAS number or the chemical name must be
#' specified. 
#'
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#'
#' @param parameters Parameters from the appropriate parameterization function
#' for the model indicated by argument model
#'
#' @param suppress.messages Suppress text messages. 
#'
#' @param input.units Units of given concentration, default of uM but can also
#' be mg/L.
#'
#' @param output.units Units of dose, default of 'mgpkgpday' for mg/kg BW/ day or
#' 'umolpkgpday' for umol/ kg BW/ day.
#'
#' @param which.quantile Which quantile from Monte Carlo steady-state
#' simulation (\code{\link{calc_mc_css}}) is requested. Can be a vector. Note that 95th
#' concentration quantile is the same population as the 5th dose quantile. 
#'
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").  
#'
#' @param return.samples Whether or not to return the vector containing the
#' samples from the simulation instead of the selected quantile.
#'
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#'
#' @param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#'
#' @param tissue Desired steady state tissue concentration. Default is of NULL
#' typically gives whole body plasma concentration.
#'
#' @param concentration Desired concentration type: 'blood','tissue', or default 
#' 'plasma'. In the case that the concentration is for plasma, selecting "blood"
#' will use the blood:plasma ratio to estimate blood concentration. In the case
#' that the argument 'tissue' specifies a particular tissue of the body, 
#' concentration defaults to 'tissue' -- that is, the concentration in the 
#' If cocentration is set to 'blood' or 'plasma' and 'tissue' specifies a
#' specific tissue then the value returned is for the plasma or blood in that
#' specific tissue.
#'
#' @param IVIVE Honda et al. (2019) identified six plausible sets of
#' assumptions for \emph{in vitro-in vivo} extrapolation (IVIVE) assumptions.
#' Argument may be set to "Honda1" through "Honda6". If used, this function
#' overwrites the tissue, restrictive.clearance, and bioactive.free.invivo arguments.
#' See Details below for more information.
#'
#' @param model Model used in calculation,'gas_pbtk' for the gas pbtk model, 
#' 'pbtk' for the multiple compartment model,
#' '3compartment' for the three compartment model, '3compartmentss' for 
#' the three compartment steady state model, and '1compartment' for one 
#' compartment model. This only applies when httkpop=TRUE and species="Human",
#' otherwise '3compartmentss' is used.
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.Pab.default = 2,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fabsgut}} for further details.
#' 
#' @param calc.analytic.css.arg.list A list of options to pass to the analytic steady-state calculation function.
#' This includes `restrictive.clearance`, `bioactive.free.invivo`, `IVIVE`,
#' `wellstirred.correction`, and `adjusted.Funbound.plasma`.
#' 
#' @param ... Additional parameters passed to \code{\link{calc_mc_css}} for httkpop and
#' variance of parameters.
#'
#' @return Equivalent dose in specified units, default of mg/kg BW/day.
#'
#' @seealso \code{\link{calc_mc_css}}
#'
#' @seealso \code{\link{create_mc_samples}}
#'
#' @author John Wambaugh
#'
#' @references 
#' \insertRef{ring2017identifying}{httk} 
#'
#' \insertRef{wetmore2015incorporating}{httk}
#' 
#' \insertRef{honda2019using}{httk}
#' 
#' \insertRef{rowland1973clearance}{httk}
#' 
#' @keywords Monte-Carlo Steady-State
#'
#' @examples
#' \donttest{
#' # Set the number of samples (NSAMP) low for rapid testing, increase NSAMP 
#' # for more stable results. Default value is 1000:
#' NSAMP = 10
#' 
#' # Basic in vitro - in vivo extrapolation with httk, convert 0.5 uM in vitro
#' # concentration of chemical Surinabant to mg/kg/day:
#' set.seed(1234)
#' 0.5/calc_mc_css(chem.name="Surinabant", samples=NSAMP, output.units="uM")
#' 
#' # The significant digits should give the same answer as:
#' set.seed(1234)
#' calc_mc_oral_equiv(chem.name="Surinabant",conc=0.5,samples=NSAMP)  
#' 
#' # Note that we use set.seed to get the same sequence of random numbers for
#' # the two different function calls (calc_mc_css and calc_mc_oral_equiv)
#' 
#' # The following example should result in an error since we do not 
#' # estimate tissue partitioning with '3compartmentss'. 
#' set.seed(1234)                        
#' try(calc_mc_oral_equiv(0.1, chem.cas="34256-82-1",
#'                        which.quantile=c(0.05,0.5,0.95),
#'                        samples=NSAMP,
#'                        tissue='brain'))
#'        
#' set.seed(1234)
#' calc_mc_oral_equiv(0.1,chem.cas="34256-82-1", model='pbtk',
#'                    samples=NSAMP,
#'                    which.quantile=c(0.05,0.5,0.95), tissue='brain')
#'  
#' # We can also use the Monte Carlo functions by passing a table
#' # where each row represents a different Monte Carlo draw of parameters:
#' p <- create_mc_samples(chem.cas="80-05-7")
#' # Use data.table for steady-state plasma concentration (Css) Monte Carlo:
#' calc_mc_css(parameters=p)
#' # Using the same table gives the same answer:
#' calc_mc_css(parameters=p)
#' # Use Css for 1 mg/kg/day for simple reverse toxicokinetics 
#' # in Vitro-In Vivo Extrapolation to convert 15 uM to mg/kg/day:
#' 15/calc_mc_css(parameters=p, output.units="uM")
#' # Can do the same with calc_mc_oral_equiv:
#' calc_mc_oral_equiv(15, parameters=p)
#' }
#' 
#' @importFrom purrr compact 
#' @export calc_mc_oral_equiv
calc_mc_oral_equiv <- function(conc,
                               chem.name=NULL,
                               chem.cas=NULL,
                               dtxsid=NULL,
                               parameters=NULL,
                               which.quantile=0.95,
                               species="Human",
                               input.units='uM',
                               output.units='mgpkgpday',
                               suppress.messages=FALSE,
                               return.samples=FALSE,
                               restrictive.clearance=TRUE,
                               bioactive.free.invivo = FALSE,
                               tissue=NULL,
                               concentration = "plasma",
                               IVIVE=NULL,
                               model='3compartmentss',
                               Caco2.options = list(),
                               calc.analytic.css.arg.list = list(),
                               ...)
{
  # check if the input units are in concentration units - output error if TRUE
  if (!(tolower(input.units) %in% c('um','mg/l'))) 
    stop("Input units can only be uM or mg/L.")
  # check if the output units are in "amount / kg / day" - output error if TRUE
  if(grepl(output.units, pattern = "pkgpday$")==FALSE)
  {
    stop("Output units can only be umolpkgpday or mgpkgpday.")
  } else {
    # if the output units contain 'pkgpday' (i.e. '/kg/day'),
    # then remove this string from the output.units
    tmp.output.units <- gsub(x = output.units,
                             pattern = "pkgpday",
                             replacement = "")
    # check the output units are in acceptable amount units
    # if TRUE, then output error message
    if(!(tolower(tmp.output.units) %in% c('umol','mg'))){
      stop("Output units can only be umolpkgpday or mgpkgpday.")
    }
    # if the above logical check is FALSE, then use 'tmp.output.units'
    # for 'dose' unit conversions later in the function
  }
    
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    bioactive.free.invivo <- out[["bioactive.free.invivo"]]
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
    concentration <- out[["concentration"]]
  }
  
  # Unfortunately there are historically two ways to pass arguments to calc.analytic.css,
  # Throw a warning if the user specifies them twice:
  for (this.param in c("restrictive.clearance",
                       "bioactive.free.invivo",
                       "IVIVE",
                       "well.stirred.correction",
                       "adjusted.Funbound.plasma"))
# Check if parameter specified both ways:
    if (this.param %in% calc.analytic.css.arg.list)
# Check if value differs:
      if (eval(parse(text=this.param)) != calc.analytic.css.arg.list[[this.param]])
      { 
        warning(paste("Argument ", 
                      this.param, 
                      "overwrote same argument given in calc.analytic.css.arg.list"))
        calc.analytic.css.arg.list[[this.param]] <- eval(parse(text=this.param))
      }
  
  if ((bioactive.free.invivo == TRUE & !is.null(tissue)) | 
     (bioactive.free.invivo == TRUE & tolower(concentration) != "plasma"))
  {
    stop("Option bioactive.free.invivo only works with tissue = NULL and concentration = \"plasma\".\n
         Ctissue * Funbound.plasma is not a relevant concentration.\n
         Cfree_blood should be the same as Cfree_plasma = Cplasma*Funbound.plasma.")
  }
 
  # We need to know model-specific information (from modelinfo_[MODEL].R]) 
  # to set up the solver:
  model <- tolower(model)
  if (!(model %in% names(model.list)))            
  {
    stop(paste("Model",model,"not available. Please select from:",
      paste(names(model.list),collapse=", ")))
  } 
  
  # Error handling for tissue argument:
  if (!is.null(tissue))
  {
    if (is.null(model.list[[model]]$alltissues))
    {
      stop(paste("Tissues are not available for model", model))
    }
    if (!(tissue %in% model.list[[model]]$alltissues))
    {
      stop(paste("Tissue", tissue, "not available for model", model))
    }
  }  
  
  # Error handling for concentration arugment:
  if (!(concentration %in% c("blood","tissue","plasma")))
  {
    stop("Concentration must be one of blood, tissue, or plasma")
  }
    
  if (!is.null(tissue) & tolower(concentration) != "tissue"){
    concentration <- "tissue"
    warning(
      "Tissue selected. Overwriting option for concentration with \"tissue\".")
  }
  
  
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  well.stirred.correction <- adjusted.Funbound.plasma <- NULL
  #End R CMD CHECK appeasement.
  

  # output units are in '<input.units>/mg/kg/day' for 'Css'
  # (i.e. 'mg/L / kg/day' or 'uM / kg/day')
  Css <- try(do.call(calc_mc_css,
# we use purrr::compact to drop NULL values from arguments list:
                        args=purrr::compact(c(list(
                          chem.name=chem.name,
                          chem.cas=chem.cas,
                          dtxsid=dtxsid,
                          parameters=parameters,
                          model=model,
                          which.quantile=which.quantile,
                          species=species,
                          output.units=input.units,
                         # suppress.messages=TRUE,
                          tissue=tissue,
                          concentration=concentration,
                          calc.analytic.css.arg.list=calc.analytic.css.arg.list,
                          Caco2.options = Caco2.options,
                          return.samples=return.samples,
                          ...)))))
                         
  if (is(Css,"try-error"))
  {
    return(NA)
  }
  
  #
  # The super-impressive and complimacated reverse dosimetry IVIVE calculation: 
  #
  # uM to mg/kg/day:
  #
  # output units here are in 'mg/kg/day' for 'dose'
  # either 'uM / uM/mg/kg/day' or 'mg/L / (mg/L)/kg/day'
  dose <- conc/Css  # conc (input.units) / Css (input.units/kg/day)

  if (tolower(tmp.output.units) == 'umol')
  {
    if (is.null(chem.cas)){
      chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
    } 

    MW <- get_physchem_param("MW",chem.cas=chem.cas)
    # output units are in 'umol/kg/day' for 'dose'
    dose <- dose * convert_units(
      input.units = 'mg',
      output.units = tmp.output.units,
      chem.cas = chem.cas,
      chem.name = chem.name,
      dtxsid = dtxsid
    )
  } else if (tolower(tmp.output.units) != 'mg') 
    stop("Output units can only be in mgpkgpday or umolpkgpday.")
  
  if (!suppress.messages & !return.samples)
  {
    cat(input.units,
      "concentration converted to",
      output.units,
      "dose for",
      which.quantile,
      "quantile.\n")
  }

  return(set_httk_precision(dose))
}
