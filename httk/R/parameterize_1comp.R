#' Parameterize_1comp
#' 
#' This function initializes the parameters needed in the function solve_1comp.
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param default.to.human Substitutes missing rat values with human values if
#' true.
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with volume of distribution calculated with this value.
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients in volume of distribution calculation.
#' @param restrictive.clearance In calculating elimination rate and hepatic
#' bioavailability, protein binding is not taken into account (set to 1) in
#' liver clearance if FALSE.
#' @param well.stirred.correction Uses correction in calculation of hepatic
#' clearance for well-stirred model if TRUE.  This assumes clearance relative
#' to amount unbound in whole blood instead of plasma, but converted to use
#' with plasma concentration.
#' @param suppress.messages Whether or not to suppress messages.
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-value greater than the threshold are
#' set to zero.
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#'
#' @return 
#' \item{Vdist}{Volume of distribution, units of L/kg BW.}
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the
#' dose that enters the gutlumen.} 
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte 
#' assay using the method of Kilford et al. (2008)} 
#' \item{kelim}{Elimination rate, units of 1/h.} 
#' \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{kgutabs}{Rate chemical is absorbed, 1/h.}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
#' \item{MW}{Molecular Weight, g/mol.} 
#' \item{Rblood2plasma}{The ratio of the concentration of the chemical in the 
#' blood to the concentration in the plasma. Not used in calculations but 
#' included for the conversion of plasma outputs.} 
#' \item{hepatic.bioavailability}{Fraction of dose remaining after
#' first pass clearance, calculated from the corrected well-stirred model.}
#' \item{BW}{Body Weight, kg.} 
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @references Pearce, Robert G., et al. "Httk: R package for high-throughput 
#' toxicokinetics." Journal of statistical software 79.4 (2017): 1.
#'
#' Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#'
#' @keywords Parameter 1compartment
#' @examples
#' 
#'  parameters <- parameterize_1comp(chem.name='Bisphenol-A',species='Rat')
#'  parameters <- parameterize_1comp(chem.cas='80-05-7',
#'                                   restrictive.clearance=FALSE,
#'                                   species='rabbit',
#'                                   default.to.human=TRUE)
#'  out <- solve_1comp(parameters=parameters)
#'
#' @export parameterize_1comp
parameterize_1comp <- function(
                        chem.cas=NULL,
                        chem.name=NULL,
                        dtxsid = NULL,
                        species='Human',
                        default.to.human=FALSE,
                        adjusted.Funbound.plasma=TRUE,
                        regression=TRUE,
                        restrictive.clearance=TRUE,
                        well.stirred.correction=TRUE,
                        suppress.messages=FALSE,
                        clint.pvalue.threshold=0.05,
                        minimum.Funbound.plasma=0.0001)
{
#R CMD CHECK throws notes about "no visible binding for global variable", for
#each time a data.table column name is used without quotes. To appease R CMD
#CHECK, a variable has to be created for each of these column names and set to
#NULL. Note that within the data.table, these variables will not be NULL! Yes,
#this is pointless and annoying.
  physiology.data <- physiology.data
#End R CMD CHECK appeasement.  
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')

# Look up the chemical name/CAS, depending on what was provide:
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
     
  params <- list()
  params[['Vdist']] <- calc_vdist(
                         chem.cas=chem.cas,
                         chem.name=chem.name,
                         dtxsid=dtxsid,
                         species=species,
                         default.to.human=default.to.human,
                         adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                         regression=regression,
                         suppress.messages=FALSE)
  
  ss.params <- suppressWarnings(parameterize_steadystate(
                                  chem.name=chem.name,
                                  chem.cas=chem.cas,
                                  dtxsid=dtxsid,
                                  species=species,
                                  default.to.human=default.to.human,
                                  adjusted.Funbound.plasma=
                                    adjusted.Funbound.plasma,
                                  restrictive.clearance=restrictive.clearance,
                                  clint.pvalue.threshold=clint.pvalue.threshold,
                                  minimum.Funbound.plasma=
                                    minimum.Funbound.plasma))
  ss.params <- c(ss.params, params['Vdist'])
  
  params[['kelim']] <- calc_elimination_rate(parameters=ss.params,
                         chem.cas=chem.cas,
                         chem.name=chem.name,
                         dtxsid=dtxsid,
                         species=species,
                         suppress.messages=TRUE,
                         default.to.human=default.to.human,
                         adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                         regression=regression,
                         restrictive.clearance=restrictive.clearance,
                         well.stirred.correction=well.stirred.correction,
                         clint.pvalue.threshold=clint.pvalue.threshold,
                         minimum.Funbound.plasma=minimum.Funbound.plasma)
  
  params[["Clint"]] <- ss.params[["Clint"]]
  params[["Clint.dist"]] <- ss.params[["Clint.dist"]]
  params[["Funbound.plasma"]] <- ss.params[["Funbound.plasma"]] 
  params[["Funbound.plasma.dist"]] <- ss.params[["Funbound.plasma.dist"]] 
  params[["Funbound.plasma.adjustment"]] <- 
    ss.params[["Funbound.plasma.adjustment"]] 
  params[["Fhep.assay.correction"]] <- ss.params[["Fhep.assay.correction"]]
  params[["Funbound.plasma.dist"]] <- ss.params[["Funbound.plasma.dist"]] 
  phys.params <-  suppressWarnings(parameterize_schmitt(chem.name=chem.name,
                    chem.cas=chem.cas,
                    species=species,
                    default.to.human=default.to.human,
                    minimum.Funbound.plasma=minimum.Funbound.plasma)) 
  params[["Pow"]] <- phys.params[["Pow"]]
  params[["pKa_Donor"]] <- phys.params[["pKa_Donor"]] 
  params[["pKa_Accept"]] <- phys.params[["pKa_Accept"]]
  params[["MA"]] <- phys.params[["MA"]]

  params[['kgutabs']] <- 2.18
  
  params[['Rblood2plasma']] <- 
    available_rblood2plasma(chem.cas=chem.cas,chem.name=chem.name,
        species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
  
  params[['million.cells.per.gliver']] <- 110
  params[["liver.density"]] <- 1.05 # g/mL
   
# Check the species argument for capitalization problems and whether or not 
# it is in the table:  
    if (!(species %in% colnames(physiology.data)))
    {
      if (toupper(species) %in% toupper(colnames(physiology.data)))
      {
        phys.species <- colnames(physiology.data)[
                          toupper(colnames(physiology.data))==toupper(species)]
      } else stop(paste("Physiological PK data for",species,"not found."))
    } else phys.species <- species
  
  # Load the physiological parameters for this species
    this.phys.data <- physiology.data[,phys.species]
    names(this.phys.data) <- physiology.data[,1]
    
    params[['hematocrit']] <- this.phys.data[["Hematocrit"]]
    params[['MW']] <- get_physchem_param("MW",chem.cas=chem.cas)
  
    Fgutabs <- try(
                 get_invitroPK_param(
                   "Fgutabs",
                   species,
                   chem.cas=chem.cas),
                 silent=TRUE)

    if (class(Fgutabs) == "try-error") Fgutabs <- 1
    
    params[['Fgutabs']] <- Fgutabs
    params[['hepatic.bioavailability']] <- 
      ss.params[['hepatic.bioavailability']]  
    params[['BW']] <- this.phys.data[["Average BW"]]
  
  return(lapply(params,set_httk_precision))
}
