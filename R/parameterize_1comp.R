#' Parameterize_1comp
#' 
#' This function initializes the parameters needed in the function solve_1comp.
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
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
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' @return \item{Vdist}{Volume of distribution, units of L/kg BW.}
#' \item{Fgutabs}{Fraction of the oral dose absorbed, i.e. the fraction of the
#' dose that enters the gutlumen.} \item{kelim}{Elimination rate, units of
#' 1/h.} \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{kgutabs}{Rate chemical is absorbed, 1/h.}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
#' \item{MW}{Molecular Weight, g/mol.} \item{Rblood2plasma}{The ratio of the
#' concentration of the chemical in the blood to the concentration in the
#' plasma. Not used in calculations but included for the conversion of plasma
#' outputs.} \item{hepatic.bioavailability}{Fraction of dose remaining after
#' first pass clearance, calculated from the corrected well-stirred model.}
#' \item{BW}{Body Weight, kg.} 
#' @author John Wambaugh
#' @keywords Parameter
#' @examples
#' 
#'  parameters <- parameterize_1comp(chem.name='Bisphenol-A',species='Rat')
#'  parameters <- parameterize_1comp(chem.cas='80-05-7',restrictive.clearance=FALSE,
#'                                   species='rabbit',default.to.human=TRUE)
#'  out <- solve_1comp(parameters=parameters)
#' 
#' @export parameterize_1comp
parameterize_1comp <- function(chem.cas=NULL,
                               chem.name=NULL,
                               species='Human',
                               default.to.human=F,
                               adjusted.Funbound.plasma=T,
                               regression=T,
                               restrictive.clearance=T,
                               well.stirred.correction=T,
                               suppress.messages=F,
                               clint.pvalue.threshold=0.05)
{
  physiology.data <- physiology.data
  if(is.null(chem.cas) & is.null(chem.name)) stop('Must specify chem.name or chem.cas')
  params <- list()
  
  params[['Vdist']] <- calc_vdist(chem.cas=chem.cas,
                                  chem.name=chem.name,
                                  species=species,
                                  default.to.human=default.to.human,
                                  adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                  regression=regression,suppress.messages=T)
  
  ss.params <- suppressWarnings(parameterize_steadystate(chem.name=chem.name,
                                                         chem.cas=chem.cas,
                                                         species=species,
                                                         default.to.human=default.to.human,
                                                         adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                                         restrictive.clearance=restrictive.clearance,
                                                         clint.pvalue.threshold=clint.pvalue.threshold))
  ss.params <- c(ss.params, params['Vdist'])
  
  params[['kelim']] <- calc_elimination_rate(parameters=ss.params,
                                             chem.cas=chem.cas,
                                             chem.name=chem.name,
                                             species=species,
                                             suppress.messages=T,
                                             default.to.human=default.to.human,
                                             adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                             regression=regression,
                                             restrictive.clearance=restrictive.clearance,
                                             well.stirred.correction=well.stirred.correction,
                                             clint.pvalue.threshold=clint.pvalue.threshold)
  
  params[["Clint"]] <- ss.params[["Clint"]]
  params[["Clint.dist"]] <- ss.params[["Clint.dist"]]
  params[["Funbound.plasma"]] <- ss.params[["Funbound.plasma"]] 
  params[["Funbound.plasma.dist"]] <- ss.params[["Funbound.plasma.dist"]] 
  params[["Funbound.plasma.adjustment"]] <- ss.params[["Funbound.plasma.adjustment"]] 
  params[["Fhep.assay.correction"]] <- ss.params[["Fhep.assay.correction"]]
  params[["Funbound.plasma.dist"]] <- ss.params[["Funbound.plasma.dist"]] 
  phys.params <-  suppressWarnings(parameterize_schmitt(chem.name=chem.name,
                                                         chem.cas=chem.cas,
                                                         species=species,
                                                         default.to.human=default.to.human)) 
  params[["Pow"]] <- phys.params[["Pow"]]
  params[["pKa_Donor"]] <- phys.params[["pKa_Donor"]] 
  params[["pKa_Accept"]] <- phys.params[["pKa_Accept"]]
  params[["MA"]] <- phys.params[["MA"]]

  params[['kgutabs']] <- 2.18
  
  params[['Rblood2plasma']] <- available_rblood2plasma(chem.cas=chem.cas,chem.name=chem.name,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
  
  params[['million.cells.per.gliver']] <- 110
  params[["liver.density"]] <- 1.05 # g/mL
   
   # Check the species argument for capitilization problems and whether or not it is in the table:  
    if (!(species %in% colnames(physiology.data)))
    {
      if (toupper(species) %in% toupper(colnames(physiology.data)))
      {
        phys.species <- colnames(physiology.data)[toupper(colnames(physiology.data))==toupper(species)]
      } else stop(paste("Physiological PK data for",species,"not found."))
    } else phys.species <- species
  
  # Load the physiological parameters for this species
    this.phys.data <- physiology.data[,phys.species]
    names(this.phys.data) <- physiology.data[,1]
    
    params[['hematocrit']] <- this.phys.data[["Hematocrit"]]
  
  if(is.null(chem.cas)) chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
  params[['MW']] <- get_physchem_param("MW",chem.CAS=chem.cas)
  
    Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.CAS=chem.cas),silent=T)
    if (class(Fgutabs) == "try-error") Fgutabs <- 1
    
    params[['Fgutabs']] <- Fgutabs
  
    params[['hepatic.bioavailability']] <- ss.params[['hepatic.bioavailability']]  
    
    params[['BW']] <- this.phys.data[["Average BW"]]
  
  return(params)
}
