calc_mc_oral_equiv <- function(conc,
                               chem.name=NULL,
                               chem.cas=NULL,
                               which.quantile=0.95,
                               species="Human",
                               input.units='uM',
                               output.units='mg',
                               suppress.messages=F,
                               return.samples=F,
                               restrictive.clearance=T,
                               plasma.binding=F,
                               tk.statistic.used="mean",
                               tissue=NULL,
                               IVIVE=NULL,
                               ...)
{
  if(!(tolower(input.units) %in% c('um','mg/l'))) stop("Input units can only be uM or mg/L.")
  
  if (!(tk.statistic.used %in% c("mean","max"))) stop ("tk.statistic.used for IVIVE must be either \"mean\" or \"max\"imum concentrtation.")
  
  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    plasma.binding <- out[["plasma.binding"]]
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
  }
  
  Css <- try(calc_mc_css(daily.dose=1,
                         chem.name=chem.name,
                         chem.cas=chem.cas,
                         which.quantile=which.quantile,
                         species=species,
                         output.units=input.units,
                         suppress.messages=T,
                         restrictive.clearance=restrictive.clearance,
                         tissue=tissue,
                         tk.statistic.used=tk.statistic.used,
                         return.samples=return.samples,
                         ...))
                         
  dose <- conc/Css  
  
  # Do we use the free concentration in the plasma or the total?
  if(plasma.binding) 
  {
    params <- parameterize_steadystate(chem.name=chem.name,chem.cas=chem.cas,species=species)
    dose <- dose/params[["Funbound.plasma"]]
  } 
  if(tolower(output.units) == 'mol'){
    if(is.null(chem.cas)) chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
    MW <- get_physchem_param("MW",chem.CAS=chem.cas)
    dose <- dose /1000 / MW * 1000000 
  }else if(tolower(output.units) != 'mg') stop("Output units can only be in mg or mol.")
  if(!suppress.messages & !return.samples){
    cat(input.units,"concentration converted to",output.units,"/kg bw/day dose for",which.quantile,"quantile.\n")
  }
	if (class(Css) == "try-error"){
    return(NA)
  }else{
    return(dose)
  }
  
}