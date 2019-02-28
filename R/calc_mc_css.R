# The function uses Monte Carlo methods to vary parameters and can return various quantiles.
# Original by John Wambaugh
# Rewritten by Caroline Ring with modifications by John Wambaugh and Robert Pearce
calc_mc_css <- function(chem.cas=NULL,
                        chem.name=NULL,
                        parameters=NULL,
                        daily.dose=1,
                        which.quantile=0.95,
                        species="Human",
                        output.units="mg/L",
                        suppress.messages=F,
                        model='3compartmentss',
                        censored.params=list(Funbound.plasma=list(cv=0.3,lod=0.01)),
                        vary.params=list(BW=0.3,Vliverc=0.3,Qgfrc=0.3,Qtotal.liverc=0.3,million.cells.per.gliver=0.3,Clint=0.3),
                        fup.meas.cv=0.4,
                        clint.meas.cv=0.3,
                        fup.pop.cv=0.3,
                        clint.pop.cv=0.3,
                        samples=1000,
                        return.samples=F,
                        default.to.human=F,
                        tissue=NULL,
                        well.stirred.correction=T,
                        adjusted.Funbound.plasma=T,
                        regression=T,
                        clint.pvalue.threshold=0.05,
                        restrictive.clearance=T,
                        tk.statistic.used="mean",
                        IVIVE=NULL,
                        httkpop=T,
                        poormetab=T,
                        fup.censored.dist=FALSE,
                        fup.lod=0.01,
                        method='direct resampling',
                        gendernum=NULL,
                        agelim_years=NULL,
                        agelim_months=NULL,
                        weight_category =  c("Underweight", "Normal", "Overweight", "Obese"),
                        gfr_category = c("Normal", "Kidney Disease", "Kidney Failure"),
                        reths = c("Mexican American", "Other Hispanic", "Non-Hispanic White","Non-Hispanic Black", "Other"),
                        physiology.matrix=NULL,
                        parameter.matrix=NULL,
                        ...)
{
  if (!(model %in% c("pbtk","1compartment","3compartment","3compartmentss"))) stop("Model must be either \"pbtk\", \"1compartment\", \"3compartmentss\", or \"3compartment\".")

  if (!(tk.statistic.used %in% c("mean","max"))) stop ("tk.statistic.used for IVIVE must be either \"mean\" or \"max\"imum concentrtation.")

  if (!is.null(IVIVE)) 
  {
    out <- honda.ivive(method=IVIVE,tissue=tissue)
    restrictive.clearance <- out[["restrictive.clearance"]]
    tissue <- out[["tissue"]]
  }
 
  css_apply <- function(params)
  {
    params <- as.list(params)
    css <- calc_analytic_css(parameters=params,
                             model=model,
                             daily.dose=daily.dose,
                             suppress.messages=T,
                             output.units=output.units,
                             tissue=tissue,
                             chem.cas=chem.cas,
                             chem.name=chem.name,
                             well.stirred.correction=well.stirred.correction,
                             adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                             regression=regression,
                             restrictive.clearance=restrictive.clearance,
                             clint.pvalue.threshold=0.05,
                             ...)
    return(css)
  }
  if (httkpop == T & tolower(species) == 'human')
  {
    if (!is.null(parameter.matrix)) css.list <- apply(parameter.matrix,1,css_apply)
    else
    {
      if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
      if (is.null(parameters))
      {
        out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
        this.chem <- out$chem.cas
      } else this.chem <- NULL
      if (is.null(physiology.matrix))
      {
        nsamp <- samples
        if(is.null(method)) stop('Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct resampling\" (\"dr\" or \"d\").')
        else if(! method %in% c('direct resampling','virtual individuals','v','vi','direct resampling','dr','d')) stop('Specify method as \"virtual individuals\" (\"v\" or \"vi\") or \"direct resampling\" (\"dr\" or \"d\").')
        physiology.matrix <- httkpop_generate(method=method,
                                              nsamp=nsamp,
                                              gendernum=gendernum,
                                              agelim_years=agelim_years,
                                              agelim_months=agelim_months,
                                              weight_category=weight_category,
                                              gfr_category=gfr_category,
                                              reths=reths)
      } 
      parameter.matrix <- get_httk_params(physiology.matrix,
                                          model=model,
                                          chemcas=this.chem,
                                          parameters=parameters,
                                          poormetab=poormetab,
                                          fup.meas.cv=fup.meas.cv,
                                          clint.meas.cv=clint.meas.cv,
                                          fup.pop.cv=fup.pop.cv,
                                          clint.pop.cv=clint.pop.cv,
                                          fup.lod=fup.lod,
                                          fup.censored.dist=fup.censored.dist,
                                          well.stirred.correction=well.stirred.correction,
                                          adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                          regression=regression,
                                          restrictive.clearance=restrictive.clearance,
                                          clint.pvalue.threshold=clint.pvalue.threshold)
      css.list <- apply(parameter.matrix,1,css_apply) 
    }
    if (return.samples) out <- css.list
    else out <- quantile(css.list,which.quantile,na.rm=T)       
  } else {
    if (is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
    if (is.null(parameters))
    {
        parameters <- parameterize_steadystate(chem.cas=chem.cas,
                                               chem.name=chem.name,
                                               species=species,
                                               default.to.human=default.to.human,
                                               adjusted.Funbound.plasma=adjusted.Funbound.plasma)
    } else {
      if (!all(param.names.3compss %in% names(parameters)))
      {
        stop(paste("Missing parameters:",
                   paste(param.names.3compss[which(!param.names.3compss %in% names(parameters))],
                     collapse=', '),
                   ".  Use parameters from parameterize_steadystate."))
      }
    }
    if (well.stirred.correction & !'Rblood2plasma' %in% names(parameters)) parameters[['Rblood2plasma']] <- available_rblood2plasma(chem.name=chem.name,chem.cas=chem.cas,species=species,adjusted.Funbound.plasma=adjusted.Funbound.plasma)
    
    out <- monte_carlo(params=parameters,
                        censored.params=censored.params,
                        which.quantile=which.quantile,
                        cv.params=vary.params,
                        samples=samples,model='3compartmentss',
                        daily.dose=daily.dose,
                        output.units=output.units,
                        tissue=tissue,
                        IVIVE=IVIVE,
                        chem.name=chem.name,
                        chem.cas=chem.cas,
                        adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                        regression=regression,
                        well.stirred.correction=well.stirred.correction,
                        suppress.messages=T,
                        return.samples=return.samples,
                        restrictive.clearance=restrictive.clearance,
                        species=species)
    if(httkpop==T) warning('httkpop model only available for human and thus not used.  Set species=\"Human\" to run httkpop model.')   
  }  
  if(!suppress.messages & !return.samples){
    if(is.null(chem.cas) & is.null(chem.name)){
      if(is.null(tissue)) cat("Plasma concentration returned in",output.units,"units.\n")
      else cat(paste(toupper(substr(tissue,1,1)),substr(tissue,2,nchar(species)),sep=''),"concentration returned in",output.units,"units.\n")
    }else{
      if(is.null(tissue))cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"plasma concentration returned in",output.units,"units for",which.quantile,"quantile.\n") 
      else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),tissue,"concentration returned in",output.units,"units.\n")
    }
  }
  return(as.numeric(out))
}
