calc_gas_equiv <- function(conc,chem.name=NULL,chem.cas=NULL,days=20,exposure.type='ambient',aerosol.type='smoke',inhalation.exposure='gas',dae=NULL,particle.density=NULL,...){
  if(tolower(exposure.type) == 'ambient') exposure <- 1
  else if(tolower(exposure.type) == 'occupational') exposure <- 1/3
  if(tolower(inhalation.exposure) %in% c('aerosol','mixture') & is.null(particle.density) & is.null(dae)){
    aerosol <- subset(aerosol.data,aerosol==aerosol.type)
    out <- solve_pbtk(chem.name=chem.name,chem.cas=chem.cas,days=days,inhalation=T,inhalation.exposure=inhalation.exposure,dae=aerosol[,'dae'],particle.density=aerosol[,'particle.density'],...,suppress.messages=T)
  }else out <- solve_pbtk(chem.name=chem.name,chem.cas=chem.cas,days=days,inhalation=T,inhalation.exposure=inhalation.exposure,dae=dae,particle.density=particle.density,...,suppress.messages=T)
  css <- out[[dim(out)[1],'AUC']] - out[[match(days - 1,out[,'time']),'AUC']]
  dose <- conc/css
  
  return(dose)
}