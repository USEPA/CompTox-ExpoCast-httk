# This function calculats an elimination rate for a one compartment model where 
# eelimination is entirely due to metablism by the liver and glomerular filtration
# in the kidneys.
calc_elimination_rate <- function(chem.cas=NULL,
                                  chem.name=NULL,
                                  parameters=NULL,
                                  species="Human",
                                  suppress.messages=F,
                                  default.to.human=F,
                                  restrictive.clearance=T,
                                  adjusted.Funbound.plasma=T,
                                  regression=T,
                                  well.stirred.correction=T,
                                  clint.pvalue.threshold=0.05)
{
  name.list <- c("Clint",
                 "Funbound.plasma",
                 "Qtotal.liverc",
                 "million.cells.per.gliver",
                 "Vliverc",
                 "BW",
                 "liver.density",
                 'Fhep.assay.correction')

# This function likes to have the blood flow to the liver per kg bw^0.75 in a 
# variable named Qtotal.liverc:
  if (!is.null(parameters))
  {
    if (all(c("Qcardiacc","Qgutf","Qliverf")%in%names(parameters)))
    {
      parameters[["Qtotal.liverc"]] <- parameters[["Qcardiacc"]]*(parameters[["Qgutf"]]+parameters[["Qliverf"]])
    }
  }
  
  if(is.null(parameters))
  {
    parameters <- parameterize_steadystate(chem.cas=chem.cas,
                                           chem.name=chem.name,
                                           species=species,
                                           default.to.human=default.to.human,
                                           adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                           clint.pvalue.threshold=clint.pvalue.threshold)
    Vd <- calc_vdist(chem.cas=chem.cas,
                     chem.name=chem.name,
                     species=species,
                     suppress.messages=T,
                     default.to.human=default.to.human,
                     adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                     regression=regression) 
  } else { 
    if(!all(name.list %in% names(parameters)))
    {
      if(is.null(chem.cas) & is.null(chem.name))
      { 
        stop('chem.cas or chem.name must be specified when not including all 3compartment or pbtk parameters.')
      }
      params <- parameterize_steadystate(chem.cas=chem.cas,
                                         chem.name=chem.name,
                                         species=species,
                                         default.to.human=default.to.human,
                                         adjusted.Funbound.plasma=adjusted.Funbound.plasma)
      parameters <- c(parameters,params[name.list[!(name.list %in% names(parameters))]])
    }
    if('Vdist' %in% names(parameters)){
      Vd <- parameters[['Vdist']]
    }else{
#        if(is.null(chem.name) & is.null(chem.cas))stop('chem.cas or chem.name must be specified when Vdist is not included in parameters.')
      Vd <- calc_vdist(chem.cas=chem.cas,
                       chem.name=chem.name,
                       parameters=parameters,
                       species=species,
                       suppress.messages=T,
                       default.to.human=default.to.human,
                       adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                       regression=regression) 
    }    
  } 
  clearance <- calc_total_clearance(chem.name=chem.name,
                                    chem.cas=chem.cas,
                                    species=species,
                                    parameters=parameters,
                                    suppress.messages=T,
                                    default.to.human=default.to.human,
                                    well.stirred.correction=well.stirred.correction,
                                    restrictive.clearance=restrictive.clearance,
                                    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                    clint.pvalue.threshold=clint.pvalue.threshold) #L/h/kgBW

  if(!suppress.messages)cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"elimination rate returned in units of 1/h.\n")
  return(as.numeric(clearance/Vd))
}