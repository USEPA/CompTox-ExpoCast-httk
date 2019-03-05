# This function calculats an elimination rate for a one compartment model where 
# eelimination is entirely due to metablism by the liver and glomerular filtration
# in the kidneys.
calc_total_clearance<- function(chem.cas=NULL,
                                chem.name=NULL,
                                parameters=NULL,
                                species="Human",
                                suppress.messages=F,
                                default.to.human=F,
                                well.stirred.correction=T,
                                restrictive.clearance=T,
                                adjusted.Funbound.plasma=T,
                                ...)
{
    if(is.null(parameters)) 
    {
      parameters <- parameterize_steadystate(chem.cas=chem.cas, 
                                             chem.name=chem.name, 
                                             species=species,
                                             default.to.human=default.to.human,
                                             adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                             ...)
    }
    Qgfrc <- get_param("Qgfrc",parameters,"calc_Css") / parameters[['BW']]^0.25 #L/h/kgBW
    fup <- parameters[["Funbound.plasma"]]# unitless fraction
    if (nchar(fup) - nchar(gsub(",","",fup))==2) 
    {
      fup <- as.numeric(strsplit(fup,",")[[1]][1])
    }
     
    clearance <- Qgfrc*fup+
                   calc_hepatic_clearance(chem.cas=chem.cas,
                     chem.name=chem.name,
                     species=species,
                     parameters=parameters,
                     suppress.messages=T,
                     well.stirred.correction=well.stirred.correction,
                     restrictive.clearance=restrictive.clearance,
                     adjusted.Funbound.plasma=adjusted.Funbound.plasma) #L/h/kgBW
    if (!suppress.messages)
    {
      cat(paste(toupper(substr(species,1,1)),
        substr(species,2,nchar(species)),sep=''),
        "total clearance returned in units of L/h/kg BW.\n")
    }
    
    return(as.numeric(clearance))
}