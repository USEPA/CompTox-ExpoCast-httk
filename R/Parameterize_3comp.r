parameterize_3comp<- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              default.to.human=F,
                              force.human.clint.fub = F,
                              clint.pvalue.threshold=0.05,
                              adjusted.Funbound.plasma=T,
                              regression=T,
                              suppress.messages=F)
{
  parms <- parameterize_pbtk(chem.cas=chem.cas,
                              chem.name=chem.name,
                              species=species,
                              default.to.human=default.to.human,
                              tissuelist=list(liver=c("liver"),gut=c("gut")),
                              force.human.clint.fub = force.human.clint.fub,
                              clint.pvalue.threshold=clint.pvalue.threshold,
                              adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                              regression=regression,
                              suppress.messages=suppress.messages)
                              
parms$Qkidneyf  <- parms$Vvenc <- parms$Vartc <- NULL
 
 return(parms)                             
}
