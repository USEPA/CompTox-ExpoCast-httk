parameterize_fetal_pbtk<- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              ...)
{
  parms <- parameterize_pbtk(chem.cas=chem.cas,
                              chem.name=chem.name,
                              species=species,
                              tissuelist=list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut"),brain=c("brain")),placenta=T,
                              ...)
# parms[['Vrestc']] <- parms[['Vrestc']] + parms[['Vvenc']] + parms[['Vartc']]
 
  parms$Kthyroid2pu <-  parms$Kfthyroid2pu <- 1
  parms$Kfliver2pu <- parms$Kliver2pu
  parms$Kfkidney2pu <- parms$Kkidney2pu
  parms$Kfrest2pu <- parms$Krest2pu
  parms$Kfgut2pu <- parms$Kgut2pu
  parms$Kflung2pu <- parms$Klung2pu
  parms$Kfbrain2pu <- parms$Kbrain2pu
  parms$Krest2pu <- (parms$Krest2pu * parms$Vrestc + parms$Kbrain2pu * parms$Vbrainc) / ( parms$Vrestc  + parms$Vbrainc)
  parms$pre_pregnant_BW <- 60 
 parms$Vthyroidc <- 0.017/60
 parms$Vkidneyc <- 0.275/60
 parms$Vgutc <- 1.14/60
 parms$Vliverc <- 1.4/60
 parms$Vlungc <- 0.95/60 
 parms$Vartc <- 0.624/60           
 parms$Vvenc <- 2.32/60
 parms$Vfgutc <- 0.0178
parms$Vrestc <- parms$BW <- parms$Qcardiacc <- parms$Qgutf <- parms$Qbrainf <- parms$Qlungf <- parms$Qliverf <- parms$Qkidneyf <- parms$Vbrainc <- parms$Kbrain2pu <- NULL  
#parms$fBW <- 0.00003107 * exp(0.8137/0.06458 * (1 - exp(-0.06458 * day / 7)))  
#parms$Vplacenta <- 0.317 * parms$fBW^0.582  
 return(parms)                             
}
