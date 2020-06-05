#' Parameterize_fetal_PBTK
#' 
#' This function initializes the parameters needed in the functions
#' solve_fetal_pbtk by calling solve_pbtk and adding additional parameters.
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param ... Arguments passed to parameterize_pbtk.
#' @return \item{pre_pregnant_BW}{Body Weight before pregnancy, kg.}
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} \item{Fgutabs}{Fraction
#' of the oral dose absorbed, i.e. the fraction of the dose that enters the
#' gutlumen.} \item{Funbound.plasma}{Fraction of plasma that is not bound.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)} \item{hematocrit}{Percent
#' volume of red blood cells in the blood.} \item{Kgut2pu}{Ratio of
#' concentration of chemical in gut tissue to unbound concentration in plasma.}
#' \item{kgutabs}{Rate that chemical enters the gut from gutlumen, 1/h.}
#' \item{Kkidney2pu}{Ratio of concentration of chemical in kidney tissue to
#' unbound concentration in plasma.} \item{Kliver2pu}{Ratio of concentration of
#' chemical in liver tissue to unbound concentration in plasma.}
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue to unbound
#' concentration in plasma.} \item{Krbc2pu}{Ratio of concentration of chemical
#' in red blood cells to unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} \item{million.cells.per.gliver}{Millions
#' cells per gram of liver tissue.} \item{MW}{Molecular Weight, g/mol.}
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid
#' filtered from kidney and excreted.} \item{Rblood2plasma}{The ratio of the
#' concentration of the chemical in the blood to the concentration in the
#' plasma from available_rblood2plasma.} \item{Vgutc}{Volume of the gut per kg body
#' weight, L/kg BW.} \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg
#' BW.} \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{Vthyroidc}{Volume of the thyroid per kg body weight, L/kg BW.}
#' \item{Kfgut2pu}{Ratio of concentration of chemical in fetal gut tissue to
#' unbound concentration in plasma.} \item{Kfkidney2pu}{Ratio of concentration
#' of chemical in fetal kidney tissue to unbound concentration in plasma.}
#' \item{Kfliver2pu}{Ratio of concentration of chemical in fetal liver tissue
#' to unbound concentration in plasma.} \item{Kflung2pu}{Ratio of concentration
#' of chemical in fetal lung tissue to unbound concentration in plasma.}
#' \item{Kfrest2pu}{Ratio of concentration of chemical in fetal rest of body
#' tissue to unbound concentration in plasma.} \item{Kfbrain2pu}{Ratio of
#' concentration of chemical in fetal brain tissue to unbound concentration in
#' plasma.} \item{Kthyroid2pu}{Ratio of concentration of chemical in fetal
#' thyroid tissue to unbound concentration in plasma.}
#' \item{Kfthyroid2pu}{Ratio of concentration of chemical in fetal thyroid
#' tissue to unbound concentration in plasma.} \item{Kplacenta2pu}{Ratio of
#' concentration of chemical in placental tissue to unbound concentration in
#' maternal plasma.} \item{Kfplacenta2pu}{Ratio of concentration of chemical in
#' placental tissue to unbound concentration in fetal plasma.} 
#' @author John Wambaugh, Robert Pearce, and Mark Sfeir
#' @references Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#' @keywords Parameter
#' @examples
#' 
#' 
#'  parameters <- parameterize_fetal_pbtk(chem.cas='80-05-7')
#' 
#'  parameters <- parameterize_fetal_pbtk(chem.name='Bisphenol-A',species='Rat')
#' 
#'  
#' 
#' @export parameterize_fetal_pbtk
parameterize_fetal_pbtk<- function(chem.cas=NULL,
                              chem.name=NULL,
                              dtxsid = NULL,
                              species="Human",
                              ...)
{
  #Call parameterize_pbtk function with brain compartment specified to obtain
  #maternal brain partitioning coefficient, to be equated to fetal Kfbrain2pu.
  parms <- parameterize_pbtk(chem.cas=chem.cas,
                            chem.name=chem.name,
                            dtxsid=dtxsid,
                            species=species,
                            tissuelist=list(liver=c("liver"),
                            kidney=c("kidney"),lung=c("lung"),
                            gut=c("gut"),adipose = c("adipose"),
                            brain = c("brain")),
                            placenta=TRUE,
                            ...)
# parms[['Vrestc']] <- parms[['Vrestc']] + parms[['Vvenc']] + parms[['Vartc']]
  
  #Store Kbrain2pu and Vbrainc values in intermediate variables
  Kbrain2pu <- parms$Kbrain2pu
  
  #Run parameterize pbtk function again, this time with brain tacitly lumped
  parms <- parameterize_pbtk(chem.cas=chem.cas,
                             chem.name=chem.name,
                             dtxsid=dtxsid,
                             species=species,
                             tissuelist=list(liver=c("liver"),
                                             kidney=c("kidney"),lung=c("lung"),
                                             gut=c("gut"),adipose = c("adipose")),
                             placenta=TRUE,
                             ...)
  parms$Kthyroid2pu <-  parms$Kfthyroid2pu <- 1
  parms$Kfliver2pu <- parms$Kliver2pu
  parms$Kfkidney2pu <- parms$Kkidney2pu
  parms$Kfrest2pu <- parms$Krest2pu
  parms$Kfgut2pu <- parms$Kgut2pu
  parms$Kflung2pu <- parms$Klung2pu
  parms$Kfbrain2pu <- Kbrain2pu
  
  #Key ICRP 2002 data for females, corresponding to reference BW of 60 kg:
  ICRP_2002_female_tissue_mass_fractions_data <- 10^-2 * c(
                                                   Vthyroidc <- 0.0283,
                                                   Vkidneyc <- 0.458,
                                                   Vgutc <- 1.90,
                                                   Vliverc <- 2.33,
                                                   Vlungc <- 1.58,
                                                   Vbrainc <- 2.17 
                                                   #^^though brain not in model
                                                   )
  
  
 parms$pre_pregnant_BW <- 61.103 #kg
  #Override parameterize_pbtk's body weight listing with average prepregnant
  #case, as scale dosing requires an entry named 'BW'
  parms$BW <- parms$pre_pregnant_BW 
 parms$Vthyroidc <- ICRP_2002_female_tissue_mass_fractions_data[['Vthyroidc']]
 parms$Vkidneyc <- ICRP_2002_female_tissue_mass_fractions_data[['Vkidneyc']]
 parms$Vgutc <- ICRP_2002_female_tissue_mass_fractions_data[['Vgutc']]
 parms$Vliverc <- ICRP_2002_female_tissue_mass_fractions_data[['Vliverc']]
 parms$Vlungc <- ICRP_2002_female_tissue_mass_fractions_data[['Vlungc']]
 
 #Remove parameters from parameterize_pbtk that aren't used in the gestational model
 parms$Vrestc <- parms$Qadiposef <- parms$Qcardiacc <- parms$Qkidneyf <- NULL 
 parms$Qbrainf <- parms$Qlungf <- parms$Qliverf <- parms$Qgutf <- NULL
 parms$Vbrainc <- parms$Kbrain2pu <- parms$Qgfrc <- parms$Vadiposec <- NULL
 parms$Qrestf <- NULL  
 
 
 return(parms)                             
}
