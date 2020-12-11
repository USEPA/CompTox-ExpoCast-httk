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
#' default "Human"). Currently only a narrow human model is supported. 
#' @param fetal_fup_adjustment Logical indicator of whether to use an adjusted
#' estimate for fetal fup based on the fetal:maternal plasma protein binding
#' ratios presented in McNamara and Alcorn's 2002 study "Protein Binding
#' Predictions in Infants." Defaults to TRUE. 
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
#' 
#' McNamara PJ, Alcorn J. Protein binding predictions in infants. 
#' AAPS PharmSci. 2002;4(1):E4. doi: 10.1208/ps040104. PMID: 12049488.
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
                              fetal_fup_adjustment=TRUE,
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
  
  schmitt.params <- c(schmitt.params,fetal.plasma.pH=7.207)
  #    PCs <- predict_partitioning_schmitt(
  #      parameters=schmitt.params,
  #      regression=regression,
  #      species=species,
  #      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
  #      minimum.Funbound.plasma=minimum.Funbound.plasma)
  #    p.list <- PCs[c('Kplacenta2pu','Kfplacenta2pu')]
  #    PCs[c('Kplacenta2pu','Kfplacenta2pu')] <- NULL
  
  
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
  #parms$Kthyroid2pu <-  parms$Kfthyroid2pu <- 1 #dummy parameter in prior use
  parms$Kfthyroid2pu <- parms$Kthyroid2pu #thyroid should be supported now
  parms$Kfliver2pu <- parms$Kliver2pu
  parms$Kfkidney2pu <- parms$Kkidney2pu
  parms$Kfrest2pu <- parms$Krest2pu
  parms$Kfgut2pu <- parms$Kgut2pu
  parms$Kflung2pu <- parms$Klung2pu
  parms$Kfbrain2pu <- Kbrain2pu
  
  if (fetal_fup_adjustment == TRUE){
  #After calling parameterize_pbtk to get certain maternal parameters for 
  #our model, make adjustment to the fetal fraction of chemical unbound to
  #protein parameter (also goes by fup or Funbound.plasma):
  
  #McNamara and Alcorn's "Protein Binding Predictions in Infants" from 2002
  #provides the infant:maternal plasma protein concentration parameters set 
  #here for use in implementing an adjusted Funbound.plasma scheme. We make
  #different estimates for fetal and maternal Funbound.plasma depending
  #on which plasma protein is assumed to predominate in binding the chemical.
  Pinfant2Pmaternal_hsa = 1.163 #human serum albumin
  Pinfant2Pmaternal_aag = 0.38 #Alpha 1-acid glycoprotein
  
  #Our assumption of which plasma protein predominates in binding the chemical
  #is based on a general observation of how acids and neutral chemicals 
  #preferentially bind to hsa, while basic chemicals tend to bind to aag. 
  #At the plasma pH of 7.4, if the fraction of a chemical that is in positive
  #ionic form according to the calc_ionization function is greater than 50%,
  #we treat the chemical as a base (which is in its conjugate acid form)
  #and use only the Pinfant2Pmaternal_aag value. Otherwise, we assume the 
  #chemical is an acid and use Pinfant2Pmaternal_hsa. 
  
  #To run the calc_ionization function, though, we need the following values:
  plasma.pH <- 7.4
  pKa_Donor <- parms$pKa_Donor
  pKa_Accept <- parms$pKa_Accept
 
  #Now let's use calc_ionization to estimate the chemical's charge profile:
  ion <- calc_ionization(
    pH=plasma.pH,
    pKa_Donor=pKa_Donor,
    pKa_Accept=pKa_Accept)
  
  fraction_positive <- ion$fraction_positive
  
  if (fraction_positive > 0.5) {
    Pinfant2Pmaternal <- Pinfant2Pmaternal_aag
  } else Pinfant2Pmaternal <- Pinfant2Pmaternal_hsa
  
  Funbound.plasma <- parms$Funbound.plasma #value of Funbound.plasma for mother
  Fraction_unbound_plasma_fetus <- 
    1 / (1 + Pinfant2Pmaternal*(1 - Funbound.plasma)/Funbound.plasma)
        
  parms$Fraction_unbound_plasma_fetus <- Fraction_unbound_plasma_fetus
  } else parms$Fraction_unbound_plasma_fetus <- parms$Funbound.plasma
                     
  
  #Key ICRP 2002 data for females, corresponding to reference BW of 60 kg:
  ICRP_2002_female_tissue_mass_fractions_data <- 10^-2 * c(
                                                   Vthyroidc = 0.0283,
                                                   Vkidneyc = 0.458,
                                                   Vgutc = 1.90,
                                                   Vliverc = 2.33,
                                                   Vlungc = 1.58,
                                                   Vbrainc = 2.17 
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
parms$gut_density <- 1.04
parms$kidney_density <- 1.05
parms$liver_density <- 1.05
parms$lung_density <- 1.05
parms$thyroid_density <- 1.05
parms$adipose_density <- 0.950
parms$ffmx_density <- 1.1
parms$placenta_density <- 1.02
parms$amnf_density <- 1.01
parms$brain_density <- 1.04
parms$BW_cubic_theta1 <- -0.010614
parms$BW_cubic_theta2 <- 0.029161
parms$BW_cubic_theta3 <- -5.0203e-4
parms$Wadipose_linear_theta0 <- 17.067
parms$Wadipose_linear_theta1 <- 0.14937
parms$Wfkidney_gompertz_theta0 <- 6.3327e-5
parms$Wfkidney_gompertz_theta1 <- 1.0409
parms$Wfkidney_gompertz_theta2 <- 0.076435
parms$Wfthyroid_gompertz_theta0 <- 0.0038483
parms$Wfthyroid_gompertz_theta1 <- 0.30799
parms$Wfthyroid_gompertz_theta2 <- 0.039800
parms$Wfliver_gompertz_theta0 <- 0.0074774
parms$Wfliver_gompertz_theta1 <- 0.65856
parms$Wfliver_gompertz_theta2 <- 0.061662
parms$Wfbrain_gompertz_theta0 <- 0.01574
parms$Wfbrain_gompertz_theta1 <- 0.70707
parms$Wfbrain_gompertz_theta2 <- 0.064827
parms$Wfgut_gompertz_theta0 <- 8.1828e-4
parms$Wfgut_gompertz_theta1 <- 0.65028
parms$Wfgut_gompertz_theta2 <- 0.047724
parms$Wflung_gompertz_theta0 <- 3.0454e-4
parms$Wflung_gompertz_theta1 <- 1.0667
parms$Wflung_gompertz_theta2 <- 0.084604
parms$hematocrit_quadratic_theta0 <- 39.192 
parms$hematocrit_quadratic_theta1 <- -0.10562
parms$hematocrit_quadratic_theta2 <- -7.1045e-4
parms$fhematocrit_cubic_theta1 <- 4.5061
parms$fhematocrit_cubic_theta2 <- -0.18487
parms$fhematocrit_cubic_theta3 <- 0.0026766
parms$fBW_gompertz_theta0 <- 0.0018282
parms$fBW_gompertz_theta1 <- 1.1735
parms$fBW_gompertz_theta2 <- 0.077577
parms$Vplacenta_cubic_theta1 <- -1.7646
parms$Vplacenta_cubic_theta2 <- 0.91775
parms$Vplacenta_cubic_theta3 <- -0.011543
parms$Vamnf_logistic_theta0 <- 822.34
parms$Vamnf_logistic_theta1 <- 0.26988
parms$Vamnf_logistic_theta2 <- 20.150
parms$Vplasma_mod_logistic_theta0 <- 1.2406
parms$Vplasma_mod_logistic_theta1 <- 0.31338
parms$Vplasma_mod_logistic_theta2 <- 17.813
parms$Vplasma_mod_logistic_theta3 <- 2.4958
parms$venous_blood_fraction <- 0.595
parms$arterial_blood_fraction <- 0.16
parms$fblood_weight_ratio <- 80 #in ml/kg
parms$Qcardiac_cubic_theta0 <- 301.78
parms$Qcardiac_cubic_theta1 <- 3.2512
parms$Qcardiac_cubic_theta2 <- 0.15947
parms$Qcardiac_cubic_theta3 <- -0.0047059
parms$term <- 40.0 #weeks at delivery
parms$Qgut_percent_initial <- 17.0
parms$Qgut_percent_terminal <- 12.5
parms$Qkidney_cubic_theta0 <- 53.248
parms$Qkidney_cubic_theta1 <- 3.6447
parms$Qkidney_cubic_theta2 <- -0.15357
parms$Qkidney_cubic_theta3 <- 0.0016968
parms$Qliver_percent_initial <- 27.0
parms$Qliver_percent_terminal <- 20.0
parms$Qthyroid_percent_initial <- 1.5
parms$Qthyroid_percent_terminal <- 1.1
parms$Qplacenta_linear_theta1 <- 0.059176
parms$Qadipose_percent_initial <- 8.5
parms$Qadipose_percent_terminal <- 7.8
parms$Qgfr_quadratic_theta0 <- 113.73
parms$Qgfr_quadratic_theta1 <- 3.5784
parms$Qgfr_quadratic_theta2 <- -0.067272
parms$Qfrvtl_logistic_theta0 <- 2466.5
parms$Qfrvtl_logistic_theta1 <- 0.14837
parms$Qfrvtl_logistic_theta2 <- 43.108
parms$Qflvtl_logistic_theta0 <- 506.30
parms$Qflvtl_logistic_theta1 <- 0.21916
parms$Qflvtl_logistic_theta2 <- 30.231
parms$Qfda_logistic_theta0 <- 1125.3
parms$Qfda_logistic_theta1 <- 0.18031
parms$Qfda_logistic_theta2 <- 35.939
parms$Qfplacenta_logistic_theta0 <- 262.20
parms$Qfplacenta_logistic_theta1 <- 0.22183
parms$Qfplacenta_logistic_theta2 <- 28.784
parms$Qfdv_gompertz_theta0 <- 1.892
parms$Qfdv_gompertz_theta1 <- 0.098249
parms$Qfdv_gompertz_theta2 <- 0.0064374
parms$Qfnonplacental_percent <- 75.0
parms$Qfgut_percent <- 6.8
parms$Qfkidney_percent <- 5.4
parms$Qfbrain_percent <- 14.3
parms$Qbrain_percent <- 12 #average of male/female ICRP 2002
parms$Qkidney_percent <- 18 #average of male/female ICRP 2002
parms$Qgut_percent <- 16 #average of male/female ICRP 2002
parms$Qfliver_percent <- 6.5
parms$Qfthyroid_percent <- 1.5
 
 
 return(parms)                             
}
