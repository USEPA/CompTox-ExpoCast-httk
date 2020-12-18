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
  #initialize a parms list for fetal model parameters to output
  parms <- list()
  
  #Key ICRP 2002 data for females, corresponding to reference BW of 60 kg:
  ICRP_2002_female_tissue_mass_fractions_data <- 10^-2 * c(
    Vthyroidc = 0.0283,
    Vkidneyc = 0.458,
    Vgutc = 1.90,
    Vliverc = 2.33,
    Vlungc = 1.58,
    Vbrainc = 2.17 
    #^^though brain not in maternal side of model
  )
  
  parms$pre_pregnant_BW <- 61.103 #kg
  #Override parameterize_pbtk's body weight listing with average prepregnant
  #case, as scale dosing requires an entry named 'BW'
  parms$Vthyroidc <- ICRP_2002_female_tissue_mass_fractions_data[['Vthyroidc']]
  parms$Vkidneyc <- ICRP_2002_female_tissue_mass_fractions_data[['Vkidneyc']]
  parms$Vgutc <- ICRP_2002_female_tissue_mass_fractions_data[['Vgutc']]
  parms$Vliverc <- ICRP_2002_female_tissue_mass_fractions_data[['Vliverc']]
  parms$Vlungc <- ICRP_2002_female_tissue_mass_fractions_data[['Vlungc']]
  Vbrainc_capture <- ICRP_2002_female_tissue_mass_fractions_data[['Vbrainc']]
  #brain volume fraction value not added to parms output
  
  #set density values as they are generally useful in converting between
  #weights and volumes here
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

  
  #Capture Schmitt parameters for maternal case
  maternal_schmitt_parms <- parameterize_schmitt(
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid,
      species=species,
      suppress.messages=T)
  
  maternal.blood.pH <- 7.38 #average maternal blood pH value measured by and 
  #reported in K.H. Lee 1972 for over 80 mothers.
  maternal_schmitt_parms$plasma.pH <- maternal.blood.pH
  
  #capture maternal partition coefficients
  maternal_pcs <- predict_partitioning_schmitt(parameters =
                                                 maternal_schmitt_parms)
  
  #preset our tissue.vols object to pass exact tissue volume information
  #for this model to lump_tissues.R, which may not exactly match
  #values present in tissue.data (from pkdata.xlsx)
  #Values for thyroid, liver, gut, kidney, lung, and brain.
  tissue.vols.list <- list(
    thyroid = parms$Vthyroidc*parms$pre_pregnant_BW/parms$thyroid_density,
    liver = parms$Vliverc*parms$pre_pregnant_BW/parms$liver_density,
    gut = parms$Vgutc*parms$pre_pregnant_BW/parms$gut_density,
    kidney = parms$Vkidneyc*parms$pre_pregnant_BW/parms$kidney_density,
    lung = parms$Vlungc*parms$pre_pregnant_BW/parms$lung_density,
    brain = Vbrainc_capture*parms$pre_pregnant_BW/parms$brain_density)
  
  #Run lump_tissues twice, once to get the mother's partition coefficients, and
  #once for the fetal partition coefficients. There is a slightly different
  #lumping scheme in each case, since we are not modeling the maternal brain.
  lumped_tissue_values_maternal <-
          lump_tissues(Ktissue2pu.in = maternal_pcs, species="Human",
                  model = "fetal_pbtk",
                  tissue.vols = tissue.vols.list,
                  tissuelist=list(
                  adipose = c("adipose"), gut = c("gut"), liver=c("liver"),
                  kidney=c("kidney"), lung=c("lung"), thyroid = c("thyroid"),
                  placenta = c("placenta")))
  parms <- c(parms, lumped_tissue_values_maternal[substr(names(
    lumped_tissue_values_maternal),1,1) == 'K']) #only add the partition coefficients
  
  
  #We'll need some fetal Schmitt params so we can calculate the fetal
  #Kfplacenta2pu, as well as potentially adjust the fraction unbound to plasma
  #estimate in the fetus.
  
  #set fetal plasma.pH
  fetal.blood.pH <- 7.26 #average fetal value measured by and reported in 
  #K.H. Lee 1972 for over 80 fetuses studied within 30 min of delivery.
  #Before estimating the fetal partition coefficients, we need fetal Schmitt
  #parameters based on the fetal plasma pH
  
  fetal_schmitt_parms <- maternal_schmitt_parms
  fetal_schmitt_parms$plasma.pH <- fetal.blood.pH
  
  fetal_pcs <- predict_partitioning_schmitt(parameters = fetal_schmitt_parms)
  
  #now for the fetus, with the brain included as a compartment. These
  #partition coefficients are based on the same Schmitt parameters except
  #for the plasma pH
  lumped_tissue_values_fetus <- 
          lump_tissues(Ktissue2pu.in = fetal_pcs, species="Human",
                model = "fetal_pbtk", tissue.vols = tissue.vols.list,
                tissuelist=list(
                  gut = c("gut"), liver=c("liver"), kidney=c("kidney"), 
                  lung=c("lung"), thyroid = c("thyroid"), brain = c("brain"),
                  placenta = c("placenta")))
  lumped_fetal_pcs <- lumped_tissue_values_fetus[substr(names(
    lumped_tissue_values_fetus),1,1) == 'K']
  
  #Now we need to rename the fetal pcs to distinguish them from the mother's
  num_lumped_fetal_pcs <- length(lumped_fetal_pcs)
  names_fetal_pcs_vec <- c() #initialize empty names vec
  
  for (entry in 1:num_lumped_fetal_pcs){
    names_fetal_pcs_vec[entry] <- paste(substr(names(lumped_fetal_pcs)[entry],1,1),"f",
substr(names(lumped_fetal_pcs)[entry],2,nchar(names(lumped_fetal_pcs)[entry])),
                              sep = "")
  }
  
  names(lumped_fetal_pcs) <- names_fetal_pcs_vec
  
  parms <- c(parms, lumped_fetal_pcs) #Keep expanding our parms list
  
  
  #Call parameterize_pbtk function to obtain useful parameters that these
  #models exactly share. 
  pbtk_parms <- parameterize_pbtk(chem.cas=chem.cas,
                                  chem.name=chem.name,
                                  dtxsid=dtxsid,
                                  species=species,
                                  ...)
  pbtk_parms$BW <- parms$pre_pregnant_BW #reset BW value to maternal
  #                                prepregnant value
  
  
  #Commit the parameters from parameterize_pbtk that aren't redundant with
  #parameters in this lump_tissues run to the output, after trimming away
  #what we don't need (trim the volume fractions V, partition coefficients K,
  #and flows Q):
  pbtk_parms_desired <- 
    pbtk_parms[!( substr(names(pbtk_parms),1,1) %in% c('K','V','Q') )]
  pbtk_parms_desired <- 
    pbtk_parms_desired[!(names(pbtk_parms_desired) %in% c("hematocrit",
            "liver.density"))] #we don't use a hematocrit value from 
  #parameterize_pbtk, and we've already captured our liver density value. 
  
  #capture our desired parameters from parameterize_pbtk in "parms," too
  parms <- c(parms, pbtk_parms_desired)
  
  
  #Now enter the scheme for adjusting fetal fraction of chemical unbound
  #in plasma according to plasma concentration ratios described in McNamara
  #and Alcorn 2002 if desired. "fup" also goes by "Funbound.plasma"...
  if (fetal_fup_adjustment == TRUE){
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
  pKa_Donor <- parms$pKa_Donor
  pKa_Accept <- parms$pKa_Accept
 
  #Now let's use calc_ionization to estimate the chemical's charge profile:
  ion <- calc_ionization(
    pH=fetal.blood.pH,
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
                     

#Now for the many parameters associated with the dynamic physiologic equations
#for pregnancy assembled by Dustin Kapraun and others in 2019.
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
