#' Parameterize_1tri_PBTK
#' 
#' This function initializes the parameters needed in the functions
#' solve_1tri_pbtk by calling solve_pbtk and adding additional parameters.
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). Currently only a human model is supported. 
#' @param return.kapraun2019 If TRUE (default), empirical parameters from 
#' Kapraun et al. (2019) necessary for defining the model are provided.
#' This is a subset of the httk::kapraun2019 list object with additional parameters. 
#' @param suppress.messages Whether or not the output message is suppressed.
#' @param ... Arguments passed to parameterize_pbtk.
#'
#' @return \item{pre_pregnant_BW}{Body Weight before pregnancy, kg.}
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Funbound.plasma}{Fraction of plasma that is not bound.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)} 
#' \item{Kadipose2pu}{Ratio of concentration of chemical in adipose tissue to unbound concentration in plasma.}
#' \item{Kconceptus2pu_initial}{Ratio of concentration of chemical in "conceptus" 
#' compartment to unbound concentration in plasma at time 0.} 
#' \item{Kconceptus2pu_final}{Ratio of concentration of chemical in "conceptus" 
#' compartment to unbound concentration in plasma at 13 weeks.} 
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound 
#' concentration in plasma.} 
#' \item{kgutabs}{Rate that chemical enters the gut from 
#' gutlumen, 1/h.} 
#' \item{Kkidney2pu}{Ratio of concentration of chemical in kidney tissue 
#' to unbound concentration in plasma.} 
#' \item{Kliver2pu}{Ratio of concentration of chemical in liver tissue to unbound concentration in plasma.}
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue to unbound
#' concentration in plasma.} 
#' \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} 
#' \item{Kthyroid2pu}{Ratio of concentration of chemical in thyroid tissue to unbound concentration in plasma.}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.} 
#' \item{MW}{Molecular Weight, g/mol.} 
#' \item{pH_Plasma_mat}{pH of the maternal plasma.}
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.} 
#' \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg BW.} 
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{Vthyroidc}{Volume of the thyroid per kg body weight, L/kg BW.}
#'
#'
#' @author Kimberly Truong, Mark Sfeir, Dustin Kapraun, John Wambaugh
#'
#' @references Add Kimberly's publication here. 
#' 
#' Kilford, P. J., Gertz, M., Houston, J. B. and Galetin, A.
#' (2008). Hepatocellular binding of drugs: correction for unbound fraction in
#' hepatocyte incubations using microsomal binding or drug lipophilicity data.
#' Drug Metabolism and Disposition 36(7), 1194-7, 10.1124/dmd.108.020834.
#' 
#'
#' @keywords Parameter
#'
#' @seealso \code{\link{solve_1tri_pbtk}}
#'
#' @seealso \code{\link{parameterize_pbtk}}
#'
#' @seealso \code{\link{predict_partitioning_schmitt}}
#'
#' @seealso \code{\link{apply_clint_adjustment}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#'
#' @seealso \code{\link{kapraun2019}}
#'
#' @examples
#' 
#' 
#'  parameters <- parameterize_1tri_pbtk(dtxsid = "DTXSID7020182")
#' 
#'  parameters <- parameterize_1tri_pbtk(chem.name='Bisphenol-A')
#' 
#' 
#' @export parameterize_1tri_pbtk
parameterize_1tri_pbtk<- function(
  chem.cas=NULL,
  chem.name=NULL,
  dtxsid = NULL,
  species="Human",
  return.kapraun2019=TRUE, # this is mostly a subset of httk::kapraun2019 
  suppress.messages=FALSE, 
  ...)
{
  #initialize a parms list for 1tri model parameters to output
  parms <- list()

  #
  # MATERNAL PARAMETERS:
  #
    
  #Key ICRP 2002 data for females, corresponding to reference BW of 60 kg:
  ICRP_2002_female_tissue_mass_fractions_data <- 10^-2 * c(
    Vthyroidc = 0.0283,
    Vkidneyc = 0.458,
    Vgutc = 1.90,
    Vliverc = 2.33,
    Vlungc = 1.58,
    Vbrainc = 2.17 # though brain is not included in maternal side of model
  )
  
  parms$pre_pregnant_BW <- 61.103 #kg
  
  # tissue volume fractions to compute respective Vtissue
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
    suppress.messages=TRUE)
  
  maternal.blood.pH <- 7.38 #average maternal blood pH value measured by and 
  #reported in K.H. Lee 1972 for over 80 mothers.
  maternal_schmitt_parms$plasma.pH <- maternal.blood.pH
  
  #capture maternal partition coefficients
  maternal_pcs <- predict_partitioning_schmitt(
    parameters = maternal_schmitt_parms,
    model = "1tri_pbtk",
    suppress.messages=TRUE)
  
  #preset our tissue.vols object to pass exact tissue volume information
  #for this model to lump_tissues.R, which may not exactly match
  #values present in tissue.data (from pkdata.xlsx)
  #Values for thyroid, liver, gut, kidney, lung.
  tissue.vols.list <- list(
    thyroid = parms$Vthyroidc*parms$pre_pregnant_BW/parms$thyroid_density,
    liver = parms$Vliverc*parms$pre_pregnant_BW/parms$liver_density,
    gut = parms$Vgutc*parms$pre_pregnant_BW/parms$gut_density,
    kidney = parms$Vkidneyc*parms$pre_pregnant_BW/parms$kidney_density,
    lung = parms$Vlungc*parms$pre_pregnant_BW/parms$lung_density,
    brain = Vbrainc_capture*parms$pre_pregnant_BW/parms$brain_density
    )
  
  #Run lump_tissues twice, once to get the mother's partition coefficients, and
  #once for the fetal partition coefficients. There is a slightly different
  #lumping scheme in each case, since we are not modeling the maternal brain.
  lumped_tissue_values_maternal <-
    lump_tissues(
      Ktissue2pu.in = maternal_pcs, 
      species="Human",
      model = "1tri_pbtk",
      tissue.vols = tissue.vols.list,
      tissuelist=list(
        adipose = c("adipose"), 
        # brain = c("brain"),
        gut = c("gut"), 
        liver=c("liver"),
        kidney=c("kidney"), 
        lung=c("lung"), 
        thyroid = c("thyroid"),
        conceptus = c("placenta") # this excludes conceptus (placental tissue) from the rest of the body
        ),
      suppress.messages=TRUE)
  parms <- c(parms, lumped_tissue_values_maternal[substr(names(
    lumped_tissue_values_maternal),1,1) == 'K']) #only add the partition coefficients
  
  parms$pH_Plasma_mat <- maternal.blood.pH
  
  #
  # JOINT PARAMETERS:
  #

  #Call parameterize_pbtk function to obtain useful parameters that these
  #models exactly share. 
  pbtk_parms <- parameterize_pbtk(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid,
    species=species,
    suppress.messages=TRUE,
    ...)
  pbtk_parms$BW <- parms$pre_pregnant_BW #Override parameterize_pbtk's
    #body weight listing with average prepregnant case, as scale dosing 
    #requires an entry named 'BW'
  
  
  #Commit the parameters from parameterize_pbtk that aren't redundant with
  #parameters in this lump_tissues run to the output, after trimming away
  #what we don't need (trim the volume fractions V, partition coefficients K,
  #and flows Q):
  pbtk_parms_desired <- 
    pbtk_parms[!( substr(names(pbtk_parms),1,1) %in% c('K','V','Q') )]
  pbtk_parms_desired <- 
    pbtk_parms_desired[!( names(pbtk_parms_desired) %in% c("hematocrit",
            "liver.density", "Rblood2plasma") )] #we don't use a hematocrit value 
  #from parameterize_pbtk, we've already captured our liver density value, and
  #Rblood2plasma is calculated in the dynamics of the 
  #corresponding .c file using other parameters. 
  
  #capture our desired parameters from parameterize_pbtk in "parms," too
  parms <- c(parms, pbtk_parms_desired)
  
  # compute Kconceptus2pu_initial as a volume-weighted avg of 
  # maternal partition coefficients at t = 0 
  const.tissues <- names(tissue.vols.list)[names(tissue.vols.list) != "brain"]
  
  fetal.parms <- parameterize_fetal_pbtk(chem.cas=chem.cas,
                                         chem.name=chem.name,
                                         dtxsid=dtxsid)
  
  # get maternal tissue partition coefficients 
  mat.pcs <- fetal.parms[grep("^K(?!.*f)", names(fetal.parms), perl = T)]
  
  # first, compute contribution from constant tissues 
  Vm = unlist(tissue.vols.list[const.tissues])
  Km =  unlist(mat.pcs[paste0("K", const.tissues,"2pu")])
  
  KV = sum(Vm*Km)
  
  # define all the functions necessary to compute the time-varying volumes
  # hard-coded this but perhaps there is a better way to retrieve them from the C model 
  Vplasma <- function(tw) {
    fetal.parms$Vplasma_mod_logistic_theta0 / ( 1 + exp ( -fetal.parms$Vplasma_mod_logistic_theta1 * ( tw - fetal.parms$Vplasma_mod_logistic_theta2 ) ) ) + fetal.parms$Vplasma_mod_logistic_theta3
  }
  
  hematocrit <- function(tw) {
    ( fetal.parms$hematocrit_quadratic_theta0 + fetal.parms$hematocrit_quadratic_theta1 * tw + fetal.parms$hematocrit_quadratic_theta2 * tw^2 ) / 100
  }
  
  Vrbcs <- function(tw)  {
    hematocrit(tw)/(1 - hematocrit(tw))*Vplasma(tw)
  }
  
  Vven <- function(tw) {
    fetal.parms$venous_blood_fraction * (Vrbcs(tw) + Vplasma(tw))
  }  
  
  Vart <- function(tw) {
    fetal.parms$arterial_blood_fraction * (Vrbcs(tw) + Vplasma(tw))
  }
  
  Wadipose <- function(tw) {
    fetal.parms$Wadipose_linear_theta0 + fetal.parms$Wadipose_linear_theta1 * tw
  }
  
  Vadipose <- function(tw) {
    1/fetal.parms$adipose_density*Wadipose(tw)
  }
  
  BW <- function(tw) {
    fetal.parms$pre_pregnant_BW + fetal.parms$BW_cubic_theta1 * tw + fetal.parms$BW_cubic_theta2 * tw^2 + fetal.parms$BW_cubic_theta3 * tw^3
  }
  
  Vffmx <- function(tw) {
    1 / fetal.parms$ffmx_density * ( BW(tw) - Wadipose(tw))
  } 
  
  Vrest <- function(tw) {
    Vffmx(tw) - (Vart(tw) + Vven(tw) + sum(Vm))
  }
  
  timevar.vols <- c(
    Vven = Vven(0), 
    Vart = Vart(0), 
    Vadipose = Vadipose(0), 
    Vrest = Vrest(0)
  )
  
  # add adipose, rest (fat and non-fat) terms 
  KV = KV + timevar.vols[["Vadipose"]]*mat.pcs[["Kadipose2pu"]] 
  KV = KV + timevar.vols[["Vrest"]]*mat.pcs[["Krest2pu"]] 
  
  # add art,ven blood components
  bV = timevar.vols[c("Vart", "Vven")]
  KV = KV + hematocrit(0)*mat.pcs[["Krbc2pu"]]*sum(bV) #RBCs
  KV = KV + (1-hematocrit(0))/fetal.parms$Funbound.plasma*sum(bV) #plasma 
  
  Vtotal = sum(Vm) + sum(timevar.vols)
  Kconceptus2pu_initial = KV/Vtotal
  parms$Kconceptus2pu_initial <- Kconceptus2pu_initial
  parms$Kconceptus2pu_final <- fetal.parms[["Kplacenta2pu"]]
  

# Set appropriate precision:
  parms <- lapply(parms[sort(names(parms))], set_httk_precision)
  
  
#Now for the many parameters associated with the dynamic physiologic equations
#for pregnancy from Kapraun et al. (2019):
  if (return.kapraun2019)
    # strip away most fetal growth params except for fBW, Vplacenta, Vamnf 
    kapraun2019.1tri <- 
      httk::kapraun2019[! ( substr(names(httk::kapraun2019), 1, 2) %in% c('Wf', 'Qf') )]
    kapraun2019.1tri <- 
      kapraun2019.1tri[! ( names(kapraun2019.1tri) %in% c(paste0("fhematocrit_cubic_theta", 1:3),
                                                          "fblood_weight_ratio",
                                                          paste("Q", 
                                                                c("brain", "kidney", "gut"), 
                                                                "_percent", sep = "")) )]
    # we do not need a fhematocrit or fblood_weight_ratio bc all tissues are lumped into the conceptus 
    # for the same reason, we don't need proportions of fcardiac output allotted to tissues 
    
    # add flow rates for maternal brain 
    # kapraun2019.1tri$Qbrain_percent_initial <- 12
    # kapraun2019.1tri$Qbrain_percent_terminal <- 8.8
    
    parms <- c(parms, kapraun2019.1tri)
 
 return(parms)                             
}
