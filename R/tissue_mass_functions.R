#' Predict blood mass.
#' 
#' Predict blood mass based on body surface area and gender, using equations
#' from Bosgra et al. 2012
#' 
#' @param BSA Body surface area in m^2. May be a vector.
#' @param gender Either 'Male' or 'Female'. May be a vector.
#'   
#' @return A vector of blood masses in kg the same length as \code{BSA} and 
#'   \code{gender}.
blood_weight <- function(BSA, gender){
  #From Bosgra et al. 2012, eq 10 and 11
  bw<-rep(NA,length(gender))
  bw[gender=='Male'] <- 3.33*BSA[gender=='Male']-0.81
  bw[gender=='Female'] <- 2.66*BSA[gender=='Female']-0.46
  return(bw)
}

#'Find average blood masses by age.
#'
#'If blood mass from \code{\link{blood_weight}} is negative or very small, then
#'just default to the mean blood mass by age. (Geigy Scientific Tables, 7th ed.)
#'                                     
#'@param blood_mass A vector of blood masses in kg to be replaced with averages.
#'@param age_months A vector of ages in months.
#'@param age_years A vector of ages in years.
#'@param gender A vector of genders (either 'Male' or 'Female').
#'@param weight A vector of body weights in kg.
#'  
#'@return A vector of blood masses in kg.
blood_mass_correct <- function(blood_mass, 
                               age_months,
                               age_years,
                               gender,
                               weight){
  
  blood_mass[age_months<3]<- 83.3*weight[age_months<3]/1000/1.06
  blood_mass[is_in_inclusive(age_months, 
                             c(3,6))]<- 87*
    weight[is_in_inclusive(age_months, 
                                 c(3,6))]/1000/1.06
  blood_mass[is_in_inclusive(age_months, 
                             c(7,72))]<- 80*
    weight[is_in_inclusive(age_months, 
                                 c(7,72))]/1000/1.06
  blood_mass[is_in_inclusive(age_months, 
                             c(73,120))]<- 75*
    weight[is_in_inclusive(age_months, 
                                 c(73,120))]/1000/1.06
  blood_mass[is_in_inclusive(age_months, 
                             c(121,15*12))]<- 71*
    weight[is_in_inclusive(age_months, 
                                 c(121,15*12))]/1000/1.06
  blood_mass[age_years>15 & 
               gender=='Male']<-71*
    weight[age_years>15 & 
                   gender=='Male']/1000/1.06
  blood_mass[age_years>15 & 
               gender=='Female']<-70*
    weight[age_years>15 & 
                   gender=='Female']/1000/1.06
  return(blood_mass)
}


#' Predict body surface area.
#' 
#' Predict body surface area from weight, height, and age, using Mosteller's
#' formula for age>18 and Haycock's formula for age<18
#' 
#' @param BW A vector of body weights in kg.
#' @param H A vector of heights in cm.
#' @param age_years A vector of ages in years.
#' 
#' @return A vector of body surface areas in cm^2.
body_surface_area <- function(BW, H, age_years) {
  #BW in kg, H in cm
  bsa <- rep(NA, length(BW))
  bsa[age_years>=18]<-sqrt(BW[age_years>=18]*H[age_years>=18]/3600) #Mosteller's formula -- for adults
  bsa[age_years<18] <- 0.024265*BW[age_years<18]^0.5378*H[age_years<18]^0.3964 # Haycock's formula -- for children/adolescents
  return(bsa*(100^2)) #convert bsa in m^2 to cm^2
}

#' Predict bone mass.
#' 
#' Predict bone mass from age_years, height, weight, gender,
#' using logistic equations fit to data from Baxter-Jones et al. 2011,
#'or for infants < 1 year, using equation from Koo et al. 2000
#' (See Price et al. 2003)
#' 
#' @param age_years Vector of ages in years.
#' @param age_months Vector of ages in months.
#' @param height Vector of heights in cm.
#' @param weight Vector of body weights in kg.
#' @param gender Vector of genders, either 'Male' or 'Female'.
#' 
#' @return Vector of bone masses.
bone_mass_age <- function(age_years, age_months, height, weight, gender){
  #From data in Baxter-Jones et al. 2011: for males, bone mineral content in kg
  #obeys the following logistic equation: bone mass (kg) = 0.89983 + (2.99019 -
  #0.89983)/(1 + exp((14.17081-age_years)/1.58179)) derived from doing nls fit
  #using SSfpl to data in first col of Table 4 and using 13.5 as the age of PHV
  #(peak height velocity) for males as stated on page 1731 (i.e. adding 13.5 to
  #data in BAge column of Table 4)
  bmc <- rep(NA,length(age_years))
  
  fmale <- function(a){
    return(0.89983 + 
             (2.99019 - 0.89983)/
             (1 + exp((14.17081-a)/1.58179)))
  }
  
  bmc[gender=='Male' & age_years>1] <- fmale(a=age_years[gender=='Male' & age_years>1])
  
  #Similarly, for females:
  #bone mineral content = 0.74042 + (2.14976 - 0.74042)/(1 + exp((12.35466-age_years)/1.35750))
  
  ffemale <- function(a){
    return(0.74042 + 
             (2.14976 - 0.74042)/
             (1 + exp((12.35466-a)/1.35750)))
  }
  
  bmc[gender=='Female' &
        age_years>1] <- ffemale(a=age_years[gender=='Female' &
                                  age_years>1])
  
  #For infants < 1 year
  #Using equation from Koo et al. 2000
  #See Price et al. 2003 (P3M paper)
  #Note: age_months converted to age in days using WHO conversion,
  #1 month = 30.4375 days
  f_inf <- function(a.m,wt,ht){
    return((77.24+
              24.94*wt+
              0.21*(a.m*30.4375)-
              1.889*ht)/1000)
  }
  
  bmc[age_years<=1] <- f_inf(a.m=age_months[age_years<=1],
                       wt=weight[age_years<=1],
                       ht=height[age_years<=1])
  
  #And deal with loss of bone density after age 50:
  #(see Looker et al. 2009)
  #Approximately 65% of the dry bone mass is mineral.
  
  bmc[gender=='Female' & age_years>=50] <- bmc[gender=='Female' & age_years>=50] -
    0.0056*age_years[gender=='Female' & age_years>=50]
  bmc[gender=='Male' & age_years>=50] <- bmc[gender=='Male' & age_years>=50] -
    0.0019*age_years[gender=='Male' & age_years>=50]
  bonemass <- bmc/0.65 #65% of dry bone mass is mineral
  return(bonemass)
}

#' Predict brain mass.
#' 
#' Predict brain mass from gender and age.
#' 
#' @param gender Vector of genders, either 'Male' or 'Female'
#' @param age_years Vector of ages in years.
#' 
#' @return A vector of brain masses in kg.
brain_mass <- function(gender, age_years){
  B<- rep(NA, length(gender))
  B[gender=='Male'] <- 0.405 #kg; brain mass at birth
  B[gender=='Female'] <- 0.373 #kg; brain mass at birth
  brain.mean.mass <- B*((3.68 - 2.68*exp(-age_years/0.89)) * exp(-age_years/629))
  return(brain.mean.mass)
}

#'Predict kidney mass for children.
#'
#'For individuals under age 18, predict kidney mass from weight, height, and
#'gender. using equations from Ogiu et al.
#'
#'@param weight Vector of weights in kg.
#'@param height Vector of heights in cm.
#'@param gender Vector of genders (either 'Male' or 'Female').
#'  
#'@return A vector of kidney masses in kg.
kidney_mass_children <- function(weight, height, gender){
  km <- rep(NA, length(weight))
  #equations from Ogiu et al.
  #For males
  km[gender=='Male'] <- (10.24*
                           (height[gender=='Male']/100)*
                           sqrt(weight[gender=='Male'])+7.85)+ #left kidney
    (9.88*
       (height[gender=='Male']/100)*
       sqrt(weight[gender=='Male'])+7.2) #right kidney
  #For females
  km[gender=='Female'] <- (10.65*
                             (height[gender=='Female']/100)*
                             sqrt(weight[gender=='Female'])+6.11)+ #left kidney
    (9.88*
       (height[gender=='Female']/100)*
       sqrt(weight[gender=='Female'])+6.55) #right kidney
  
  return(km/1000) #convert g to kg
}

#'Predict liver mass for children.
#'
#'For individuals under 18, predict the liver mass from height, weight, and
#'gender, using equations from Ogiu et al.
#'
#'@inheritParams kidney_mass_children
#'
#'@return A vector of liver masses in kg.
liver_mass_children <- function(height, weight, gender){
  #equations from Ogiu et al. 1997
  #see also Price et al. 2003 (P3M paper)
  lm <- rep(NA, length(gender))
  lm[gender=='Male'] <- (576.9*height[gender=='Male']/100 +
                           8.9*weight[gender=='Male'] -
                           159.7)/1000
  
  lm[gender=='Female'] <- (674.3*height[gender=='Female']/100 +
                             6.5*weight[gender=='Female'] -
                             214.4)/1000
  return(lm)
}

#'Predict lung mass for children.
#'
#'For individuals under 18, predict the liver mass from height, weight, and
#'gender, using equations from Ogiu et al.
#'
#'@inheritParams kidney_mass_children
#'
#'@return A vector of lung masses in kg.
lung_mass_children <- function(height, weight, gender){
  #equations from Ogiu et al. 1997
  #see also Price et al. 2003 (P3M paper)
  lm <- rep(NA, length(gender))
  lm[gender=='Male'] <- ((29.08*height[gender=='Male']/100*
                            sqrt(weight[gender=='Male'])+
                            11.06) +
                           (35.47*height[gender=='Male']/100*
                              sqrt(weight[gender=='Male'])+
                              5.53))/1000
  
  
  lm[gender=='Female'] <- ((31.46*height[gender=='Female']/100*
                              sqrt(weight[gender=='Female'])+
                              1.43) +
                             (35.30*height[gender=='Female']/100*
                                sqrt(weight[gender=='Female'])+
                                1.53))/1000
  
  return(lm)
}

#'Predict pancreas mass for children.
#'
#'For individuals under 18, predict the pancreas mass from height, weight, and
#'gender, using equations from Ogiu et al.
#'
#'@inheritParams kidney_mass_children
#'
#'@return A vector of pancreas masses in kg.
pancreas_mass_children <- function(height, weight, gender){
  
  #equations from Ogiu et al. 1997
  #see also Price et al. 2003 (P3M paper)
  pm <- rep(NA, length(gender))
  pm[gender=='Male'] <- (7.46*height[gender=='Male']/100*
                           sqrt(weight[gender=='Male']) -
                           0.79)/1000
  
  pm[gender=='Female'] <- (7.92*height[gender=='Female']/100*
                             sqrt(weight[gender=='Female']) -
                             2.09)/1000
  
  return(pm)
}

#'Predict skeletal muscle mass for children.
#'
#'For individuals under age 18, predict skeletal muscle mass from gender and age,
#'using a nonlinear equation from J Cachexia Sarcopenia Muscle 2012 3:25-29.
#'
#'@param gender Vector of genders (either 'Male' or 'Female').
#'@param age_years Vector of ages in years.
#'
#'@return Vector of skeletal muscle masses in kg.
skeletal_muscle_mass_children <- function(gender, age_years){
  #Equation from J Cachexia Sarcopenia Muscle (2012) 3:25-29
  
  if (gender=='Male'){
    C1<-12.4
    C2<-11.0
    C3<-0.45
    C4<-19.7
    C5<-0.85
    C6<-13.7
  } else if (gender=='Female'){
    C1<-7.0
    C2<-6.5
    C3<-0.55
    C4<-13.0
    C5<-0.75
    C6<-11.5
  }
  
  smm <- (C1/(1+(C2*exp(-C3*age_years)))) + (C4/(1+exp(-C5*(age_years-C6))))
  return(smm)
}

#'Predict skeletal muscle mass.
#'
#'Predict skeletal muscle mass from age, height, and gender.
#'
#'For individuals over age 18, use allometrically-scaled muscle mass with an age-based scaling factor,
#'to account for loss of muscle mass with age (Janssen et al. 2000).
#'For individuals under age 18, use \code{\link{skeletal_muscle_mass_children}}.
#'
#'@seealso \code{\link{skeletal_muscle_mass_children}}
#'
#'@param smm Vector of allometrically-scaled skeletal muscle masses.
#'@param age_years Vector of ages in years.
#'@param height Vector of heights in cm.
#'@param gender Vector of genders, either 'Male' or 'Female.'
#'
#'@return Vector of skeletal muscle masses in kg.
skeletal_muscle_mass <- function(smm, age_years, height, gender){  
  #Age-related decrease in skeletal muscle mass
  #Janssen et al. 2000 (see Figure 2)
  smm[gender=='Male' &
        age_years>18] <- smm[gender=='Male' &
                         age_years>18] -
    0.001*age_years[gender=='Male' &
                age_years>18]^2
  smm[gender=='Female' &
        age_years>18] <- smm[gender=='Female' &
                         age_years>18] -
    0.001*age_years[gender=='Female' &
                age_years>18]^2
  
  #below cutoff age use equation based on gender and age
  smm[gender=='Male' &
        age_years<=18]  <- skeletal_muscle_mass_children(gender='Male', 
                                                   age_years=age_years[gender=='Male' &
                                                             age_years<=18])
  smm[gender=='Female' &
        age_years<=18]  <- skeletal_muscle_mass_children(gender='Female', 
                                                   age_years=age_years[gender=='Female' &
                                                             age_years<=18])
  return(smm)
}

#' Predict skin mass.
#' 
#' Using equation from Bosgra et al. 2012, predict skin mass from body surface area.
#' 
#' @param BSA Vector of body surface areas in cm^2.
#' 
#' @return Vector of skin masses in kg.
skin_mass_bosgra <- function(BSA){
  wskin <- exp(1.64*(BSA/100^2)-1.93)
  return(wskin)
}

#'Predict spleen mass for children.
#'
#'For individuals under 18, predict the spleen mass from height, weight, and
#'gender, using equations from Ogiu et al.
#'
#'@inheritParams kidney_mass_children
#'
#'@return A vector of spleen masses in kg.
spleen_mass_children <- function(height, weight, gender){
  
  #equations from Ogiu et al. 1997
  #see also Price et al. 2003 (P3M paper)
  sm <- rep(NA, length(gender))
  sm[gender=='Male'] <- (8.75*height[gender=='Male']/100*
                           sqrt(weight[gender=='Male']) +
                           11.06)/1000
  
  sm[gender=='Female'] <- (9.36*height[gender=='Female']/100*
                             sqrt(weight[gender=='Female']) +
                             7.98)/1000
  
  return(sm)
}
