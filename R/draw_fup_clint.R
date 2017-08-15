#'Draw Funbound.plasma and Clint from censored or non-censored distributions.
#'
#'Given a CAS in the HTTK data set, a virtual population from HTTK-Pop, some 
#'user specifications on the assumed distributions of Funbound.plasma and Clint,
#'draw "individual" values of Funbound.plasma and Clint from those 
#'distributions.
#'
#'@export
#'
#'@param this.chem A CAS number in the HTTK data set
#'@param nsamp The number of samples to draw.
#'@param sigma.factor The coefficient of variance to assume. Default 0.3.
#'@param poormetab Logical. Whether to include poor metabolizers in the Clint 
#'  distribution or not.
#'@param fup.censor Logical. Whether to draw \code{Funbound.plasma} from a 
#'  censored distribution or not.
#'@param Clint.vary Logical, default TRUE. Whether to treat \code{Clint} as 
#'  fixed at its measured value (FALSE), or as a random variable (TRUE).
#'@param lod The average limit of detection for \code{Funbound.plasma}, below 
#'  which distribution will be censored if fup.censor is TRUE. Default 0.01.
#'  
#'@return A data.table with three columns:
#'  \code{Funbound.plasma} and \code{Clint}, containing the sampled values, and 
#'  \code{Fhep.assay.correction}, containing the value for fraction unbound in 
#'  hepatocyte assay.

draw_fup_clint <- function(this.chem,
                           nsamp,
                           sigma.factor=0.3,
                           poormetab,
                           fup.censor,
                           Clint.vary=TRUE,
                           lod=0.01,
                           Funbound.plasma.correction=T){
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  Fhep.assay.correction <- Clint <- Funbound.plasma <- junk <- NULL
  #End R CMD CHECK appeasement.
  
  #To get the measured values of Funbound.plasma and Clint, get the HTTK default
  #parameter set for the steady-state model, which contains the measured values
  #of Funbound.plasma and Clint
  pss<-httk::parameterize_steadystate(chem.cas=this.chem,
                                      species='Human',
                                      Funbound.plasma.correction=Funbound.plasma.correction)
  
  #Initialize the data table
  indiv_tmp <- data.table(junk=rep(NA, nsamp))
  
  #Assign the HTTK default value for fraction unbound in hepatocyte assay to all
  #individuals.
 
  indiv_tmp[, 
                Fhep.assay.correction:=pss$Fhep.assay.correction]
  
  #Draw Clint from a normal distribution if poor metabolizers excluded, or
  #Gaussian mixture distribution if poor metabolizers included.
  if (Clint.vary & pss$Clint>0) { #do not sample Clint if measured value is zero,
    #or if user said not to vary Clint.
    if (poormetab){ #if poor metabolizers are included:
      #Assume that 5% of the population has 10% the metabolism; i.e., assume a 
      #Gaussian mixture distribution

      #Gaussian mixture distribution: first, draw whether each individual will
      #come from the "regular metabolizers" distribution (with 95% probability),
      #or the "poor metabolizers" distribution (with 5% probability)
      components <- sample(1:2, 
                           prob=c(0.05,0.95), 
                           size=nsamp, 
                           replace=TRUE)
      #Set the means of the two distributions. Poor metabolizers distribution
      #has mean 10% of regular metabolizers distribution.
      mus <- c(0.1*pss$Clint,
               pss$Clint)
      #Set the standard deviations of the two distributions.
      #Both have a coefficient of variation given by sigma.factor.
      sds <- sigma.factor*mus
      
      #Now draw from the "regular" or "poor metabolizers" distributions as
      #assigned earlier for each individual.
      indiv_tmp[,Clint:=truncnorm::rtruncnorm(n=nsamp,
                                                  a=0,
                                                  b=Inf,
                                                  mean=mus[components], 
                                                  sd=sds[components])]
    } else{ #if poor metabolizers were excluded
      #Draw Clint from a normal distribution with mean = measured Clint, and
      #coefficient of variation given by sigma.factor.
      indiv_tmp[,Clint:=truncnorm::rtruncnorm(n=nsamp,
                                                  a=0,
                                                  b=Inf,
                                                  mean=pss$Clint, 
                                                  sd=sigma.factor*pss$Clint)]
    }
  } else { #if either the user said don't vary Clint, or measured Clint is zero,
    #just set Clint to its measured value, for all individuals.
    indiv_tmp[, Clint:=pss$Clint]
  }
  
  #next, draw Funbound.plasma from either a normal or censored distribution.
  fup.mean <- min(1, 
                  pss$Funbound.plasma) #if measured Funbound.plasma > 1, 
  #then set the distribution mean to 1.
  
  if (fup.censor){ #if user specified to use a censored distribution,
    #then draw from a normal distribution, left-censored at the specified LOD.
    indiv_tmp[, 
                  Funbound.plasma:=r_left_censored_norm(n=nsamp,
                                                        mean=fup.mean,
                                                        sd=sigma.factor*fup.mean,
                                                        lod=lod)
                  ]
  }else{ #if user specified to use a non-censored distribution
    #Draw Funbound.plasma from a normal distribution, truncated at 0 and 1.
    indiv_tmp[, Funbound.plasma:=truncnorm::rtruncnorm(n=nsamp,
                                                           a=0,
                                                           b=1,
                                                           mean=fup.mean,
                                                           sd=sigma.factor*fup.mean)] #draw from trunc normal dist
    #Any samples that were below specified LOD, fix them at LOD/2.
    indiv_tmp[Funbound.plasma<lod, 
                  Funbound.plasma:=lod/2]
  }
  
  indiv_tmp[, junk:=NULL] #remove initialization column
  return(indiv_tmp)
}