#' Solve_full_pregnancy
#' 
#' This function solves for the amounts (in umol) or concentrations (in uM) of a 
#' chemical in different tissues of a maternal-fetal system over the full course 
#' of human pregnancy given a dose and dosing frequency. 
#' 
#' The simulation starts at the 0th week and ends at 40 weeks of pregnancy (term), 
#' covering all trimesters of human pregnancy. This is accomplished by 
#' stitching together the 1tri and fetal PBTK models with appropriate initial 
#' conditions, as described in Truong et al. (TBD). 
#'
#' @param chem.name Either the chemical name, CAS number, or DTXSID
#' must be specified.
#' 
#' @param chem.cas Either the chemical name, CAS number, or DTXSID must be specified.
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard}) 
#' 
#' @param time.course Time sequence in days. Default is from 0th week of pregnancy to 
#' 40th, incremented by day. 
#' 
#' @param dose Amount of a single, initial dose (on day 0) in mg/kg BW. 
#' 
#' @param daily.dose Total daily dose, mg/kg BW for 40 weeks. 
#' 
#' @param doses.per.day Number of doses per day for 40 weeks.
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability for fetal_pbtk and 1tri_pbtk models (i.e. PFAS chemicals).
#' 
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by the modelinfo
#' files for fetal_pbtk and 1tri_pbtk. 
#' 
#' @param track.vars which variables to return in solution output dataframe
#' 
#' @param plt plots all outputs, if TRUE
#' 
#' @return A dataframe with columns for time (in days), each compartment, the 
#' area under the curve (for plasma vs time), and plasma, and a row for each time 
#' point.
#' 
#' @author Kimberly Truong
#' 
#' @keywords Solve
#' 
#' @seealso \code{\link{solve_1tri_pbtk}}
#' @seealso \code{\link{solve_fetal_pbtk}}
#' @seealso \code{\link{parameterize_1tri_pbtk}}
#' @seealso \code{\link{parameterize_fetal_pbtk}}
#' 
#' @examples
#' 
#' # dosing schedule of 1 mg/kg BW/day for 40 weeks
#' # return solution by hour
#' out <- solve_full_pregnancy(chem.name = "fipronil",  
#'                            daily.dose = 1, 
#'                            doses.per.day = 1,
#'                            time.course = seq(0, 40*7, 1/24))
#'                    
#'                                
#' # return solution in chemical amounts for fetal compartments + placenta
#' maternal_compts <- c('gutlumen', 'gut', 'liver', 'kidney', 'lung', 'ven', 'art', 
#' 'adipose','thyroid', 'rest')
#' 
#' fetal_compts <- c(maternal_compts[! maternal_compts %in% c('adipose', 'gutlumen') ], 
#' "brain")
#' 
#' amt.out <- solve_full_pregnancy(chem.name = "fipronil",  
#'                                daily.dose = 1, 
#'                                doses.per.day = 1,
#'                                time.course = seq(0, 40*7, 1), 
#'                                track.vars = c(paste0("Af", fetal_compts), "Aplacenta"))
#'
#' # return solution in concentrations for fetal compartments + placenta 
#' conc.out <- solve_full_pregnancy(chem.name = "fipronil", 
#'                                 daily.dose = 1, 
#'                                 doses.per.day = 1,
#'                                 time.course = seq(0, 40*7, 1), 
#'                                 track.vars = c(paste0("Cf", fetal_compts), "Cplacenta"))
#' 
#' # plot solution based on output 
#' plt.out <- solve_full_pregnancy(chem.name = "genistein", 
#'                                 dose = 1, plt = TRUE)
#'
#' @export solve_full_pregnancy
#' @importFrom dplyr bind_rows

solve_full_pregnancy <- function(
    chem.name = NULL,
    chem.cas = NULL,
    dtxsid = NULL,
    time.course = seq(0,40*7,1), 
    dose = NULL,
    daily.dose = NULL, 
    doses.per.day = NULL, 
    class.exclude = TRUE, 
    physchem.exclude = TRUE,
    track.vars = NULL, 
    plt = FALSE)
{
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid)) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')

  # Look up the chemical name/CAS, depending on what was provided:
  chid <- get_chem_id(chem.cas=chem.cas,
                     chem.name=chem.name,
                     dtxsid=dtxsid)
  chem.cas <- chid$chem.cas
  chem.name <- chid$chem.name
  dtxsid <- chid$dtxsid
    
  cat(paste0("Solving for chemical: ", chem.name, " (", dtxsid, ")\n"))
    
  maternal_compts <- c('gutlumen', 'gut', 'liver', 'kidney', 'lung', 'ven', 'art', 
                       'adipose','thyroid', 'rest')
  maternal_states <- paste0('A', maternal_compts)
  maternal_states <- c(maternal_states, 
                       'Atubules', 'Ametabolized', 'AUC')
  maternal_concs <- paste0("C", maternal_compts[! (maternal_compts %in% c("gutlumen"))]) # no Vgutlumen 
  
  fetal_compts <- c(maternal_compts[!( maternal_compts %in% c('adipose', 'gutlumen') )], 
                    "brain") 
  fetal_states <- c(paste0('Af', fetal_compts), 'fAUC')
  fetal_concs <- paste0("Cf", fetal_compts)
  
  # all compts by the 2nd/3rd trimesters
  mf.states <- c(maternal_states, fetal_states, "Aplacenta")
  mf.outputs <- c(mf.states, # these are amounts (states of fetal_pbtk)
                  maternal_concs, 
                  fetal_concs, 
                  "Cplacenta", 
                  "Cplasma", "Aplasma", "Rblood2plasma", 
                  "Cfplasma", "Afplasma", "Rfblood2plasma")
  
  # spell out all possible outputs of 1tri_pbtk
  firsttri.states <- c(maternal_states, "Aconceptus")
  firsttri.outputs <- c(firsttri.states, # amounts 
                        maternal_concs, "Cconceptus", 
                        "Cplasma", "Aplasma", "Rblood2plasma")
  
  # split time.course to the appropriate domain for each model
  t1 = time.course[time.course <= 13*7]
  t2 = time.course[time.course > 13*7]
  
  # we add day 91 to both time series to stitch the models together
  t1 = sort(unique(c(0,t1, 13*7))) # ode solver needs the first value of "times" to be the initial time (T0=0)
  t2 = c(13*7,t2) # initial time T0 = 91
  
  # set default dose to be 1 mg/kg BW if dosing parameters are not specified
  if(is.null(dose) & is.null(daily.dose) & is.null(doses.per.day)) 
    dose = 1
    
  # track chemical amounts (i.e. state vars of each model) 
  firsttri.out <- solve_1tri_pbtk(chem.name = chem.name,
                                  chem.cas = chem.cas,
                                  dtxsid = dtxsid,
                                  times = t1, 
                                  dose = dose,
                                  daily.dose = daily.dose, 
                                  doses.per.day = doses.per.day, 
                                  class.exclude = class.exclude, 
                                  physchem.exclude = physchem.exclude,
                                  monitor.vars = firsttri.outputs, 
                                  suppress.messages = TRUE)

  # initialize vector for "initial.values" input to fetal_pbtk 
  initial.dat <- setNames(rep(0, length(mf.states)), mf.states)
  
  # populate end values for maternal compts 
  ind <- which(firsttri.out[, 'time'] == 13*7)
  initial.dat[maternal_states] = firsttri.out[ind, maternal_states]
  
  # compute initial amts for fetal compts (and placenta)
  missing.amts <- c(paste0("Af", fetal_compts),
                    "Aplacenta")
  
  # partition out chemical into fetal compts + placenta based on weights from KfVf
  missing.vols <- sub("^A", "V", missing.amts)
  
  # get volumes from C model implementation
  vols.out <- solve_fetal_pbtk(chem.name = chem.name,
                               chem.cas = chem.cas,
                               dtxsid = dtxsid,
                               times = c(13*7), 
                               dose = 0, # solution will be trivial; we just want the volumes at end of 13 weeks
                               class.exclude = class.exclude, 
                               physchem.exclude = physchem.exclude,
                               monitor.vars = c(missing.vols, "fhematocrit", "Rfblood2plasma"), 
                               suppress.messages = TRUE
  )
  
  fetal.parms <- parameterize_fetal_pbtk(chem.name = chem.name,
                                         chem.cas = chem.cas,
                                         dtxsid = dtxsid,
                                         class.exclude = class.exclude, 
                                         physchem.exclude = physchem.exclude)
  
  # get fetal tissue partition coefficients 
  fetal.pcs <- c(fetal.parms[substr(names(fetal.parms),1,2) == 'Kf'], 
                 fetal.parms["Kplacenta2pu"])
  
  fetal.tissues <- fetal_compts[! (fetal_compts %in% c("ven", "art"))]
  
  # reorder fetal volumes to match order of fetal.pcs 
  Vf = vols.out[1, c(paste0("Vf", fetal.tissues), "Vplacenta")]
  Kf = unlist(fetal.pcs[c(paste0("Kf", fetal.tissues,"2pu"), "Kplacenta2pu")])
  
  KVtotal = sum(Kf*Vf)
  
  # add art,ven blood components
  bV = vols.out[1, c("Vfart", "Vfven")]
  KVtotal = KVtotal + vols.out[[1, "fhematocrit"]]*fetal.pcs[["Kfrbc2pu"]]*sum(bV) #RBCs
  KVtotal = KVtotal + + (1-vols.out[[1, "fhematocrit"]])/fetal.parms$Fraction_unbound_plasma_fetus*sum(bV) #plasma 
  
  Af = firsttri.out[ind, "Aconceptus"]*Vf*Kf/KVtotal
  names(Af) <- sub("^V", "A", names(Af))
  initial.dat[names(Af)] = Af 
  
  # compute amounts for Afart, Afven based on avg partition coefficient of RBCs and plasma 
  initial.dat["Afart"] = firsttri.out[[ind, "Aconceptus"]]*vols.out[[1, "Vfart"]]*vols.out[[1, "fhematocrit"]]*fetal.pcs[["Kfrbc2pu"]] 
  initial.dat["Afart"] = initial.dat[["Afart"]] + firsttri.out[[ind, "Aconceptus"]]*vols.out[[1, "Vfart"]]*(1-vols.out[[1, "fhematocrit"]])/fetal.parms$Fraction_unbound_plasma_fetus 
  initial.dat["Afart"] = initial.dat[["Afart"]]/KVtotal
  
  initial.dat["Afven"] = firsttri.out[[ind, "Aconceptus"]]*vols.out[[1, "Vfven"]]*vols.out[[1, "fhematocrit"]]*fetal.pcs[["Kfrbc2pu"]] 
  initial.dat["Afven"] = initial.dat[["Afven"]] + firsttri.out[[ind, "Aconceptus"]]*vols.out[[1, "Vfven"]]*(1-vols.out[[1, "fhematocrit"]])/fetal.parms$Fraction_unbound_plasma_fetus 
  initial.dat["Afven"] = initial.dat[["Afven"]]/KVtotal
  
  # assuming no placental barrier 
  initial.dat["fAUC"] = initial.dat["AUC"]
  
  # modified input to fetal_pbtk
  mod.fetal.out <- solve_fetal_pbtk(chem.name = chem.name,
                                    chem.cas = chem.cas,
                                    dtxsid = dtxsid,
                                    times = t2, 
                                    dose = 0,
                                    daily.dose = daily.dose, 
                                    doses.per.day = doses.per.day, 
                                    class.exclude = class.exclude, 
                                    physchem.exclude = physchem.exclude,
                                    monitor.vars = mf.outputs, 
                                    initial.values = initial.dat, 
                                    suppress.messages = TRUE)
  
  # get full solution by concatenating 2 outputs
  full_sol <- bind_rows(data.frame(firsttri.out), data.frame(mod.fetal.out))

  # add initial.values computed at day 91 from Aconceptus(13)
  # always return fetal_pbtk solution at day 91 as well
  full_sol[ind, c(missing.amts, "fAUC")] = initial.dat[c(missing.amts, "fAUC")]
  
  # convert these initial.values to concentrations
  # add these concentrations to output's first row pertaining to day 91
  missing.concs <- sub("^A", "C", missing.amts)
  full_sol[ind, missing.concs] = initial.dat[missing.amts]/vols.out[1, missing.vols]
  
  # initial plasma conc values are calculated from Afven solution
  full_sol[ind, "Cfplasma"] = initial.dat[["Afven"]]/vols.out[[1, "Vfven"]]/vols.out[[1, "Rfblood2plasma"]]
  full_sol[ind, "Afplasma"] = initial.dat[["Afven"]]/vols.out[[1, "Rfblood2plasma"]]*(1 - vols.out[[1, "fhematocrit"]])
  full_sol[ind, "Rfblood2plasma"] = vols.out[[1, "Rfblood2plasma"]]
  

  # The monitored variables can be altered by the user 
  if (is.null(track.vars)) {
    
    # have the default output columns be selected concs 
    default.track.vars <- c("Agutlumen", maternal_concs,
                            "Aconceptus", "Cconceptus",
                            "Cplasma",
                            "Atubules","Ametabolized","Rblood2plasma",
                            "AUC","fAUC", 
                            "Aplacenta", "Cplacenta",
                            fetal_concs, 
                            "Cfplasma","Rfblood2plasma")
    track.vars <- default.track.vars 
    
    full_sol <- full_sol[, c("time", track.vars)]
  }
  else {
    
    # however, always include the compartment that receives the dose 
    full_sol <- full_sol[, unique(c("time", "Agutlumen", track.vars))]
  }
  
  # convert full_sol to deSolve object for plotting to work
  full_sol <- structure(as.matrix(full_sol), class = "deSolve")
  
  # PLOTTING from deSolve
  if (plt == TRUE) {
    
    #assemble a y-axis units vector to correspond to each entry in track.vars
    n_track_vars = length(track.vars)
    plot_units_vector = rep(NA, n_track_vars) 
    
    for (var in 1:n_track_vars) {
      if (substr(track.vars[var],1,1) == 'A') {
        #other variables that start with 'A' should all be amounts
        plot_units_vector[var] = "umol"
      } else if (substr(track.vars[var],1,1) == 'C') {
        plot_units_vector[var] = "uM"
      } else if (track.vars[var] %in% c("Rblood2plasma", "Rfblood2plasma")) {
        plot_units_vector[var] = "unitless"
      } else if (track.vars[var] %in% c("AUC", "fAUC")) {
        plot_units_vector[var] = "uM*days"
      }
    }
    
    # again always include the compartment that receives the dose for visual check 
    # of dosing
    graphics::plot(full_sol, select=unique(c("Agutlumen",track.vars)),
                   ylab = plot_units_vector, xlab = 'time (days)')
  }  
  
  return(full_sol)
  
}
