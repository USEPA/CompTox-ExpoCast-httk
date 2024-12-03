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
#'
#' @param dtxsid EPA's DSSTox Structure ID (\url{http://comptox.epa.gov/dashboard})  
#' @param track.vars which variables to return in solution output dataframe
#' @param plt plots all outputs, if TRUE
#' @param return.units if plt = TRUE, plots outputs in desired units of interest. 
#' @param time.course Time sequence in days. Default is from 0th week of pregnancy to 
#' 40th, incremented by day.
#' @param ... additional arguments passed to solve_fetal_pbtk and solve_1tri_pbtk
#' (this is where you input daily.dose and doses.per.day)
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
#' out <- solve_full_pregnancy(dtxsid = "DTXSID4034609", # Fipronil 
#'                                daily.dose = 1, 
#'                                doses.per.day = 1,
#'                                time.course = seq(0, 40*7, 1/24))
#'                    
#'                                
#' # return solution in chemical amounts for fetal compartments + placenta
#' maternal_compts <- c('gutlumen', 'gut', 'liver', 'kidney', 'lung', 'ven', 'art', 
#' 'adipose','thyroid', 'rest')
#' 
#' fetal_compts <- c(maternal_compts[! maternal_compts %in% c('adipose', 'gutlumen') ], 
#' "brain")
#' 
#' amt.out <- solve_full_pregnancy(dtxsid = "DTXSID4034609", # Fipronil 
#'                                daily.dose = 1, 
#'                                doses.per.day = 1,
#'                                time.course = seq(0, 40*7, 1), 
#'                                track.vars = c(paste0("Af", fetal_compts), "Aplacenta"))
#'
#' # return solution in concentrations for fetal compartments + placenta 
#' conc.out <- solve_full_pregnancy(dtxsid = "DTXSID4034609", # Fipronil 
#'                                 daily.dose = 1, 
#'                                 doses.per.day = 1,
#'                                 time.course = seq(0, 40*7, 1), 
#'                                 track.vars = c(paste0("Cf", fetal_compts), "Cplacenta"))
#' 
#' # plot solution in units of amounts
#' # in this case, time.course affects both deSolve output and plot 
#' gplot.out <- solve_full_pregnancy(dtxsid = "DTXSID5022308", # Genistein
#'                                  daily.dose = 1, 
#'                                  doses.per.day = 1,
#'                                  time.course = seq(89,92, 1/24), 
#'                                  plt = T, return.units = "amt")
#'
#' @export solve_full_pregnancy
#' @importFrom dplyr bind_rows
#' @importFrom RColorBrewer brewer.pal
#' @import data.table
#' @import ggplot2

solve_full_pregnancy <- function(dtxsid, track.vars = NULL, plt = FALSE,
                           return.units = "amt",
                           time.course = seq(0,40*7,1), 
                           ...) {
  cat("Solving for chemical: ", dtxsid, "\n")
  
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
  
  # track chemical amounts (i.e. state vars of each model) 
  firsttri.out <- solve_1tri_pbtk(dtxsid = dtxsid, 
                                  times = t1, 
                                  dose = 0, # initial dose on day 1 
                                  monitor.vars = firsttri.outputs, 
                                  suppress.messages = TRUE, # suppress messages about running single models
                                  ...)

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
  vols.out <- solve_fetal_pbtk(dtxsid = dtxsid,
                               dose = 0, 
                               times = c(13*7), 
                               monitor.vars = c(missing.vols, "fhematocrit", "Rfblood2plasma"), 
                               suppress.messages = TRUE,
  )
  
  fetal.parms <- parameterize_fetal_pbtk(dtxsid = dtxsid)
  
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
  # ode solver starts solution at day 90.9999 due to small.time in solve_model 
  # this is slight imprecise but won't change my results significantly - 
  # keep row for day 90.9999 in output for now
  mod.fetal.out <- solve_fetal_pbtk(dtxsid = dtxsid, 
                                    times = t2, 
                                    dose = 0, 
                                    monitor.vars = mf.outputs, 
                                    initial.values = initial.dat, 
                                    suppress.messages = TRUE,
                                    ...)
  
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
  
  
  # plot all states (the amts)
  if (plt == TRUE) {
    
    if (return.units == "amt") {
      
      cols <- c(maternal_states, fetal_states, 
                "Aconceptus", "Aplacenta")
      
      out <- full_sol[, c("time", cols)]
      
      # subset down to requested times 
      out <- out[which(out$time %in% time.course), ]
      
      setDT(out)
      out[, Mtotal := Agutlumen + Agut + Aliver + Akidney + Alung + Aven + Aart + Aadipose + Athyroid + Arest]
      out[, ftotal := Afgut + Afliver + Afkidney + Aflung + Afven + Afart + Afthyroid + Afrest + Afbrain]
      
      # melt the data to ggplot 
      out.m <- melt.data.table(out, id.vars = c("time"), 
                              variable.name = 'tissue', 
                              value.name = return.units)
      setDT(out.m)
      out.m[, model := '1st trimester']
      out.m[time > 91, model := '2nd-3rd trimester']
      
      p <- ggplot(out.m[!is.na(get(return.units))], 
                  aes(x = time, y = log10(get(return.units)))) + 
        geom_point(aes(color = model)) +
        scale_color_manual(values = c('1st trimester' = 'red', 
                                      '2nd-3rd trimester' = 'black')) +
        facet_wrap(~tissue) + 
        theme_bw() + 
        labs(x = 'time (days)', y = 'Amount (log10 umol)', 
             title = 'Chemical Amounts in Compartments of full gestational model') +
        guides(colour = guide_legend(override.aes = list(size = 5)))
      
      print(p)
      
    } else if (return.units == "conc") {
      cols <- c(maternal_concs, fetal_concs, "Cconceptus", "Cplacenta")
      
      out <- full_sol[, c("time", cols)]
      
      # subset down to requested times 
      out <- out[which(out$time %in% time.course), ]
      
      setDT(out)
      
      # melt the data to ggplot 
      out.m <- melt.data.table(out, id.vars = c("time"), 
                              variable.name = 'tissue', 
                              value.name = return.units)
      setDT(out.m)
      out.m[, body := "maternal"]
      out.m[tissue %in% colnames(out)[grep("^Cf", colnames(out))], body := "fetal"]
      out.m[tissue %in% c("Cconceptus", "Cplacenta"), body := "conceptus"]
      out.m[, tissue := sub("^C[f]*", "", tissue)]
      
      # plot all compartments on a graph faceted by mother/fetus
      all.tissues <- c(union(maternal_compts, fetal_compts), "conceptus", "placenta")
      set3.colors <- brewer.pal(12, 'Set3') # max num of colors for Set 3 is 12
      tissue.colors <- setNames(set3.colors, all.tissues[all.tissues != "gutlumen"])
      tissue.colors <- c(tissue.colors, "#7FC97F")
      names(tissue.colors)[13] = c("gutlumen")
      
      p <- ggplot(out.m[!is.na(get(return.units))], 
                  aes(x = time, y = log10(get(return.units)))) + 
        geom_point(aes(color = tissue)) +
        scale_color_manual(values = tissue.colors) +
        facet_wrap(~body) + 
        theme_bw() + 
        labs(x = 'time (days)', y = 'Concentration (log10 uM)', 
             title = 'Chemical Concentration in Compartments of full gestational model') +
        guides(colour = guide_legend(override.aes = list(size = 5)))
      
      print(p)
      
    } else{
      stop("Acceptable values for return.units to plot is 'conc' or 'amt.'")
    }
  }  
  
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
    return(full_sol[, c("time", default.track.vars)])
  }
  else {
    
    # however, always include the compartment that receives the dose 
    return(full_sol[, unique(c("time", "Agutlumen", track.vars))])
  }
  
}
