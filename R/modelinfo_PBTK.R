# Add this model to the list of models:
model.list[["pbtk"]]$analytic.css.func <- "calc_analytic_css_pbtk"

# These are all the paramters returned by the model parameterization fucntion.
# Some of these paramters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["pbtk"]]$param.names <- c("BW",
                    "Clint",
                    "Clmetabolismc",
                    "Funbound.plasma",
                    "Funbound.plasma.dist",
                    "Funbound.plasma.adjustment",
                    "Fgutabs",
                    "Fhep.assay.correction",
                    "hematocrit",
                    "Kgut2pu",
                    "kgutabs",
                    "Kkidney2pu",
                    "Kliver2pu",
                    "Klung2pu",
                    "Krbc2pu",
                    "Krest2pu",
                    "liver.density",
                    "million.cells.per.gliver",
                    "MW",
                    "Pow",
                    "pKa_Donor",
                    "pKa_Accept",
                    "MA",
                    "Qcardiacc",
                    "Qgfrc",
                    "Qgutf",
                    "Qkidneyf",
                    "Qliverf",
                    "Rblood2plasma",
                    "Vartc",
                    "Vgutc",
                    "Vkidneyc",
                    "Vliverc",
                    "Vlungc",
                    "Vrestc",
                    "Vvenc")
                    
# The parameters are needed to calculate the derivative of the system of
# equations describing the model:
model.list[["pbtk"]]$solver.param.names <- c("BW",
                    "Clmetabolismc",
                    "Fraction_unbound_plasma",
                    "hematocrit",
                    "Kgut2pu",
                    "kgutabs",
                    "Kkidney2pu",
                    "Kliver2pu",
                    "Klung2pu",
                    "Krest2pu",
                    "Qcardiacc",
                    "Qgfrc",
                    "Qgutf",
                    "Qkidneyf",
                    "Qliverf",
                    "Rblood2plasma",
                    "Vartc",
                    "Vgutc",
                    "Vkidneyc",
                    "Vliverc",
                    "Vlungc",
                    "Vrestc",
                    "Vvenc")

# These parameters specific the exposure scenario simulated by the model:
model.list[["pbtk"]]$solver.param.names <- c("daily.dose",
                    "dose", # Assume dose is in mg/kg BW/day  
                    "doses.per.day",
                    "iv.dose",
                    "dosing.matrix")
model.list[["pbtk"]]$parameterize.func <- "parameterize_pbtk"
model.list[["pbtk"]]$compiled.init.func <- "init_mod"


model.list[["pbtk"]]$derivative.func <- "derivspbtk"
model.list[["pbtk"]]$derivative.output.names <- c(
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma"
)

# Allowable units (and whether they are for amounts or concentration):
model.list[["pbtk"]]$conc.units <- c('um', 'mg/l')
model.list[["pbtk"]]$amount.units <- c('umol', 'mg')

#model.list[["pbtk"]]$R.init.func <- "R_initfunc_pbtk"
model.list[["pbtk"]]$dosing.params <- c("daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")
model.list[["pbtk"]]$routes <- c("oral","iv")
# We need to know which compartment gets the dose 
model.list[["pbtk"]]$dose.variable <- list(oral="Agutlumen",
  iv="Aven")
# Can take the values "add" to add dose C1 <- C1 + dose,
#"replace" to change the value C1 <- dose
#or "multiply" to change the value to C1 <- C1*dose
model.list[["pbtk"]]$dose.type <- list(oral="add",
  iv="add")

# This function handles setting up the model (including the initial dose):
R_initfunc_pbtk <- function(initial.state,dosing,use.amounts=F)
{
  # Check that dosing isn't ambiguous:
  if (!is.null(dosing$dose) & 
    !is.null(dosing$daily.dose) & 
    !is.null(dosing$dosing.matrix))
  {
    stop("Please specify only one of the arguments \"dose\", \"daily.dose\", \
    or \"dosing.matrix\"")
  }

  state <- initial.state

  state["Atubules"] <- 0
  state["Ametabolized"] <- 0
  state["AUC"] <- 0

  if (dosing$route=="iv")
  {
    state["Aven"] <- state["Aven"] + dosing$dose
  } else if (dosing$route=="oral")
  {
    state["Agutlumen"] <- state["Agutlumen"] + dosing$dose
  } else stop (paste("Route",dosing$route,"is unavailable"))

  return(state)
}


# OLD INFO (for compatibility testing)

# Add this model to the list of models:
model.list[["pbtk"]]$analytic.css.func <- "calc_analytic_css_pbtk"

#Define the parameter names for each model in one place so that all functions can use them:
param.names.pbtk <- c("BW",
                    "Clint",
                    "Clmetabolismc",
                    "Funbound.plasma",
                    "Funbound.plasma.dist",
                    "Funbound.plasma.adjustment",
                    "Fgutabs",
                    "Fhep.assay.correction",
                    "hematocrit",
                    "Kgut2pu",
                    "kgutabs",
                    "Kkidney2pu",
                    "Kliver2pu",
                    "Klung2pu",
                    "Krbc2pu",
                    "Krest2pu",
                    "liver.density",
                    "million.cells.per.gliver",
                    "MW",
                    "Pow",
                    "pKa_Donor",
                    "pKa_Accept",
                    "MA",
                    "Qcardiacc",
                    "Qgfrc",
                    "Qgutf",
                    "Qkidneyf",
                    "Qliverf",
                    "Rblood2plasma",
                    "Vartc",
                    "Vgutc",
                    "Vkidneyc",
                    "Vliverc",
                    "Vlungc",
                    "Vrestc",
                    "Vvenc")

param.names.pbtk.solver <- c("BW",
                    "Clmetabolismc",
                    "Fraction_unbound_plasma",
                    "hematocrit",
                    "Kgut2pu",
                    "kgutabs",
                    "Kkidney2pu",
                    "Kliver2pu",
                    "Klung2pu",
                    "Krest2pu",
                    "Qcardiacc",
                    "Qgfrc",
                    "Qgutf",
                    "Qkidneyf",
                    "Qliverf",
                    "Rblood2plasma",
                    "Vartc",
                    "Vgutc",
                    "Vkidneyc",
                    "Vliverc",
                    "Vlungc",
                    "Vrestc",
                    "Vvenc")

pbtk.initparms <- function(newParms = NULL){
  parms <- c(
    BW = 70,
    Clmetabolismc = 0.203,
    hematocrit = 0.44,
    kgutabs = 1,
    Kkidney2pu = 0,
    Kliver2pu = 0,
    Krest2pu = 0,
    Kgut2pu = 0,
    Klung2pu = 0,
    Qcardiacc = 4.8,
    Qgfrc = 0.108,
    Qgutf = 0.205,
    Qkidneyf = 0.221,
    Qliverf = 0.0536,
    Vartc = 0.0487,
    Vgutc = 0.0158,
    Vkidneyc = 0.00119,
    Vliverc = 0.02448,
    Vlungc = 0.00723,
    Vrestc = 0.77654,
    Vvenc = 0.0487,
    Fraction_unbound_plasma = 0.0682,
    Rblood2plasma = 0.0,
    Clmetabolism = 0.0,
    Qcardiac = 0.0,
    Qgfr = 0.0,
    Qgut = 0.0,
    Qkidney = 0.0,
    Qliver = 0.0,
    Qrest = 0.0,
    Vart = 0.0,
    Vgut = 0.0,
    Vkidney = 0.0,
    Vliver = 0.0,
    Vlung = 0.0,
    Vrest = 0.0,
    Vven = 0.0
  )
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
  }
  if (!is.null(newParms)) parms[names(newParms)] <- newParms
  out <- .C("getParmspbtk",
   as.double(parms),
  out=double(length(parms)),
  as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

pbtk.Outputs <- c(
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma"
)


model.list[["pbtk"]]$amount.compartments<- c(
    "Agutlumen",
    "Agut",
    "Aliver",
    "Aven",
    "Alung",
    "Aart",
    "Arest",
    "Akidney",
    "Atubules",
    "Ametabolized",
    "AUC")
    
pbtk.initState <- function(parms, newState = NULL) {
  Y <- c(
    Agutlumen = 0.0,
    Agut = 0.0,
    Aliver = 0.0,
    Aven = 0.0,
    Alung = 0.0,
    Aart = 0.0,
    Arest = 0.0,
    Akidney = 0.0,
    Atubules = 0.0,
    Ametabolized = 0.0,
    AUC = 0.0
  )
  Y <- with(as.list(parms), {  Y
  })

  if (!is.null(newState)) {
    if (!all(names(newState) %in% c(names(Y)))) {
      stop("illegal state variable name in newState")
    }
    Y[names(newState)] <- newState
  }
  Y
}
