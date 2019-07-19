# Add this model to the list of models:
model.list[["pbtk"]]$analytic.css.func <- "calc_analytic_css_pbtk"

# The is the R function for generating model parameters:
model.list[["pbtk"]]$parameterize.func <- "parameterize_pbtk"

# These are all the paramters returned by the R model parameterization fucntion.
# Some of these paramters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["pbtk"]]$param.names <- c(
  "BW",
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
                    
# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver:
model.list[["pbtk"]]$init.param.names <- c(
  "BW",
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

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["pbtk"]]$compiled.parameters.init <- "getParmspbtk"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["pbtk"]]$compiled.param.names <- c(
  "BW",
  "Clmetabolismc",
  "hematocrit",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Krest2pu",
  "Kgut2pu",
  "Klung2pu",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qkidneyf",
  "Qliverf",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vrestc",
  "Vvenc",
  "Fraction_unbound_plasma",
  "Rblood2plasma",
  "Clmetabolism",
  "Qcardiac",
  "Qgfr",
  "Qgut",
  "Qkidney",
  "Qliver",
  "Qrest",
  "Vart",
  "Vgut",
  "Vkidney",
  "Vliver",
  "Vlung",
  "Vrest",
  "Vven"
  )

# This function initializes the state vector for the compiled model:
model.list[["pbtk"]]$compiled.init.func <- "initmodpbtk"

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["pbtk"]]$derivative.func <- "derivspbtk"

# This is the ORDERED list of variables returned by the derivative function:
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

# These parameters specific the exposure scenario simulated by the model:
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


# OLD INFO (for compatibility testing)

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
