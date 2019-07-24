# Add this model to the list of models:

#Analytic expression for steady-state plasma concentration.
model.list[["1compartment"]]$analytic.css.func <- "calc_analytic_css_1comp"

model.list[["1compartment"]]$parameterize.func <- "parameterize_1comp"

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[["1compartment"]]$param.names <- c(
  "BW",
  "Clint",
  "Clint.dist",
  "Fgutabs",
  "Fhep.assay.correction",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "Funbound.plasma.adjustment",
  "hepatic.bioavailability",
  "hematocrit",
  "kelim",
  "kgutabs",
  "liver.density",
  "million.cells.per.gliver",
  "MA",
  "MW",
  "Rblood2plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "Vdist")

# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[["1compartment"]]$Rtosolvermap <- list(
  vdist="Vdist",
  ke="kelim",
  kgutab="kgutabs")

# This function translates the R model parameters into the compiled model
# parameters:
model.list[["1compartment"]]$compiled.parameters.init <- "getParms1comp"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[["1compartment"]]$compiled.param.names <- c(
  "vdist",
  "ke",
  "kgutabs")

# This function initializes the state vector for the compiled model:
model.list[["1compartment"]]$compiled.init.func <- "initmod1comp"

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[["1compartment"]]$derivative.func <- "derivs1comp"

# This is the ORDERED list of variables returned by the derivative function:
model.list[["1compartment"]]$derivative.output.names <- c(
  "Ccompartment")

model.list[["1compartment"]]$default.monitor.vars <- c(
  "Agutlumen",
  "Ccompartment",
  "Ametabolized",
  "AUC")

# Allowable units (and whether they are for amounts or concentration):
model.list[["1compartment"]]$conc.units <- c('um', 'mg/l')
model.list[["1compartment"]]$amount.units <- c('umol', 'mg')

# These parameters specific the exposure scenario simulated by the model:
model.list[["1compartment"]]$dosing.params <- c("daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")
model.list[["1compartment"]]$routes <- c("oral","iv")
# We need to know which compartment gets the dose 
model.list[["1compartment"]]$dose.variable <- list(oral="Agutlumen",
  iv="Acompartment")
# Can take the values "add" to add dose C1 <- C1 + dose,
#"replace" to change the value C1 <- dose
#or "multiply" to change the value to C1 <- C1*dose
model.list[["1compartment"]]$dose.type <- list(oral="add",
  iv="add")


# These variables are always calculated in amounts: 
model.list[["1compartment"]]$amount.compartments<- c(
    "Agutlumen",
    "Ametabolized", 
    "Atubules",
    "AUC")

# These variables may be calculated using amounts but are returned as 
# concentrations:
model.list[["1compartment"]]$other.compartments<- c("compartment")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[["1compartment"]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )

# Do we ignore the Fups where the value was below the limit of detection?
model.list[["1compartment"]]$exclude.fup.zero <- T


# Old infor for compatibility:

param.names.1comp.solver <- c("vdist",
                     "ke",
                     "kgutabs")


initparms1comp <- function(newParms = NULL){
  parms <- c(
    vdist = 0,
    ke = 0,
    kgutabs = 1
  )
  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
  }
  if (!is.null(newParms)) parms[names(newParms)] <- newParms
  out <- .C("getParms1comp",
   as.double(parms),
  out=double(length(parms)),
  as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs1comp <- c(
    "Ccompartment"
)


initState1comp <- function(parms, newState = NULL) {
  Y <- c(
    Agutlumen = 0.0,
    Acompartment = 0.0,
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
