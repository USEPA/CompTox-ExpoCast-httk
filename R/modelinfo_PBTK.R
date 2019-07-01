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
>>>>>>> 001cd665de2fef5e6226a9e04c729d66b8d3d155

#initparms <- function(newParms = NULL){
#  parms <- c(
#    BW = 70,
#    Clmetabolismc = 0.203,
#    hematocrit = 0.44,
#    kgutabs = 1,
#    Kkidney2pu = 0,
#    Kliver2pu = 0,
#    Krest2pu = 0,
#    Kgut2pu = 0,
#    Klung2pu = 0,
#    Qcardiacc = 4.8,
#    Qgfrc = 0.108,
#    Qgutf = 0.205,
#    Qkidneyf = 0.221,
#    Qliverf = 0.0536,
#    Vartc = 0.0487,
#    Vgutc = 0.0158,
#    Vkidneyc = 0.00119,
#    Vliverc = 0.02448,
#    Vlungc = 0.00723,
#    Vrestc = 0.77654,
#    Vvenc = 0.0487,
#    Fraction_unbound_plasma = 0.0682,
#    Rblood2plasma = 0.0,
#    Clmetabolism = 0.0,
#    Qcardiac = 0.0,
#    Qgfr = 0.0,
#    Qgut = 0.0,
#    Qkidney = 0.0,
#    Qliver = 0.0,
#    Qrest = 0.0,
#    Vart = 0.0,
#    Vgut = 0.0,
#    Vkidney = 0.0,
#    Vliver = 0.0,
#    Vlung = 0.0,
#    Vrest = 0.0,
#    Vven = 0.0
#  )
#  if (!is.null(newParms)) {
#    if (!all(names(newParms) %in% c(names(parms)))) {
#      stop("illegal parameter name")
#    }
#  }
#  if (!is.null(newParms)) parms[names(newParms)] <- newParms
#  out <- .C("getParms",
#   as.double(parms),
#  out=double(length(parms)),
#  as.integer(length(parms)))$out
#  names(out) <- names(parms)
#  out
#}
#
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

# This function initializes the model:
model.list[["pbtk"]]$init.state.func <- "initState_pbtk"
initState_pbtk <- function(parms, 
  initial.values, 
  use.amounts=F) 
{
  if (use.amounts)
  {
    CompartmentsToInitialize <-c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest")
  } else {
    CompartmentsToInitialize <-c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")
  }

  for (this.compartment in CompartmentsToInitialize)
  {
  # If the compartment has a value specified in the list initial.values, then set it to that value:
    if (this.compartment %in% names(initial.values))
    {
      eval(parse(text=paste(this.compartment,"<-",initial.values[[this.compartment]])))
      
    }
  # Otherwise set the value to zero:
    else eval(parse(text=paste(this.compartment,"<- 0")))
  }
  

   if (use.amounts) 
  {
    if(iv.dose){
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen,Alung = Alung,Aliver = Aliver,
               Aven = Aven + dose,Arest = Arest,Akidney = Akidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }else{
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen + dose,Alung = Alung,Aliver = Aliver,
               Aven = Aven,Arest = Arest,Akidney = Akidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }
  }else{
    if(iv.dose){
      state <- c(Agutlumen = Agutlumen,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven + dose,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }else{
      state <- c(Agutlumen = Agutlumen + dose,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }
  }    

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
#  Y <- with(as.list(parms), {  Y
#  })

  if (!is.null(newState)) {
    if (!all(names(newState) %in% c(names(Y)))) {
      stop("illegal state variable name in newState")
    }
    Y[names(newState)] <- newState
  }
  return(Y)
}
