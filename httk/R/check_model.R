#' Check for sufficient model parameters
#' 
#' @description
#' This function halt model evaluation if not all the needed parameters
#' (as specified in the modelinfo_[MODEL].r file) are available. The function uses
#' \code{\link{get_cheminfo}}, so if the chemical has been checked against that
#' function already then evaluation should proceed as expected. If you do not
#' have the parameters you need and are using a non-human
#' species try default.to.human = TRUE (there are many more values for human than
#' any other species). If working in human, try first using
#' \code{\link{load_dawson2021}}, \code{\link{load_sipes2017}}, or
#' \code{\link{load_pradeep2020}}.
#'
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  if
#'  parameters is not specified then the chemical must be identified by either
#'  CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#'  -- if parameters is not specified then the chemical must be identified by 
#' either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param default.to.human Substitutes missing fraction of unbound plasma with
#' human values if true.
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @return Stops code from running if all parameters needed for model
#' are not available, otherwise does nothing.
#'
#' @author john Wambaugh
#'
#' @keywords Parameter 
#'
#' @seealso \code{\link{get_cheminfo}}
#'
#' @examples
#' # Should work:
#' check_model(chem.cas="80-05-7",model="pbtk",species="rat")
#' # Should work:
#' check_model(chem.cas="80-05-7",model="pbtk",species="human")
#' # Should throw an error:
#' check_model(chem.cas="80-05-1",model="pbtk",species="human")
#' # Should throw an error:
#' check_model(chem.cas="80-05-1",model="pbtk",species="rat")
check_model <- function(chem.name=NULL,
                        chem.cas=NULL,
                        dtxsid=NULL,
                        model=NULL,
                        species=NULL,
                        class.exclude=TRUE,
                        default.to.human=FALSE)
{
  good.chems <- get_cheminfo(info=c("Compound",
                                    "CAS",
                                    "DTXSID"),
                             model=model,
                             species=species,
                             class.exclude=class.exclude,
                             suppress.messages=TRUE)
  good.chems.nophyschem <- get_cheminfo(info=c("Compound",
                                    "CAS",
                                    "DTXSID"),
                             model=model,
                             species=species,
                             class.exclude=class.exclude,
                             physchem.exclude=FALSE,
                             suppress.messages=TRUE)
  if (tolower(species) != "human" & default.to.human)
  {
    good.chems <- rbind(good.chems,
                        get_cheminfo(info=c("Compound",
                                     "CAS",
                                     "DTXSID"),
                                     model=model,
                                     species="Human",
                                     class.exclude=class.exclude,
                                     suppress.messages=TRUE)
                        )       
  }
                                                
  chem.present <- FALSE
  chem.present.nophyschem <- FALSE
  if (!is.null(chem.cas))
  { 
    if (chem.cas %in% good.chems[,"CAS"]) chem.present <- TRUE
    if (chem.cas %in% good.chems.nophyschem[,"CAS"]) chem.present.nophyschem <- TRUE
  }
  if (!is.null(dtxsid))
  {
    if (dtxsid %in% good.chems[,"DTXSID"]) chem.present <- TRUE
    if (dtxsid %in% good.chems.nophyschem[,"DTXSID"]) chem.present.nophyschem <- TRUE
  }
  if (!is.null(chem.name))
  {
    if (chem.name %in% good.chems[,"Compound"]) chem.present <- TRUE
    if (chem.name %in% good.chems.nophyschem[,"Compound"]) chem.present.nophyschem <- TRUE
  }  
  
  if (!(chem.present)) 
  {
    this.index <- which(dtxsid == chem.physical_and_invitro.data[,"DTXSID"])
    if (any(this.index))
    {
      chem.class.filt <- model.list[[model]]$chem.class.filt
      if (!is.null(chem.class.filt))
      {
        chem.class <- strsplit(
          chem.physical_and_invitro.data[this.index,"Chemical.Class"],
          split = ",")
        # check if the chemical class is in the filter-out object
        in.domain <- !(any(chem.class%in%chem.class.filt))
        if (!in.domain) stop(paste0("Chemical CAS: ",
          chem.cas,
          ", DTXSID: ",
          dtxsid,
          ", named: ",
          chem.name,
          " is outside the chemical class domain for model ",
          model,
          ". See help(get_cheminfo)."
          ))
      } else if (chem.present.nophyschem)
      {
        stop(paste0("Chemical CAS: ",
          chem.cas,
          ", DTXSID: ",
          dtxsid,
          ", named: ",
          chem.name,
          " is outside the physico-chemical property domain for model ",
          model,
          ". See help(get_cheminfo)."
          ))
       }
     }

    if (tolower(species) != "human" &
        !default.to.human) stop(paste0("Chemical CAS: ",
      chem.cas,
      ", DTXSID: ",
      dtxsid,
      ", named: ",
      chem.name,
      " has insufficient parameters for model ",
      model,
      ". Try setting default.to.human=TRUE. See also help(get_cheminfo)"
      ))
    else stop(paste0("Chemical CAS: ",
      chem.cas,
      ", DTXSID: ",
      dtxsid,
      ", named: ",
      chem.name,
      " has insufficient parameters for model ",
      model,
      ". See help(get_cheminfo)."
      ))
  }
}