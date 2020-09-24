
#' Get physico-chemical parameters from chem.physical_and_invitro.data
#'
#' This function retrieves physico-chemical properties ("param") for the chemical specified 
#' by chem.name or chem.cas from the vLiver tables.
#' 
#' @param param The desired parameters, a vector or single value.
#' @param chem.name The chemical names that you want parameters for, a vector or single value
#' @param chem.cas The chemical CAS numbers that you want parameters for, a vector or single value
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @return The parameters, either a single value, a named list for a single chemical, or a list of lists
#' 
#' @author John Wambaugh and Robert Pearce
#'
#' @examples 
#'
#' get_physchem_param(param = 'logP', chem.cas = '80-05-7')
#' get_physchem_param(param = c('logP','MW'), chem.cas = c('80-05-7','81-81-2'))
#' 
#'
#' @export get_physchem_param 
get_physchem_param <- function(
                        param, 
                        chem.name=NULL,
                        chem.cas=NULL,
                        dtxsid=NULL)
{

  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  
  chem.cas0 <- chem.cas
  chem.name0 <- chem.name
  dtxsid0 <- dtxsid
  
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) ) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')
    
  # Look up the chemical name/CAS, depending on what was provide:
  if (any(is.null(chem.cas),is.null(chem.name),is.null(dtxsid)))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  }
  
  if(!is.null(chem.cas0) & any(is.na(chem.name))){
    stop(paste("CAS not matched in chem.physical_and_invitro.data for input CAS:", paste(chem.cas0[is.na(chem.name)], collapse = ",")))
  }
  if(!is.null(chem.name0) & any(is.na(chem.cas))){
    stop(paste("Compound name not matched in chem.physical_and_invitro.data for input Compounds:", paste(chem.name0[is.na(chem.cas)], collapse = ",")))
  }
  if(!is.null(dtxsid0) & any(is.na(chem.cas))){
    stop(paste("DTXSID not matched in chem.physical_and_invitro.data for input DTXSID:", paste(dtxsid0[is.na(chem.cas)], collapse = ",")))
  }
  
  
  if(!all(param %in% c("MW","logP","pKa_Donor","pKa_Accept",'logMA',"logP","logHenry","logWSol","MP"))){
    stop(paste("Parameter",param,"not among \"MW\", \"logP\", \"logMA\", \"logHenry\", \"logWSol\", \"MP\", \"pKa_Donor\", and \"pKa_Accept\".\n"))
  }

  if (length(dtxsid)!=0) this.index <- 
    match(dtxsid, chem.physical_and_invitro.data[,"DTXSID"])
  else if (length(chem.cas)!=0) this.index <- 
    match(chem.cas, chem.physical_and_invitro.data[,"CAS"])
  else this.index <- 
    match(chem.name, chem.physical_and_invitro.data[,"Compound"])
  if(!any(is.na(suppressWarnings(chem.physical_and_invitro.data[this.index,
                                  param[!param %in% c("pKa_Accept","pKa_Donor", "logMA")]]))) | 
     any(param %in% c("pKa_Donor","pKa_Accept","logMA")))
  {
    values <- chem.physical_and_invitro.data[this.index,param]
    if(any(!param %in% c("pKa_Accept", "pKa_Donor"))){
      values.out <- lapply(as.list(values[!param %in% c("pKa_Accept", "pKa_Donor")]), as.numeric)
    }else{
      values.out <- list()
    }
    if(any(param %in% c("pKa_Donor", "pKa_Accept"))){
      
      if(length(param) > 1){
        if(length(chem.cas) > 1){
          if("pKa_Donor" %in% param){
            values.out[["pKa_Donor"]] <- gsub(";",",",values[,"pKa_Donor"])
          }
          if("pKa_Accept" %in% param){
            values.out[["pKa_Accept"]] <- gsub(";",",",values[,"pKa_Accept"])
          }
        }else{
          if("pKa_Donor" %in% param){
            values.out[["pKa_Donor"]] <- unlist(gsub(";",",",values[,"pKa_Donor"]))
          }
          if("pKa_Accept" %in% param){
            values.out[["pKa_Accept"]] <- unlist(gsub(";",",",values[,"pKa_Accept"]))
          }
        }
      }else{
        if("pKa_Donor" %in% param){
          values.out[["pKa_Donor"]] <- gsub(";",",",values)
        }
        if("pKa_Accept" %in% param){
          values.out[["pKa_Accept"]] <- gsub(";",",",values)
        }
      }

    }
    
    if(length(this.index) == 1 & length(param) == 1){
      return(unlist(values.out))
    }else if(length(this.index) >= 1 & length(param) > 1){
      return(values.out)
    }else if(length(this.index) > 1 & length(param == 1)){
      if(param %in% c("pKa_Accept", "pKa_Donor")){
        return(values.out[[param]])
      }else{
        return(unlist(values.out))
      }
    }
    
  }else{
    
    if(length(this.index) == 1 & length(param) == 1){
      stop(paste0("Incomplete phys-chem data for ",
                   chem.name,
                   " -- missing ",
                   param,"."))
    }else{
      missing.param <- which(is.na(chem.physical_and_invitro.data[
        this.index,param[!param %in% c("pKa_Accept", "pKa_Donor", "logMA")]]), arr.ind = T)
      
      if(length(this.index) >= 1 & length(param) > 1){
         
        stop(paste0("Missing phys-chem data for combinations of: \n",
                    paste(lapply(unique(missing.param[,1]), 
               function(x) paste0(chem.cas[x], ": ", 
                                  paste(param[!param %in% c("pKa_Accept", "pKa_Donor", "logMA")][missing.param[missing.param[,1] %in% x,2]],
                                        collapse = ", "))),
               collapse = "\n")))
        
       
      }else if(length(this.index) > 1 & length(param == 1)){
        
        stop(paste0("Incomplete phys-chem data for ", param, " for: \n",
                   paste(chem.cas[missing.param], collapse = ",")
                   ))

      }
    }
    
  }

}

