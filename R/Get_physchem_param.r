#' Get physico-chemical parameters from chem.physical_and_invitro.data
#'
#' This function retrives physico-chemical properties ("param") for the chemical specified 
#' by chem.name or chem.CAS from the vLiver tables.
#' 
#' @param param The desired parameters, a vector or single value.
#' @param chem.name The chemical names that you want parameters for, a vector or single value
#' @param chem.CAS The chemical CAS numbers that you want parameters for, a vector or single value
#' 
#' @return The paramters, either a single value, a named list for a single chemical, or a list of lists
#' 
#' @examples 
#' 
#' get_physchem_param(param = 'logP', chem.CAS = '80-05-7')
#' get_physchem_param(param = c('logP','MW'), chem.CAS = c('80-05-7','81-81-2'))
#' 
#'
#' @export get_physchem_param 
get_physchem_param <- function(param, chem.name=NULL, chem.CAS=NULL)
{

  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  chem.CAS0 <- chem.CAS
  chem.name0 <- chem.name
  if(is.null(chem.CAS) & is.null(chem.name)){
    stop("Must specifiy compound name or CAS.\n")
  }else if((!is.null(chem.CAS) & !any(chem.physical_and_invitro.data[,"CAS"] %in% chem.CAS)) & 
           (!is.null(chem.name) & !any(chem.physical_and_invitro.data[,"Compound"] %in% chem.name))){
    stop("Compounds not found.\n")
  }else{
    if(!is.null(chem.CAS)){
      chem.name <- chem.physical_and_invitro.data[match(chem.CAS, chem.physical_and_invitro.data[,"CAS"]),"Compound"]
    }else{
      chem.CAS <- chem.physical_and_invitro.data[match(chem.name, chem.physical_and_invitro.data[,"Compound"]),"CAS"]
    }
    
    if(!is.null(chem.CAS0) & any(is.na(chem.name))){
      stop(paste("CAS not matched in chem.physical_and_invitro.data for input CAS:", paste(chem.CAS0[is.na(chem.name)], collapse = ",")))
    }
    if(!is.null(chem.name0) & any(is.na(chem.CAS))){
      stop(paste("Compound name not matched in chem.physical_and_invitro.data for input Compounds:", paste(chem.name0[is.na(chem.CAS)], collapse = ",")))
    }

    if(!all(param %in% c("MW","logP","pKa_Donor","pKa_Accept",'logMA',"logP","logHenry","logWSol","MP"))){
      stop(paste("Parameter",param,"not among \"MW\", \"logP\", \"logMA\", \"logHenry\", \"logWSol\", \"MP\", \"pKa_Donor\", and \"pKa_Accept\".\n"))
    }
    this.index <- match(chem.CAS, chem.physical_and_invitro.data[,"CAS"])
    if(!any(is.na(suppressWarnings(chem.physical_and_invitro.data[this.index,
                                                                  param[!param %in% c("pKa_Accept","pKa_Donor", "logMA")]]))) | 
       any(param %in% c("pKa_Donor","pKa_Accept","logMA"))){
      values <- chem.physical_and_invitro.data[this.index,param]
      if(any(!param %in% c("pKa_Accept", "pKa_Donor"))){
        values.out <- lapply(as.list(values[!param %in% c("pKa_Accept", "pKa_Donor")]), as.numeric)
      }else{
        values.out <- list()
      }
      if(any(param %in% c("pKa_Donor", "pKa_Accept"))){
        
        if(length(param) > 1){

          if(length(chem.CAS) > 1){
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
                 function(x) paste0(chem.CAS[x], ": ", 
                                    paste(param[!param %in% c("pKa_Accept", "pKa_Donor", "logMA")][missing.param[missing.param[,1] %in% x,2]],
                                          collapse = ", "))),
                 collapse = "\n")))
          
         
        }else if(length(this.index) > 1 & length(param == 1)){
          
          stop(paste0("Incomplete phys-chem data for ", param, " for: \n",
                     paste(chem.CAS[missing.param], collapse = ",")
                     ))

        }
      }
      
    }
  }
}

