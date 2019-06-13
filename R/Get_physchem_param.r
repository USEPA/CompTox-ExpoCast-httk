# This function retrives physico-chemical properties ("param") for the chemical specified by chem.name or chem.CAS from the vLiver tables.
get_physchem_param <- function(param,chem.name=NULL,chem.CAS=NULL)
{

  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
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
    if(!all(param %in% c("MW","logP","pKa_Donor","pKa_Accept",'logMA',"logP","logHenry","logWSol","MP"))){
      stop(paste("Parameter",param,"not among \"MW\", \"logP\", \"logMA\", \"logHenry\", \"logWSol\", \"MP\", \"pKa_Donor\", and \"pKa_Accept\".\n"))
    }
    this.index <- match(chem.CAS, chem.physical_and_invitro.data[,"CAS"])
    if(!any(is.na(suppressWarnings(chem.physical_and_invitro.data[this.index,param]))) | 
       any(param %in% c("pKa_Donor","pKa_Accept","logMA"))){
      values <- chem.physical_and_invitro.data[this.index,param]
      if(any(!param %in% c("pKa_Accept", "pKa_Donor"))){
        values.out <- lapply(as.list(values[!param %in% c("pKa_Accept", "pKa_Donor")]), as.numeric)
      }else{
        values.out <- list()
      }
      if(any(param %in% c("pKa_Donor", "pKa_Accept"))){
        
        if(length(param) > 1){
          if("pKa_Donor" %in% param){
            values.out[["pKa_Donor"]] <- lapply(strsplit(gsub(";",",",values[,"pKa_Donor"]),","),
                                                function(x) sort(as.numeric(x), na.last = TRUE))
          }
          if("pKa_Accept" %in% param){
            values.out[["pKa_Accept"]] <- lapply(strsplit(gsub(";",",",values[,"pKa_Accept"]),","),
                                                 function(x) sort(as.numeric(x), na.last = TRUE))
          }
        }else{
          if("pKa_Donor" %in% param){
            values.out[["pKa_Donor"]] <- lapply(strsplit(gsub(";",",",values),","),
                                                function(x) sort(as.numeric(x), na.last = TRUE))
          }
          if("pKa_Accept" %in% param){
            values.out[["pKa_Accept"]] <- lapply(strsplit(gsub(";",",",values),","),
                                                 function(x) sort(as.numeric(x), na.last = TRUE))
          }
        }

      }
      
      if(length(this.index) == 1 & length(param) == 1){
        return(as.numeric(unlist(values.out)))
      }else if(length(this.index) >= 1 & length(param) > 1){
        return(values.out)
      }else if(length(this.index) > 1 & length(param == 1)){
        if(param %in% c("pKa_Accept", "pKa_Donor")){
          return(values.out[[param]])
        }else{
          return(unlist(values.out))
        }
      }

    }else stop(paste("Incomplete phys-chem data for ",
                     paste(chem.name, collapse = ","),
                     " -- missing ",
                     paste(param, collapse = ","),"."))
  }
}

