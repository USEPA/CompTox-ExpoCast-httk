#' Randomly draws from a one-dimensional KDE
#' 
#' Randomly draws from a one-dimensional KDE
#' 
#' 
#' @param n Number of samples to draw
#' @param fhat A list with elements x, w, and h (h is the KDE bandwidth).
#' @return A vector of n samples from the KDE fhat
#' @author Caroline Ring
#' @references 
#' \insertRef{ring2017identifying}{httk}
#' @keywords httk-pop
#' @import stats
#' @export rfun
rfun <- function(n,
                 fhat){
  tmp <- fhat$x[sample(seq_along(fhat$x), 
                       size=n, 
                       replace=TRUE, 
                       prob=fhat$w)] + 
    fhat$h*rnorm(n=n, mean=0, sd=1)
  return(tmp)
}





#' Checks whether a value, or all values in a vector, is within inclusive
#' limits
#' 
#' Checks whether a value, or all values in a vector, is within inclusive
#' limits
#' 
#' 
#' @param x A numeric value, or vector of values.
#' @param lims A two-element vector of (min, max) values for the inclusive
#' limits. If \code{x} is a vector, \code{lims} may also be a two-column matrix
#' with \code{nrow=length(x)} where the first column is lower limits and the
#' second column is upper limits. If \code{x} is a vector and \code{lims} is a
#' two-element vector, then each element of \code{x} will be checked against
#' the same limits. If \code{x} is a vector and \code{lims} is a matrix, then
#' each element of \code{x} will be checked against the limits given by the
#' corresponding row of \code{lims}.
#' @return A logical vector the same length as \code{x}, indicating whether
#' each element of \code{x} is within the inclusive limits given by
#' \code{lims}.
#' @author Caroline Ring
#' @references 
#' \insertRef{ring2017identifying}{httk}
#' @keywords httk-pop
#' @export is_in_inclusive
is_in_inclusive <- function(x, lims) {
  if (is.numeric(x)){
    if (length(x)!=1) {
      if (is.matrix(lims)){
        if (length(x)!=nrow(lims)){
          stop('x must either be length 1 or the same length as nrow(lims) if lims is a matrix')
        }
      }
    }
  } else stop('x must be a numeric vector')
  
  if (is.numeric(lims)){
    if (length(lims)==2){
      a <- lims[1]
      b <- lims[2]
    } else stop('If lims is a numeric vector, it must be length 2.') 
  } else if (is.matrix(lims)) {
    if (ncol(lims)==2){
      if (nrow(lims)>0){
        a <- lims[,1]
        b <- lims[,2]
        
      } else {
        #warning('x was numeric(0) and lims was a matrix with 0 rows; returning logical(0)')
        return(logical(0))
      }
    } else stop('If lims is a matrix, it must have two columns.')
  } else stop('lims must be either a 2-element numeric vector or a 2-column numeric matrix')
  
  if (!is.numeric(a) & is.numeric(b)) {
    print('a = ')
    print(a)
    stop('The lower limit was not numeric')
  }
  if (!is.numeric(b) & is.numeric(a)){
    print('b = ')
    print(b)
    stop('The upper limit was not numeric')
  }
  if (!is.numeric(a) & !is.numeric(b)){
    print('a = ')
    print(a)
    print('b = ')
    print(b)
    stop('The upper and lower limits were not numeric')
  }
  return((x - a)  *  (b - x) >= 0)
}

#' CAS number format check function
#' 
#' This function checks whether the CAS/CARN chemical identifier follows the anticipated
#' format of XXXXXXX-YY-Z (i.e. 2-7 digits, 2 digits, and 1 digit, respectively).
#' 
#' @param cas A character string, or vector of character strings, indicating
#' CAS/CASRN number.
#' 
#' @return Logical output (TRUE or FALSE) indicating whether the character string(s)
#' provided match the anticipated format for a CAS/CASRN chemical identifier.
#' 
#' @export
cas_id_check <- function(cas){
  # generic CAS format check
  check1 <- grepl(cas,pattern = "^\\d{2,7}[-]\\d+{2}[-]\\d$")
  # checksum for CAS ID's
  check2 <- sapply(cas,httk::CAS.checksum)
  # both checks should pass
  out <- check1 & check2
  
  return(out)
}

#' DTXSID number format check function
#' 
#' This function checks whether the DTXSID chemical identifier follows the anticipated
#' format of "DTXSID<uniqueID>".
#' 
#' @param dtxsid A character string, or vector of character strings, indicating
#' DTXSID number.
#' 
#' @return Logical output (TRUE or FALSE) indicating whether the character string(s)
#' provided match the anticipated format for a DTXSID chemical identifier.
#' 
#' @export
dtxsid_id_check <- function(dtxsid){
  # generic dtxsid format check
  check1 <- grepl(dtxsid,pattern = "DTXSID\\d+")
  # checksum check for DTXSID's - suppress warnings for NA's
  suppressWarnings(
    check2 <- strsplit(dtxsid,split = "") %>%
      lapply(.,function(x){as.numeric(c(x[7:length(x)]))}) %>% 
      lapply(.,function(x){x[1]==(x[3:length(x)]%*%1:length(3:length(x)))%%10}) %>% 
      unlist()
  )
  # both checks should pass
  out <- check1 & check2
  
  return(out)
}

#' HTTK data chemical subsetting function
#' 
#' This function is meant to take any `httk` data and subset it based on a list
#' of chemicals provided. Main functionality is for speeding up the `load_sipes2017`,
#' `load_pradeep2020`, `load_dawson2021`, `load_honda2023`, and similar phys-chem
#' data files.  However, it should be generalizable to any dataset with CAS/CASRN
#' or DTXSID chemical identifiers.
#' 
#' @param data Data frame, with chemical data, to be subset.
#' @param chem_include (\emph{character vector}) A character vector containing
#' CAS/CASRN or DTXSID chemical identifiers to include in the data subset.
#' 
#' @return A subset data set containing only the data rows for chemicals identified
#' as those that should be included.
#' 
#' @export
httk_chem_subset <- function(data,chem_include){
  # list of valid chemical ID types
  valid_chem_ids <- c('cas','casrn','dtxsid') 
  # check if the chemical ID type is NOT in the data
  if(all(!(valid_chem_ids%in%tolower(colnames(data))))){
    stop("The data does not contain any of the valid chemical identifiers:\n\t",
         paste(valid_chem_ids,collapse = ", "))
  }
  # obtain the cas/casrn & dtxsid chemical identifiers in chem_include
  cas_chems <- cas_id_check(chem_include)
  dtxsid_chems <- dtxsid_id_check(chem_include)
  # check the chemical id's are in CAS/CASRN or DTXSID format
  if(any((cas_chems|dtxsid_chems)!=TRUE)){
    stop("At least one chemical ID in `chem_include` does not follow the standard CAS/CASRN or DTXSID format.")
  }
  # placeholder for chemicals not in the chemical id list
  out_chem_list <- c()
  # placeholder for data rows to keep
  chemid_dataselect <- c()
  # checks for CAS/CASRN chemical identifiers
  if(any(cas_chems)){
    # check that the data has CAS/CASRN chemical identifiers - if TRUE stop
    if(!any(grep(colnames(data),pattern = "^cas$|^casrn$",ignore.case = TRUE))){
      stop("The data does not contain CAS/CASRN chemical identifiers.")
    }
    # obtain the cas chemical identifier column 
    cas_chemid_col <- grep(colnames(data),pattern = "^cas$|^casrn$",ignore.case = TRUE)
    # obtain the cas numbers not in the dataset
    #   (1) which chem id's to include are CAS/CASRN
    #   (2) of those that are CAS/CASRN chem id's which are not in the dataset
    #   (3) save the out_chems
    out_chem_list <- c(out_chem_list,which(cas_chems)[which(!(chem_include[cas_chems]%in%data[,cas_chemid_col]))])
    # obtain the rows of data for relevant cas/casrn ID's
    chemid_dataselect <- c(chemid_dataselect,which(data[,cas_chemid_col]%in%chem_include))
  }
  # checks for DTXSID chemical identifiers
  if(any(dtxsid_chems)){
    # check that the data has CAS/CASRN chemical identifiers - if TRUE stop
    if(!any(grepl(colnames(data),pattern = "^dtxsid$",ignore.case = TRUE))){
      stop("The data does not contain DTXSID chemical identifiers.")
    }
    # obtain the dtxsid chemical identifier column
    dtxsid_chemid_col <- grep(colnames(data),pattern = "^dtxsid$",ignore.case = TRUE)
    # obtain the cas numbers not in the dataset
    #   (1) which chem id's to include are DTXSID
    #   (2) of those that are CAS/CASRN chem id's which are not in the dataset
    #   (3) save the out_chems
    out_chem_list <- c(out_chem_list,which(dtxsid_chems)[which(!(chem_include[dtxsid_chems]%in%data[,dtxsid_chemid_col]))])
    # obtain the rows of data for relevant dtxsid ID's
    chemid_dataselect <- c(chemid_dataselect,which(data[,dtxsid_chemid_col]%in%chem_include))
  }
  
  ## CHECKS ##
  if(length(out_chem_list)==length(chem_include)){
    # provide an error message if none of the chemical identifiers to include are
    # available in the dataset
    stop("None of the chemical identifiers provided are in the dataset.")
  }
  if(length(out_chem_list)>0){
    # provide a message listing the chemical identifiers that are not available
    # in the dataset
    cat("The following chemical identifiers are not in the dataset:\n\t",
        paste0(chem_include[sort(out_chem_list)],collapse = ", "))
    cat("\n\n")
  }
  
  # subset to the chemicals that are included in the dataset
  out_data <- data[sort(chemid_dataselect),]
  
  return(out_data)
}