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
cas_id_check <- function(cas){
  out <- grepl(cas,pattern = "^\\d{2,7}[-]\\d+{2}[-]\\d$")
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
dtxsid_id_check <- function(dtxsid){
  # generic dtxsid format check
  check1 <- grepl(dtxsid,pattern = "DTXSID\\d+")
  # checksum check for DTXSID's
  check2 <- strsplit(dtxsid,split = "") %>%
    lapply(.,function(x){as.numeric(c(x[7:length(x)]))}) %>% 
    lapply(.,function(x){x[1]==(x[3:length(x)]%*%1:length(3:length(x)))%%10}) %>% 
    unlist()
  # both checks should pass
  out <- check1 & check2
  
  return(out)
}

httk_chem_subset <- function(data,chem_include){
  # obtain the chemical ID type
  chemid_type <- name(chem_include)
  # check if the chemical ID type is NOT in the data
  if(all(!(c('cas','casrn','dtxsid')%in%tolower(colnames(data))))){
    stop("The data does not contain CAS/CASRN or DTXSID chemical identifiers.")
  }
  # checks for CAS/CASRN chemical identifiers
  if(tolower(chemid_type)%in%c("cas","casrn")){
    # check that the data has CAS/CASRN chemical identifiers
    if(grepl(colnames(data),pattern = "^cas$|^casrn$",ignore.case = TRUE)){
      stop("The data does not contain CAS/CASRN chemical identifiers.")
    }
    # check that they are CAS number format
    chem_include_check <- all(cas_id_check(chem_include)==TRUE)
    if(chem_include_check == FALSE){
      stop("At least one chemical ID in `chem_include` does not follow the standard CAS/CASRN format.")
    }
    chemid_col <- grep(colnames(data),pattern = "^cas$|^casrn$",ignore.case = TRUE)
  }
  # checks for DTXSID chemical identifiers
  if(tolower(chemid_type)%in%c("dtxsid")){
    # check that the data has CAS/CASRN chemical identifiers
    if(grepl(colnames(data),pattern = "^dtxsid$",ignore.case = TRUE)){
      stop("The data does not contain DTXSID chemical identifiers.")
    }
    # check that they are DTXSID number format
    chem_include_check <- all(dtxsid_id_check(chem_include)==TRUE)
    if(chem_include_check == FALSE){
      stop("At least one chemical ID in `chem_include` is not a valid DSSTox Chemical Identifier (DTXSID).")
    }
    chemid_col <- grep(colnames(data),pattern = "^dtxsid$",ignore.case = TRUE)
  }
  
  ## CHECKS ##
  # obtain any chemicals that are not in the chemical ID list
  out_chem_list <- which(!(chem_include%in%data[,chemid_col]))
  if(length(out_chem_list)==length(chem_include)){
    # provide an error message if none of the chemical identifiers to include are
    # available in the dataset
    stop("None of the chemical identifiers provided are in the dataset.")
  }
  if(length(out_chem_list)>0){
    # provide a message listing the chemical identifiers that are not available
    # in the dataset
    cat("The following chemical identifiers are not in the dataset:\n\t",
        paste0(chem_include[out_chem_list],collapse = ", "))
  }
  # subset to the chemicals that are included in the dataset
  out_data <- data[which(data[,chemid_col]%in%chem_include),]
  
  return(out_data)
}