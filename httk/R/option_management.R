#' Ammend an httk option list
#' 
#' This function adds default values to httk options that are formatted as a list
#' in the case where the user neglected to include all of the options in the list.
#' 
#' @param httk.option.list An httk option that is formatted as a list may be: \code{Caco2.options}
#'
#' @return \item{httk.option.list}{The ammended list of options}
#' @author Gregory Honda
  # List of httk options that are formatted as a list
httk.option.listoflists <- list(
  'Caco2.options' = list(
    Caco2.Pab.default = "1.6",
    Caco2.Fgut = TRUE,
    Caco2.Fabs = TRUE,
    overwrite.invivo = FALSE,
    keepit100 = FALSE)
    )
  
ammend.httk.option.list <- function(httk.option.list = NULL){
  

  
  # Get the name of the input option.list
  option.name.in <- deparse(substitute(httk.option.list))  
  
  #
  if(option.name.in == "NULL"){
    stop('Named option list must be supplied to ammend.httk.option.list')
  }else if(!option.name.in %in% names(httk.option.listoflists)){
    stop(paste0('Named option list ',option.name.in,' not in list of httk option list names: ',
                paste(names(httk.option.listoflists), collapse = ", ")))
  }else if(is.null(httk.option.list)){
    httk.option.list <- httk.option.listoflists[[option.name.in]]
    warning(paste0('Updating default option list: ',option.name.in,
                   '\n with values: ', paste(httk.option.list, collapse = ', '),
                   '\n for named list items: ', paste(names(httk.option.list), collapse = ', ')))
    
  }else if(!all(names(httk.option.listoflists[[option.name.in]]) %in% names(unlist(httk.option.list)))){
    
    httk.option.list[names(httk.option.listoflists[[option.name.in]])[!names(httk.option.listoflists[[option.name.in]]) %in% names(unlist(httk.option.list))]] <- 
      httk.option.listoflists[[option.name.in]][names(httk.option.listoflists[[option.name.in]])[!names(httk.option.listoflists[[option.name.in]]) %in% names(unlist(httk.option.list))]]
    warning(paste0('Updating default option list: ',option.name.in,
                   '\n with values: ', paste(httk.option.list, collapse = ', '),
                   '\n for named list items: ', paste(names(httk.option.list), collapse = ', ')))
  }
  
  return(httk.option.list)
  
}
