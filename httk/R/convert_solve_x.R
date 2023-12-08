#' convert_solve_x
#' 
#' This function is designed to convert compartment values estimated from one
#' of the HTTK models (e.g. "1compartment) using the solve_model function.
#' It takes the HTTK model output matrix, model name, desired output units,
#' and compound information to perform the conversion default model units to
#' user specified units.
#' 
#' The function can be used to convert all compartments to a single unit,
#' only units for a single model compartment, or units for a set of model
#' compartments.
#' 
#' More details on the unit conversion can be found in the documentation for
#' \code{\link{convert_units}}.
#' 
#' @param model.output.mat Matrix of results from HTTK solve_model function.
#' @param model Specified model to use in simulation: "pbtk", "3compartment",
#' "3compartmentss", "1compartment", "schmitt", ...
#' @param output.units Output units of interest for the compiled components.
#' Defaults to NULL, and will provide values in model units if unspecified.
#' @param MW Molecular weight of substance of interest in g/mole 
#' @param vol Volume for the target tissue of interest in liters (L).
#' NOTE: Volume should not be in units of per BW, i.e. "kg".
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' @param dtxsid EPA's DSSTox Structure ID .
#' (\url{https://comptox.epa.gov/dashboard}) the chemical must be identified by
#' either CAS, name, or DTXSIDs.
#' @param parameters A set of model parameters, especially a set that
#' includes MW (molecular weight) for our conversions.
#' @param monitor.vars A vector of character strings indicating the model
#' component variables to retain in the conversion factor table
#' (assuming suppress.messages == FALSE).  It should also be noted this option
#' does NOT exclude columns from the input matrix provided in the
#' 'model.output.mat' parameter. (Default is NULL, i.e. conversion factors for
#' all model components are included in the reporting matrix.)
#' @param suppress.messages Whether or not the output messages are suppressed.
#' (Default is FALSE, i.e. show messages.)
#' @param verbose Whether or not to display the full conversion factor table.
#' (Default is FALSE, i.e. only include rows where the conversion factor is 1.)
#' @param ... Other parameters that can be passed to \code{convert_units}, e.g.
#' temperature and compound state.  See details in \code{\link{convert_units}}.
#' 
#' @return 'new.ouput.matrix' A matrix with a column for time (in days), each
#' compartment, and the area under the curve (AUC) and a row
#' for each time point. The compartment and AUC columns are
#' converted from model specified units to user specified units.
#' @return 'output.units.vector' A vector of character strings providing the
#' model compartments and their corresponding units after \code{convert_solve_x}.
#' 
#' @author Sarah E. Davidson
#' 
#' @examples 
#' output.mat <- solve_1comp(dtxsid = "DTXSID0020573")
#' new.output.mat <- convert_solve_x(output.units = "mg",
#'                                   model.output.mat = output.mat,
#'                                   model = "1compartment",
#'                                   dtxsid = "DTXSID0020573")
#' 
#' @seealso convert_units
#' @export convert_solve_x
convert_solve_x <- function(model.output.mat,
                            model=NULL,
                            output.units=NULL,
                            MW = NULL,
                            vol = NULL,
                            chem.cas = NULL,
                            chem.name = NULL,
                            dtxsid = NULL,
                            parameters = NULL,
                            monitor.vars = NULL,
                            suppress.messages=FALSE,
                            verbose = FALSE,
                            ...){
  # default compartment units; includes case 1, i.e. output.units is NULL
  compartment_units <- model.list[[model]]$compartment.units
  # determine the state of matter (liquid or gas) for each compartment:
  compartment_state <- model.list[[model]]$compartment.state
  # If there is only one state set all compartments to that state:
  if (length(names(compartment_state))==1)
  {
    compartment_state <- setNames(rep(names(compartment_state),
                                  length(compartment_units)),
                                  names(compartment_units))
  } else {
    new_compartment_state <- NULL
    # Build up compartment_state one state at a time:
    for (this.state in names(compartment_state))
      new_compartment_state <- c(new_compartment_state,
                                 setNames(rep(this.state,
                                   length(compartment_state[[this.state]])),
                                   compartment_state[[this.state]]))
    compartment_state <- new_compartment_state 
  } 
  
  ou <- compartment_units[which(names(compartment_units)%in%colnames(model.output.mat))]
    
  if(length(output.units)==1 & is.null(names(output.units))){
    # case 2: only one unit is provided with no specified compartment
    if(tolower(output.units)%in%c('um','umol')){
      ou[grepl(names(ou),pattern = "^A") & names(ou)!="AUC"] <- 'umol'
      ou[grepl(names(ou),pattern = "AUC$")] <- 'uM*days'
      ou[grepl(names(ou),pattern = "^C")] <- 'uM'
    }else if(tolower(output.units)%in%c('mg/l','mg')){
      ou[grepl(names(ou),pattern = "^A") & names(ou)!="AUC"] <- 'mg'
      ou[grepl(names(ou),pattern = "AUC$")] <- 'mg/l*days'
      ou[grepl(names(ou),pattern = '^C')] <- 'mg/l'
    }else if(tolower(output.units)=='ppmv'){
      ou[grepl(names(ou),pattern = "^A") & names(ou)!="AUC"] <- 'umol'
      ou[grepl(names(ou),pattern = "AUC$")] <- 'ppmv*days'
      ou[grepl(names(ou),pattern = '^C')] <- 'ppmv'
    }else{
      warning("The provided output.units '",output.units,"' are not currently supported.\n  ",
              "Resulting values are assumed to be in the model default output units per compartment.")
    }
  }else if(length(output.units)>1 & is.null(names(output.units))){
    # case 6: vector of units is provided with no specified compartment 
    warning("There are no names to map provided output.units with corresponding compartments.\n  ",
            "Resulting values are assumed to be in the model default output units per compartment.")
  }else if(!is.null(output.units)){
    # case 3 & 4: all or some of the compartment units are in the model
    # Obtain any output units that ARE IN the default monitor variables to KEEP
    keep.out.vars <- names(output.units)[which((names(output.units)%in%names(ou)))]
    # Obtain any output units that ARE NOT IN the out matrix to TOSS
    non.out.vars <- names(output.units)[which(!(names(output.units)%in%names(ou)))]
    
    if(length(keep.out.vars)>0){
      # re-set the variables specified by user in output.units
      ou[keep.out.vars] <- output.units[keep.out.vars]
      
      #Check if there are any non.out.vars
      if(length(non.out.vars)!=0){
        warning("Additional unnecessary elements were included in the",
                "output.units -- namely ",
                paste(non.out.vars,collapse = ", "),".\n  ",
                "These variables were removed from the output units.")
      }
    }else{
      # case 5: none of the compartment units are in the model
      warning("None of the specified compartments and units are applicable for",
              "the", model," model.\n  ",
              "Resulting values are assumed to be in the model default output ",
              "units per compartment.")
    }
  }
  
  # Set-up the default
  cf <- setNames(rep(1,length(ou)),names(ou)) # conversion factor vector
  out <- model.output.mat # output matrix to return

  # Convert compartments in the model output matrix
  for(this.compartment in names(ou))
  {
    # Check if conversion is needed:
    if (compartment_units[this.compartment] != ou[this.compartment])
    {
      # conversion factor
      cf.tmp <- try(
        convert_units(input.units = compartment_units[this.compartment],
                      output.units = ou[this.compartment],
                      MW = MW,
                      chem.cas = chem.cas,
                      chem.name = chem.name,
                      dtxsid = dtxsid,
                      parameters = parameters,
                      state = compartment_state[this.compartment],
                      ...)
      )

      # check if the conversion factor from 'convert_units' is not a 'try-error'
      if (!is(cf.tmp,"try-error"))
      {
        cf[this.compartment] <- cf.tmp
      } else {
        # print a warning
        warning(this.compartment,
                " was not converted since the specified units '", 
                ou[this.compartment],
                "' are not supported. ",
                "Units are set back to the default model compartment units, (i.e. '",
                compartment_units[this.compartment],
                "'), and the conversion factor is 1.")
        # save missing since unit conversion is not applicable
        cf[this.compartment] <- 1
        ou[this.compartment] <- compartment_units[this.compartment]
      }
      # re-set the values in the compartment column with converted values
      out[,this.compartment] <- model.output.mat[,this.compartment] *
                                cf[this.compartment]
    } else out[,this.compartment] <- model.output.mat[,this.compartment]
  }
  
  # Print a matrix of desired output units and the conversion factors from
  # default solve_X model units specified in the model.info file.
  if(!suppress.messages){
    reporter.df <- cbind.data.frame(output.unit = ou,conversion.factor = cf)
    
    if(!is.null(monitor.vars)){
      # keep only the model components included in the 'monitor.vars'
      reporter.df <- reporter.df[monitor.vars,]
    }
    
    if(!verbose){
      # remove any rows where the conversion factor is 1 (i.e. no unit change)
      reporter.df <- reporter.df[-which(reporter.df[,"conversion.factor"]==1),]
    }
    
    # Check if the reporter matrix/data.frame has any information to report
    # that is nrow is equal to zero.
    if(nrow(reporter.df)==0){
      cat("None of the monitored components undergo unit conversions",
          " (i.e. conversion factor of 1).\n\n")
    }else{
      print(reporter.df)
    }
  }
  # Return the output units vector & new converted output matrix.
  return(list(new.ouput.matrix = out,output.units.vector = ou))
}