#' List all available HTTK models
#' 
#' @return Describes (or lists) available HTTK models
#' 
#' @author John Wambaugh
#' 
#' @param names.only If true, only return the model names
#' 
#' @export list_models
list_models <- function(names.only=FALSE)
{
  if (names.only)
  {
     models.text <- strwrap(paste("Available models for httk are: \"",
            paste(names(model.list), collapse = "\", \""),
            "\".\n",sep=""))
    writeLines(models.text)
    return(models.text)
  } else {
    table.out <- data.frame(Model=names(model.list),
                               Description=unlist(lapply(model.list,
                                                  function (x)
                                                    ifelse(!is.null(x$Description),
                                                        x$Description,
                                                        ""))),
                               Reference=unlist(lapply(model.list,
                                                  function (x)
                                                    ifelse(!is.null(x$Reference),
                                                        x$Reference,
                                                        ""))),
                               Parameters=unlist(lapply(model.list,
                                                  function (x)
                                                    ifelse(!is.null(x$parameterize.func),
                                                        x$parameterize.func,
                                                        ""))),
                               Excluded.Class=unlist(lapply(model.list,
                                                  function (x)
                                                    ifelse(!is.null(x$chem.class.filt),
                                                        x$chem.class.filt,
                                                        "None"))),                         
                               Henrys.Law.Filter=unlist(lapply(model.list,
                                                  function (x)
                                                    ifelse(!is.null(x$log.henry.threshold),
                                                        x$log.henry.threshold,
                                                        "None"))),
                               DOI=unlist(lapply(model.list,
                                                  function (x)
                                                    ifelse(!is.null(x$DOI),
                                                        x$DOI,
                                                        "")))
                               )
    table.out <- table.out[order(table.out$Model),]                        
    return(table.out)
  }
}
