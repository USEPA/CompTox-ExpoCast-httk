#' List all available HTTK models
#' 
#' @return Prints a list of available HTTK models to the screen.
#' 
#' @author John Wambaugh

#' 
#' @export list_models
list_models <- function()
{
  models.text <- strwrap(paste("Available models for httk are: \"",
            paste(names(model.list), collapse = "\", \""),
            "\".\n",sep=""))
  writeLines(models.text)
}
