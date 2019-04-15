honda.ivive <- function(method="Honda1",tissue="liver")
{
  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. To appease R CMD
  #CHECK, a variable has to be created for each of these column names and set to
  #NULL. Note that within the data.table, these variables will not be NULL! Yes,
  #this is pointless and annoying.
  restrictive.clearance<-plasma.binding<-statistic.used<-NULL
  #End R CMD CHECK appeasement.

  if (is.null(tissue)) tissue <- "liver"

  if (tolower(method)==tolower("Honda1"))
  {
    tissue <- NULL
    restrictive.clearance <- T
    plasma.binding <- T
#    tk.statistic.used <<- "mean"
    warning("Argument ivive=\"Honda1\" uses plasma concentration, restrictive clearance, and treats the unbound concentration as bioactive.")
  } else if (tolower(method)==tolower("Honda2"))
  {
    tissue <- NULL
    restrictive.clearance <- T
    plasma.binding <- T
#    tk.statistic.used <<- "max"
    warning("Argument ivive=\"Honda2\" uses plasma concentration, restrictive clearance, and treats the unbound concentration as bioactive.")
  } else if (tolower(method)==tolower("Honda3"))
  {
    tissue <- NULL
    restrictive.clearance <- F
    plasma.binding <- F     
#    tk.statistic.used <<- "mean"
    warning("Argument ivive=\"Honda3\" uses plasma concentration, non-restrictive clearance, and treats the total concentration as bioactive.")
  } else if (tolower(method)==tolower("Honda4"))
  {
    tissue <- NULL
    restrictive.clearance <- F
    plasma.binding <- F
#    tk.statistic.used <<- "max"
    warning("Argument ivive=\"Honda4\" uses plasma concentration, non-restrictive clearance, and treats the total concentration as bioactive.")
  } else if (tolower(method)==tolower("Honda5"))
  {
    tissue <- tolower(tissue)
    restrictive.clearance <- F
    plasma.binding <- F
#    tk.statistic.used <<- "mean"
    warning(paste("Argument ivive=\"Honda5\" uses target tissue (",tissue,") concentration, non-restrictive clearance, and treats the total concentration as bioactive.",sep=""))
  } else if (tolower(method)==tolower("Honda6"))
  {
    tissue <- tolower(tissue)
    restrictive.clearance <- F
    plasma.binding <- F
#    tk.statistic.used <<- "max"
    warning(paste("Argument ivive=\"Honda6\" uses target tissue (",tissue,") concentration, non-restrictive clearance, and treats the total concentration as bioactive.",sep=""))
  } else stop("There were only six sets of IVIVE assunptions that performed well in Honda et al. (2019): \"Honda1\" through \"Honda6\". See Supplemental Table X in that paper. Honda1 and Honda4 are omitted because the use max (peak) concentration which can not currently calculate with calc_analytic_css.")

  return(list(tissue = tissue,
              plasma.binding = plasma.binding,
              restrictive.clearance=restrictive.clearance))
}