#' Convenience Boolean (yes/no) functions to identify chemical membership in
#' several key lists.
#' 
#' These functions allow easy identification of whether or not a chemical CAS
#' is included in various research projects. While it is our intent to keep
#' these lists up-to-date, the information here is only for convenience and
#' should not be considered to be definitive.
#' 
#' Tox21: Toxicology in the 21st Century (Tox21) is a U.S. federal High
#' Throughput Screening (HTS) collaboration among EPA, NIH, including National
#' Center for Advancing Translational Sciences and the National Toxicology
#' Program at the National Institute of Environmental Health Sciences, and the
#' Food and Drug Administration.  (Bucher et al., 2008)
#' 
#' ToxCast: The Toxicity Forecaster (ToxCast) is a HTS screening project led by
#' the U.S. EPA to perform additional testing of a subset of Tox21 chemicals.
#' (Judson et al. 2010)
#' 
#' ExpoCast: ExpoCast (Exposure Forecaster) is an U.S. EPA research project to
#' generate tenetative exposure estimates (e.g., mg/kg BW/day) for thousands of
#' chemicals that have little other information using models and informatics.
#' (Wambaugh et al. 2014)
#' 
#' NHANES: The U.S. Centers for Disease Control (CDC) National Health and
#' Nutrition Examination Survery (NHANES) is an on-going survey to characterize
#' the health and biometrics (e.g., weight, height) of the U.S. population. One
#' set of measurments includes the quantification of xenobiotic chemicals in
#' various samples (blood, serum, urine) of the thousands of surveyed
#' individuals. (CDC, 2014)
#' 
#' @aliases in.list is.nhanes is.nhanes.serum.parent is.nhanes.serum.analyte
#' is.nhanes.blood.parent is.nhanes.blood.analyte is.nhanes.urine.parent
#' is.nhanes.urine.analyte is.tox21 is.toxcast is.expocast is.pharma
#' @seealso \code{\link{is.httk}} for determining inclusion in httk project
#' @param chem.cas The Chemical Abstracts Service Resgistry Number (CAS-RN)
#' corresponding to the chemical of interest.
#' @param which.list A character string that can take the following values:
#' "ToxCast", "Tox21", "ExpoCast", "NHANES", ""NHANES.serum.parent",
#' "NHANES.serum.analyte","NHANES.blood.parent","NHANES.blood.analyte",
#' "NHANES.urine.parent","NHANES.urine.analyte"
#' @return \item{logical}{A Boolean (1/0) value that is TRUE if the chemical is
#' in the list.}
#' @author John Wambaugh
#' @references Bucher, J. R. (2008). Guest Editorial: NTP: New Initiatives, New
#' Alignment. Environ Health Perspect 116(1).
#' 
#' Judson, R. S., Houck, K. A., Kavlock, R. J., Knudsen, T. B., Martin, M. T.,
#' Mortensen, H. M., Reif, D. M., Rotroff, D. M., Shah, I., Richard, A. M. and
#' Dix, D. J. (2010). In Vitro Screening of Environmental Chemicals for
#' Targeted Testing Prioritization: The ToxCast Project. Environmental Health
#' Perspectives 118(4), 485-492.
#' 
#' Wambaugh, J. F., Wang, A., Dionisio, K. L., Frame, A., Egeghy, P., Judson,
#' R. and Setzer, R. W. (2014). High Throughput Heuristics for Prioritizing
#' Human Exposure to Environmental Chemicals. Environmental Science &
#' Technology, 10.1021/es503583j.
#' 
#' CDC (2014). National Health and Nutrition Examination Survey. Available at:
#' https://www.cdc.gov/nchs/nhanes.htm.
#' @examples
#' 
#' \donttest{
#' httk.table <- get_cheminfo(info=c("CAS","Compound"))
#' httk.table[,"Rat"] <- ""
#' httk.table[,"NHANES"] <- ""
#' httk.table[,"Tox21"] <- ""
#' httk.table[,"ToxCast"] <- ""
#' httk.table[,"ExpoCast"] <- ""
#' httk.table[,"PBTK"] <- ""
#' # To make this example run quickly, this loop is only over the first ten 
#' # chemicals. To build a table with all available chemicals use:
#' # for (this.cas in httk.table$CAS)
#' for (this.cas in httk.table$CAS[1:10])
#' {
#'   this.index <- httk.table$CAS==this.cas
#'   if (is.nhanes(this.cas)) httk.table[this.index,"NHANES"] <- "Y"
#'   if (is.tox21(this.cas)) httk.table[this.index,"Tox21"] <- "Y"
#'   if (is.toxcast(this.cas)) httk.table[this.index,"ToxCast"] <- "Y"
#'   if (is.expocast(this.cas)) httk.table[this.index,"ExpoCast"] <- "Y"
#'   if (is.httk(this.cas,model="PBTK")) httk.table[this.index,"PBTK"] <- "Y"
#'   if (is.httk(this.cas,species="rat")) httk.table[this.index,"Rat"] <- "Y"
#' }
#' }
#' 
#' @export in.list
#' @export is.nhanes.serum.parent
#' @export is.nhanes.serum.analyte
#' @export is.nhanes.blood.parent
#' @export is.nhanes.blood.analyte
#' @export is.nhanes.urine.parent
#' @export is.nhanes.urine.analyte
#' @export is.tox21
#' @export is.toxcast
#' @export is.expocast
#' @export is.nhanes
#' @export is.pharma
in.list <- function(chem.cas=NULL,
                    which.list="ToxCast")
{
  chem.lists <- chem.lists
  if (!(tolower(which.list) %in% tolower(names(chem.lists))))
    stop(paste("List",which.list,"not in available lists:",
    paste(names(chem.lists),collapse=", ")))

  good.cas <- sapply(chem.cas,function(x) suppressWarnings(CAS.checksum(x)))
  
  if (!all(good.cas)) stop (paste("Chemical CAS failed checksum:",
    chem.cas[!good.cas]))
  
  return(chem.cas %in% chem.lists[tolower(names(chem.lists))==tolower(which.list)][[1]][,"CAS"])
}

is.nhanes.serum.parent <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="NHANES.serum.parent"))
is.nhanes.serum.analyte <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="NHANES.serum.analyte"))
is.nhanes.blood.parent <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="NHANES.blood.parent"))
is.nhanes.blood.analyte <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="NHANES.blood.analyte"))
is.nhanes.urine.parent <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="NHANES.urine.parent"))
is.nhanes.urine.analyte <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="NHANES.urine.analyte"))
is.tox21 <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="Tox21"))
is.toxcast <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="ToxCast"))
is.expocast <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="ExpoCast"))
is.nhanes <- function(chem.cas) return(in.list(chem.cas=chem.cas,which.list="NHANES"))
is.pharma <- function (chem.cas) return(in.list(chem.cas = chem.cas, which.list = "Pharma"))




#' Convenience Boolean (yes/no) function to identify chemical membership and
#' treatment within the httk project.
#' 
#' Allows easy identification of whether or not a chemical CAS is included in
#' various aspects of the httk research project (by model type and species of
#' interest). While it is our intent to keep these lists up-to-date, the 
#' information here is only for convenience and should not be considered
#' definitive. 
#' 
#' Tox21: Toxicology in the 21st Century (Tox21) is a U.S. federal High
#' Throughput Screening (HTS) collaboration among EPA, NIH, including National
#' Center for Advancing Translational Sciences and the National Toxicology
#' Program at the National Institute of Environmental Health Sciences, and the
#' Food and Drug Administration.  (Bucher et al., 2008)
#' 
#' ToxCast: The Toxicity Forecaster (ToxCast) is a HTS screening project led by
#' the U.S. EPA to perform additional testing of a subset of Tox21 chemicals.
#' (Judson et al. 2010)
#' 
#' ExpoCast: ExpoCast (Exposure Forecaster) is an U.S. EPA research project to
#' generate tenetative exposure estimates (e.g., mg/kg BW/day) for thousands of
#' chemicals that have little other information using models and informatics.
#' (Wambaugh et al. 2014)
#' 
#' NHANES: The U.S. Centers for Disease Control (CDC) National Health and
#' Nutrition Examination Survery (NHANES) is an on-going survey to characterize
#' the health and biometrics (e.g., weight, height) of the U.S. population. One
#' set of measurments includes the quantification of xenobiotic chemicals in
#' various samples (blood, serum, urine) of the thousands of surveyed
#' individuals. (CDC, 2014)
#' 
#' @seealso \code{\link{in.list}} for determining chemical membership in 
#' several other key lists
#' @param chem.cas The Chemical Abstracts Service Resgistry Number (CAS-RN)
#' corresponding to the chemical of interest.
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' @param model Model used in calculation, 'pbtk' for the multiple compartment
#' model, '1compartment' for the one compartment model, '3compartment' for
#' three compartment model, '3compartmentss' for the three compartment model
#' without partition coefficients, or 'schmitt' for chemicals with logP and
#' fraction unbound (used in predict_partitioning_schmitt).
#' @return \item{logical}{A Boolean (1/0) value that is TRUE if the chemical
#' is included in the httk project with a given modeling scheme (PBTK) and 
#' a given species}
#' @author John Wambaugh
#' @references Bucher, J. R. (2008). Guest Editorial: NTP: New Initiatives, New
#' Alignment. Environ Health Perspect 116(1).
#' 
#' Judson, R. S., Houck, K. A., Kavlock, R. J., Knudsen, T. B., Martin, M. T.,
#' Mortensen, H. M., Reif, D. M., Rotroff, D. M., Shah, I., Richard, A. M. and
#' Dix, D. J. (2010). In Vitro Screening of Environmental Chemicals for
#' Targeted Testing Prioritization: The ToxCast Project. Environmental Health
#' Perspectives 118(4), 485-492.
#' 
#' Wambaugh, J. F., Wang, A., Dionisio, K. L., Frame, A., Egeghy, P., Judson,
#' R. and Setzer, R. W. (2014). High Throughput Heuristics for Prioritizing
#' Human Exposure to Environmental Chemicals. Environmental Science &
#' Technology, 10.1021/es503583j.
#' 
#' CDC (2014). National Health and Nutrition Examination Survey. Available at:
#' https://www.cdc.gov/nchs/nhanes.htm.
#' @examples
#' 
#' \donttest{
#' httk.table <- get_cheminfo(info=c("CAS","Compound"))
#' httk.table[,"Rat"] <- ""
#' httk.table[,"NHANES"] <- ""
#' httk.table[,"Tox21"] <- ""
#' httk.table[,"ToxCast"] <- ""
#' httk.table[,"ExpoCast"] <- ""
#' httk.table[,"PBTK"] <- ""
#' # To make this example run quickly, this loop is only over the first ten 
#' # chemicals. To build a table with all available chemicals use:
#' # for (this.cas in httk.table$CAS)
#' for (this.cas in httk.table$CAS[1:10])
#' {
#'   this.index <- httk.table$CAS==this.cas
#'   if (is.nhanes(this.cas)) httk.table[this.index,"NHANES"] <- "Y"
#'   if (is.tox21(this.cas)) httk.table[this.index,"Tox21"] <- "Y"
#'   if (is.toxcast(this.cas)) httk.table[this.index,"ToxCast"] <- "Y"
#'   if (is.expocast(this.cas)) httk.table[this.index,"ExpoCast"] <- "Y"
#'   if (is.httk(this.cas,model="PBTK")) httk.table[this.index,"PBTK"] <- "Y"
#'   if (is.httk(this.cas,species="rat")) httk.table[this.index,"Rat"] <- "Y"
#' }
#' }
#'
#' @export is.httk
is.httk <- function(chem.cas,species="Human", model = "3compartmentss") return(chem.cas %in% get_cheminfo(species=species,model = model))
