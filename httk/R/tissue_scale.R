#'Allometric scaling.
#'
#'Allometrically scale a tissue mass or flow based on height^(3/4).
#'
#'@param height_ref Reference height in cm.
#'@param height_indiv Individual height in cm.
#'@param tissue_mean_ref Reference tissue mass or flow.
#'  
#'@return Allometrically scaled tissue mass or flow, in the same units as
#'  \code{tissue_mean_ref}.
#'
#'@keywords httk-pop
#'
#'@author Caroline Ring
#'
#'@references 
#' \insertRef{ring2017identifying}{httk}

tissue_scale <- function(height_ref, height_indiv, tissue_mean_ref){
  
  tissue.mean.indiv <- ((height_indiv/height_ref)^(3/4))*
    tissue_mean_ref
  return(tissue.mean.indiv)
}
  
