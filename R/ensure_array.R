#' @title Ensure an array output
#' @description Forces an array output for manipulation and overall conversion
#'
#' @param img Image object (\code{\link{nifti}} or \code{niftiImage})
#'
#' @return Array of same dimensions as image object
#' @export
ensure_array <- function(img) {
  UseMethod("ensure_array")
} 


#' @export
#' @method ensure_array nifti
ensure_array.nifti = function(img){
  return(img_data(img))
}

#' @export
#' @method ensure_array anlz
ensure_array.anlz = function(img){
  img = as.nifti(img)
  return(ensure_array.nifti(img))
}

#' @export
#' @method ensure_array niftiImage
ensure_array.niftiImage = function(img){
  img = array(img, dim = dim(img))  
  img = as.array(img)
  return(img)
}


#' @export
#' @method ensure_array array
ensure_array.array = function(img){
  return(img)
}


#' @export
#' @method ensure_array default
ensure_array.default  = function(img){
  stop("ensure_array not implemented for class yet!")
}


