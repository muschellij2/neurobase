#' @title Ensure an array output
#' @description Forces an array output for manipulation and overall conversion
#'
#' @param img Image object (\code{\link{nifti}} or \code{niftiImage})
#'
#' @return Array of same dimensions as image object
#' @export
#' @examples 
#' set.seed(5)
#' dims = rep(10, 3)
#' arr = array(rnorm(prod(dims)), dim = dims)
#' arr[,,10] = 0
#' nim = oro.nifti::nifti(arr)
#' rnifti = RNifti::asNifti(nim)
#' timg = tempimg(nim)
#' limg = list(factor(timg), factor(timg))
#' func = ensure_array
#' func(arr)
#' func(nim)
#' func(rnifti)
#' func(timg)
#' func(limg[[1]])
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
#' @method ensure_array character
ensure_array.character = function(img){
  stopifnot(length(img) == 1)
  img = readnii(img)
  img = ensure_array(img)
  return(img)
}

#' @export
#' @method ensure_array factor
ensure_array.factor = function(img){
  stopifnot(length(img) == 1)
  img = as.character(img)
  img = ensure_array(img)
  return(img)
}


#' @export
#' @method ensure_array default
ensure_array.default  = function(img){
  stop("ensure_array not implemented for class yet!")
}


