
#' @title Image Center of Gravity
#' @description Find Center of Gravity of Image, after thresholding
#' @param img Object of class nifti
#' @param thresh threshold for image, will find \code{img > 0}
#' @param ceil Run \code{\link{ceiling}} to force integers (usu for plotting)
#' @param warn Produce a warning if the image is empty after thresholding
#' @return Vector of length 3
#' @export
#' @examples
#' dims = rep(20, 3)
#' x = array(rnorm(prod(dims)), dim = dims)
#' img = nifti(x, dim= dims, 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' cog(img)
cog = function(img, thresh = 0, ceil = FALSE, warn = TRUE){
  #   stopifnot(inherits(img, "nifti"))
  mask = img > thresh
  if (sum(mask, na.rm = TRUE) == 0) {
    if (warn) {
      warning(paste0("No voxels found to be > ", round(thresh, 3), 
                     ", using the center of whole image"))
    }
    xyz = dim(mask) / 2
  } else {
    xyz = colMeans(which(mask, arr.ind = TRUE))
  }
  if (ceil) xyz = ceiling(xyz)
  return(xyz)
}



#' @title Image Center of Gravity Wrapper
#' @description Find Center of Gravity of Image, after thresholding and
#' take ceiling (wrapper for \code{\link{cog}})
#' @param ... Arguments ppssed to \code{\link{cog}}
#' @return Vector of length 3
#' @note Just a convenience wrapper for \code{cog(ceil=TRUE)}
#' @export
xyz = function(...){
  xyz = cog(..., ceil = TRUE)
  return(xyz)
}
