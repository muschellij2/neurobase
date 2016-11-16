#' @title Apply Subsetting from Empty Image Dimensions
#' @name applyEmptyImageDimensions
#' @param img nifti object
#' @param inds indices of subset from \code{\link{getEmptyImageDimensions}} or
#' \code{\link{dropEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @description Simple wrapper for subsetting an image with indices, 
#' dropping empty dimensions.
#' @return Object of class \code{nifti} or \code{array} if \code{nifti}
#' is not supplied
#' @note \code{apply_empty_dim} is a shorthand for 
#' \code{applyEmptyImageDimensions} with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}, 
#' \code{\link{dropEmptyImageDimensions}} 
#' @export
applyEmptyImageDimensions <- function(img, 
                                      inds,
                                      reorient = FALSE) {
  
  img = check_nifti(img, reorient = reorient, allow.array = TRUE)
  dimg = dim(img)
  if (length(dimg) > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by length(dim(img))"))
  }
  i2 = img[inds[[1]], inds[[2]], inds[[3]]]
  if (is.nifti(img)) {
    outimg = copyNIfTIHeader(img = img, arr = i2, drop = TRUE)  
    return(outimg)
  } else {
    return(i2)
  }
}

#' @rdname applyEmptyImageDimensions
#' @export
apply_empty_dim <- function(img, 
                            inds,
                            reorient = FALSE) {
  applyEmptyImageDimensions(img = img, 
                           inds = inds,
                           reorient = reorient)
}
