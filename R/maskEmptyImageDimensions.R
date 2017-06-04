#' @title Replace Empty Image Dimensions with Mask Values
#' @name maskEmptyImageDimensions
#' @param img nifti object
#' @param ... Arguments to be passed to \code{\link{getEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @param mask.value Value to replace voxels outside the mask.
#' @description Replaces values from dropped dimensions with a \code{mask.va.ue}
#' @return Object of class \code{nifti} with the values of the 
#' original image in the image and \code{mask.value} outside the mask
#' @note \code{mask_empty_dim} is a shorthand for \code{maskEmptyImageDimensions}
#' with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}  
#' @export
maskEmptyImageDimensions <- function(img, 
                                     ...,
                                     reorient = FALSE,
                                     mask.value = 0) {
  mask = emptyImageDimensionsMask(img = img, ..., reorient = reorient)
  img[ mask == 1 ] = mask.value
  
  return(img)
}

#' @rdname maskEmptyImageDimensions
#' @export
mask_empty_dim <- function(img, 
                           ...,
                           reorient = FALSE,
                           mask.value = 0) {
  maskEmptyImageDimensions(img = img, 
                           ... = ...,
                           reorient = reorient, 
                           mask.value = mask.value)
}
