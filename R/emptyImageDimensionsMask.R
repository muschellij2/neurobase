#' @title Make Mask from Empty Image Dimensions
#' @name emptyImageDimensionsMask
#' @param img nifti object
#' @param ... Arguments to be passed to \code{\link{getEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @description Make a mask of an image that has all irrelevant
#' values
#' @return Object of class \code{nifti}, with binary values
#' @note \code{empty_dim_mask} is a shorthand for \code{emptyImageDimensionsMask}
#' with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}  
#' @export
emptyImageDimensionsMask <- function(img, 
                                     ...,
                                     reorient = FALSE) {
  
  img = check_nifti(img, 
                    reorient = reorient, 
                    allow.array = TRUE,
                    need_header = FALSE)
  inds = getEmptyImageDimensions(img = img,
                                 reorient = reorient,
                                 ...)
  
  mask = array(FALSE, dim = dim(img))
  mask[inds[[1]], inds[[2]], inds[[3]]] = TRUE
  if (is.nifti(img)) {
    mask = copyNIfTIHeader(img = img, arr = mask)
    mask@datatype <- convert.datatype()[["UINT8"]]
    mask@bitpix <- convert.bitpix()[["UINT8"]]      
  }
  
  return(mask)
}

#' @rdname emptyImageDimensionsMask
#' @export
empty_dim_mask <- function(img, 
                           ...,
                           reorient = FALSE) {
  maskEmptyImageDimensions(img = img, 
                           ... = ...,
                           reorient = reorient
                           )
}
