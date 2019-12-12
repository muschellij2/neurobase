#' @title Get Empty Image Dimensions
#' @name getEmptyImageDimensions
#' @param img nifti object or array
#' @param value Value to check against.  If zero, then 
#' \code{getEmptyImageDimensions} will include any dimension that has 
#' fewer than \code{threshold} zeroes. May be a vector of values, matched with 
#' \code{\link{match}}
#' @param threshold Include dimension if fewer than \code{threshold} voxels
#' are in the slice
#' @param reorient Should image be reoriented if a filename?
#' @param ... additional arguments to pass to 
#' \code{\link{check_nifti}}
#' @description Creates a list of indices of an image that has all irrelevant
#' values
#' @return List of length 3 of indices.
#' @note \code{get_empty_dim} is a shorthand for \code{getEmptyImageDimensions}
#' with all the same arguments.  Also, \code{NA} are set to zero.
#' @export
getEmptyImageDimensions <- function(img, 
                                    value = 0, 
                                    threshold = 0,
                                    reorient = FALSE, 
                                    ...) {
  
  img = check_nifti(img, 
                    reorient = reorient, 
                    allow.array = TRUE,
                    need_header = FALSE,
                    ...)
  dimg = dim(img)
  ndim = length(dimg)
  if (ndim > 3) {
    stop(paste0("Only images with 3 dimensions supported, ", 
                "as checked by length(dim(img))"))
  }
  ############################
  # Set NAs to 0
  ############################
  img = as.array(img)
  img[is.na(img)] = 0
  # img = niftiarr(img, arr)
  
  
  ############################
  # Get indices for slices with all zeros (or of certain value)
  ############################
  inds = vector(mode = "list", length = ndim)
  bin_img = array(!(img %in% value), dim = dim(img))
  for (i in 1:ndim) {
    zero_x = apply(bin_img, i, sum)
    # zero_x = apply(img, i, function(x){
    # sum(!(x %in% value))
    # })
    # will only drop the ends of the slices
    # cumsum is used so that if any passes threshold, then won't be dropped 
    # after.  Rev is for the other side
    dzero_x = !(
      cumsum(zero_x) <= threshold | 
        rev( cumsum(rev(zero_x)) <= threshold )
    )
    inds[[i]] = which(dzero_x)
  }
  return(inds)
}

#' @rdname getEmptyImageDimensions
#' @export
get_empty_dim <- getEmptyImageDimensions
