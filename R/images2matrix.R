#' @title Transform set of images to matrix
#' @description Creates a matrix, where the voxels are on the rows 
#' and images are on the columns
#'
#' @param imgs Vector of files or list of images (niftiImage, array, or nifti)
#' @param mask Binary image to subset the voxels
#'
#' @return Matrix of V by p, where V is the product of the dimensions of one 
#' image or the number of voxels in the mask, and p is the number of images
#' @export
images2matrix <- function(
  imgs, 
  mask = NULL){
  # Do mask first because if fails - want it to fail early
  if (!is.null(mask)) {
    mask = img_data(mask)
    check_mask_fail(mask, allow.array = TRUE)
  }    
  # Get the arrays
  imgs <- lapply(imgs, img_data)
  # check dimensions
  if (!same_dims(imgs)) {
    stop("Not all images have the same dimensions!")
  }
  if (!is.null(mask)) {
    if (!same_dims(imgs[[1]], mask)) {
      stop("Mask is not the same dimensions as the images!")
    }
  }
  if (!is.null(mask)) {
    mask = which(mask %in% 1)
    imgs = lapply(imgs, function(x){
      x[mask]
    })
  }  
  imgs <- do.call(cbind, imgs)
  return(imgs)
}