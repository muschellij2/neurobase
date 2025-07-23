#' @title Flip NifTI Image
#'
#' @description This image will flip x, y, or z direction
#' @param img nifti object or character filename
#' @param x (logical) Flip x direction
#' @param y (logical) Flip y direction
#' @param z (logical) Flip z direction
#' @param ... Arguments passed to \code{\link{check_nifti}}
#' @export
#' @return Object of class nifti
#' @examples 
#' img = random_nifti(rep(15, 3))
#' flipped = flip_img(img, x = TRUE, y = TRUE, z = TRUE)
#' img = random_nifti(rep(15, 2))
#' flipped = flip_img(img, x = TRUE)
#' testthat::expect_error(flip_img(img, z= TRUE))
flip_img <- function(img, x = FALSE, y = FALSE, z = FALSE, ...){
  img = check_nifti(img, ...)
  d = dim(img)
  ndim = length(d)
  if (ndim == 2) {
    if (z) {
      stop("Cannot flip z- 2D Image!")
    }
    if (x) {
      img = copyNIfTIHeader(img = img, img[nrow(img):1,]) 
    }    
    if (y) {
      img = copyNIfTIHeader(img = img,  img[,ncol(img):1])
    }
  }
  if (ndim == 3) {
    if (x){
      for (i in seq(d[3])){
        x = img[,,i]
        x = x[nrow(x):1,]
        img@.Data[,,i] = x
      }
    }    
    if (y){
      for (i in seq(d[3])){
        x = img[,,i]
        x = x[,ncol(x):1]
        img@.Data[,,i] = x
      }
    }
    if (z){
      for (i in seq(d[1])){
        x = img[i,,]
        x = x[,ncol(x):1]
        img@.Data[i,,] = x
      }
    } 
  }
  return(img)
}
