
#' @title Get Z-score over a margin of an img
#' @import oro.nifti
#' @description Standardizes an image either by the axial, sagittal, or
#' coronal slice or whole image
#' @importFrom matrixStats colSds
#' @return Array of object of class nifti
#' @seealso \link{aperm}
#' @param img character path of image or 
#' an object of class nifti
#' @param mask character path of mask or 
#' an object of class nifti 
#' @param margin Margin of image to z-score over (\code{NULL} - whole brain,
#' 3-Axial, 2-Sagittal, 
#' 1-Coronal)
#' @param centrality (character) Measure to center the data, 
#' either mean or median
#' @param variability (character) Measure to scale the data
#' @param remove.na (logical) change NAs to remove.val
#' @param remove.nan (logical) change NaN to remove.val
#' @param remove.inf (logical) change Inf to remove.val
#' @param remove.val (logical) value to put the NA/NaN/Inf
#' @param trim if centrality is \code{trimmed_mean} or variability is 
#' \code{trimmed_sd}, then the amount of trimming
#' @param remask (logical) Should the image be remasked after normalizing?
#' @export
#' @importFrom matrixStats colMedians 
#' @importFrom matrixStats colSds 
#' @importFrom matrixStats colIQRDiffs colIQRs iqrDiff iqr
#' @importFrom matrixStats colMadDiffs colMads madDiff
#' @import stats
#' @examples
#' dim = c(100, 30, 5)
#' img = array(rnorm(prod(dim), mean=4, sd=4), 
#' dim=dim)
#' 
#' truth2 = img
#' for (i in 1:dim(img)[2]) {
#' truth2[,i,] = (truth2[,i,]- mean(truth2[,i,]))/sd(truth2[,i,])
#' }
#' 
#' truth1 = img
#' for (i in 1:dim(img)[1]) {
#' truth1[i,,] = (truth1[i,,]- mean(truth1[i,,]))/sd(truth1[i,,])
#' }
#' 
#' truth3 = img
#' for (i in 1:dim(img)[3]) {
#' truth3[,,i] = (truth3[,,i]- mean(truth3[,,i]))/sd(truth3[,,i])
#' }
#' try3 = zscore_img(img, margin=3)
#' stopifnot(all.equal(try3, truth3))
#' try2 = zscore_img(img, margin=2)
#' stopifnot(all.equal(try2, truth2))
#' try1 = zscore_img(img, margin=1)
#' stopifnot(all.equal(try1, truth1))
#'   
#' z = zscore_img(img, margin=NULL)
#' ztrim = zscore_img(img, margin=NULL, 
#' centrality = "trimmed_mean", variability = "trimmed_sd")
#' 
#' 
zscore_img <- function(img, mask = NULL, margin= NULL, 
                       centrality = c("mean", "median", "trimmed_mean"),
                       variability = c("sd", "iqrdiff", "mad", 
                                       "maddiff", "iqr", "trimmed_sd"),
                       trim = 0.2,
                       remove.na = TRUE,
                       remove.nan = TRUE, 
                       remove.inf = TRUE,
                       remove.val = 0,
                       remask = TRUE){
  centrality = match.arg(centrality, c("mean", "median", 
                                       "trimmed_mean"))
  variability = match.arg(variability, c("sd", "iqrdiff", "mad", 
                                         "maddiff", "iqr", "trimmed_sd"))
  img = check_nifti(img, allow.array=TRUE)
  orig.img = img
  dimg = dim(orig.img)
  if (is.null(mask)){
    mask = array(1, dim = dimg)  
  } else {
    mask = check_nifti(mask)
  }
  check_mask_fail(mask)
  mask = check_nifti(mask, allow.array=TRUE)
  img[mask == 0] = NA
  
  stopifnot(length(dimg) == 3)  
  if (!is.null(margin)){
    if (centrality %in% "trimmed_mean" || 
        variability %in% "trimmed_sd") {
      stop("trimming not implemented when margin doesn't equal NULL")
    }
    if (margin == 3){
      perm = 1:3
    }
    if (margin == 2){
      perm = c(1, 3, 2)
    }  
    if (margin == 1){
      perm = c(2, 3, 1)
    }
    revperm = match(1:3, perm)
    
    img = aperm(img, perm)
    vec = matrix(img, ncol = dimg[margin])
    
    img = aperm(orig.img, perm)
    orig.vec = matrix(img, ncol = dimg[margin])

    if (centrality == "mean") {
      m = colMeans(vec, na.rm = TRUE)
    }
    if (centrality == "median") {
      m = colMedians(vec, na.rm = TRUE)
    } 
    if (variability == "iqrdiff") {
      s = colIQRDiffs(vec, na.rm = TRUE)
    }
    if (variability == "maddiff") {
      s = colMadDiffs(vec, na.rm = TRUE)
    }   
    if (variability == "mad") {
      s = colMads(vec, na.rm = TRUE)
    }       
    if (variability == "iqr") {
      s = colIQRs(vec, na.rm = TRUE)
    }       
    if (variability == "sd") {
      s = colSds(vec, na.rm = TRUE)
    }     
    
    vecc = (t(orig.vec) - m)/s
    vecc = t(vecc)
    imgc = array(vecc, 
                 dim = dim(img))
    imgc = aperm(imgc, revperm)
  } else {
    if (centrality %in% "trimmed_mean" || 
        variability %in% "trimmed_sd") {
      trim_vals = c(img)
      qtrim <- quantile(trim_vals,
                        c(trim, 0.5, 1 - trim),
                        na.rm = TRUE)
      xbot <- qtrim[1]
      xtop <- qtrim[3]
      trim_vals[trim_vals < xbot] <- NA
      trim_vals[trim_vals > xtop] <- NA
      
      mn = mean(trim_vals, na.rm = TRUE)
      s = sd(trim_vals, na.rm = TRUE)
      
    }
    
    if (!(centrality %in% "trimmed_mean")) {
      mn = do.call(centrality, list(x = c(img), na.rm = TRUE))
    }
    if (variability == "iqrdiff") {
      s = iqrDiff(c(img), na.rm = TRUE)
    }
    if (variability == "sd") {
      s = sd(c(img), na.rm = TRUE)
    }
    if (variability == "maddiff") {
      s = madDiff(c(img), na.rm = TRUE)
    }   
    if (variability == "mad") {
      s = mad(c(img), na.rm = TRUE)
    }       
    if (variability == "iqr") {
      s = iqr(c(img), na.rm = TRUE)
    }       
    
    imgc = (orig.img - mn) / s
  }
  stopifnot(all.equal(dim(imgc), dim(orig.img)))
  if (inherits(orig.img, "nifti")) {
    imgc = niftiarr(img = orig.img, imgc)
    imgc = datatyper(imgc, 
                     datatype = convert.datatype()$FLOAT32, 
                     bitpix = convert.bitpix()$FLOAT32) 
  }
  if (remask) {
    imgc[mask == 0] = NA
  }  
  if (remove.na) {
    imgc[is.na(imgc)] = remove.val
  }
  if (remove.nan) {
    imgc[is.nan(imgc)] = remove.val
  } 
  if (remove.inf) {
    imgc[is.infinite(imgc)] = remove.val
  } 

  if (inherits(orig.img, "nifti")) {
    imgc = cal_img(imgc)
    imgc = zero_trans(imgc)
  }
  imgc
  
}
